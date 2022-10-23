import { basicSetup } from "./setup"
import { vim } from "@replit/codemirror-vim"
import {
    EditorView, WidgetType,
    Decoration, DecorationSet,
    ViewUpdate, ViewPlugin
} from "@codemirror/view"
import {
    EditorState, StateField,
    StateEffect, RangeSet,
    Compartment
} from "@codemirror/state"
import { rust } from "@codemirror/lang-rust"
import { indentUnit } from "@codemirror/language"
import * from "./types"

const initial_code: string =
    `// Please start typing :)

#[derive(Debug, Default)]
struct Box {
    value: i32,
}

impl Box {
    fn inc(&mut self) {
        self.value += 1;
    }

    fn destroy(mut self) {}
}

fn main() {

    let v1 = vec![1, 2, 3];
    v1.push(0);

    let v2 = &mut vec![1, 2, 3];
    v2.push(0);

    let b1 = &Box::default();
    b1.inc();

    let mut b2 = Box::default();
    b2.inc();

    Box::default().destroy();

    println!("Gruëzi, Weltli");
}
`;

let readOnly = new Compartment

export class Editor {
    private view: EditorView;

    public constructor(dom: HTMLElement) {
        let initial_state = EditorState.create({
            doc: initial_code,
            extensions: [
                vim(),
                basicSetup,
                rust(),
                readOnly.of(EditorState.readOnly.of(false)),
                indentUnit.of("    "),
                methodCallPoints
            ],
        });

        let initial_view = new EditorView({
            state: initial_state,
            parent: dom,
        });

        this.view = initial_view;
    }

    public get_current_contents(): string {
        return this.view.state.doc.toString();
    }

    public toggle_readonly(b: boolean): void {
        this.view.dispatch({
            effects: [readOnly.reconfigure(EditorState.readOnly.of(b))]
        });
    }

    // Ideally we could just allow the receiver marks to stay until
    // the code changes in the editor. TODO
    public remove_receiver_types(): void {
        this.view.dispatch({
            effects: [ setMethodCallPoints.of([]) ]
        });
    }

    // XXX this method gets called when the receiver types are received from
    // the backend, however, on `update` (e.g. someone is typing code) we
    // probably want them to go away.
    public show_receiver_types(method_call_points: ReceiverTypes): void {
        this.view.dispatch({
            effects: [
                // Process the set of visual marks that need to be included.
                setMethodCallPoints.of(
                    method_call_points.map((call_type: CallTypes) => {
                        // FIXME I would like a more flexible way to represent
                        // actual vs expected instead of explicitly using colors
                        // in the Icon type. Ideally, I could have some modifier
                        // which will change the types, I may not have enough knowledge
                        // about the DOM to create that right now.
                        //
                        // Regardless this is pretty ugly.
                        let high_color = `rgba(112,128,144,1)`;
                        let low_color = `rgba(233,236,238,1)`;
                        let color = (b) => (b ? high_color : low_color);

                        let ts_actual: TypeState = call_type.actual.of_type;
                        let ts_expected: TypeState = call_type.expected.of_type;

                        let [a_o, a_m] =
                            (("Owned" in ts_actual) ?
                                [true, ts_actual.Owned.mutably_bound] :
                                [false, ts_actual.Ref.is_mut]);

                        let [e_o, e_m] =
                            (("Owned" in ts_expected) ?
                                [true, ts_expected.Owned.mutably_bound] :
                                [false, ts_expected.Ref.is_mut]);

                        let owned_ico: Icon = {
                            class_name: "fa-square",
                            color_expected: color(e_o),
                            color_actual: color(a_o)
                        };

                        let mut_ico: Icon = {
                            class_name: "fa-circle",
                            color_expected: color(e_m),
                            color_actual: color(a_m)
                        };

                        // HACK the ending character of the actual type
                        // might not actually be right before the dereference
                        // operator `.`, do some testing and then we can probably
                        // use the character preceding the expected `char_start`.
                        let loc = call_type.expected.range.char_start - 1;
                        return [owned_ico, mut_ico, loc];
                    }))]
        });
    }
}

// Example Widget from the codemirror 6 api doc.
class CallTypesWidget extends WidgetType {
    constructor(readonly owner: Icon, readonly mut: Icon) { super() }

    eq(other: CallTypesWidget) {
        return other.owner == this.owner && other.mut == this.mut;
    }

    toDOM() {
        let gen_ico = (name, color1, color2) => {
            let make_name_at_size = (ico_name) => (n: number) => `fa ${ico_name} fa-stack-${n}x`;
            // Create the DOM element for Ownership
            let wrap = document.createElement("span");
            wrap.className = "fa-stack small";
            let box_o = wrap.appendChild(document.createElement("i"));
            let box_i = wrap.appendChild(document.createElement("i"));
            let make_style = make_name_at_size(name);
            box_o.className = make_style(2);
            box_i.className = make_style(1);
            box_o.setAttribute("style", `color: ${color1};`)
            box_i.setAttribute("style", `color: ${color2};`)
            return wrap;
        };

        // Main DOM span element
        let wrap = document.createElement("span");

        let l_ico = gen_ico(
            this.owner.class_name,
            this.owner.color_expected,
            this.owner.color_actual
        );
        let r_ico = gen_ico(
            this.mut.class_name,
            this.mut.color_expected,
            this.mut.color_actual
        );

        wrap.appendChild(l_ico);
        wrap.appendChild(r_ico);

        console.log(wrap);

        return wrap;
    }

    ignoreEvent() { return false }
}

// ----------------------------------------

type Icon {
    class_name: string,
    color_expected: `rgba(${number},${number},${number},${number})`,
    color_actual: `rgba(${number},${number},${number},${number})`,
}

let setMethodCallPoints = StateEffect.define<Array<[Icon, Icon, number]>>();

let  methodCallPoint = (ico_o, ico_m) =>
    Decoration.widget({
    widget: new CallTypesWidget(ico_o, ico_m),
    side: 0,
});

let methodCallPoints = StateField.define<DecorationSet>({
    create: () => Decoration.none,
    update(points, transactions) {

        console.log(transactions);

        for (let e of transactions.effects) {
            if (e.is(setMethodCallPoints)) {
                console.log(e);
                return RangeSet.of(e.value.map(([ico_o, ico_m, from]) =>
                    methodCallPoint(ico_o, ico_m).range(from)));
            }
        }

        return points;
    },
    provide: f => EditorView.decorations.from(f),
});
