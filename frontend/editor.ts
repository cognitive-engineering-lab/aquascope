import { basicSetup } from "./setup"
import { vim } from "@replit/codemirror-vim"
import {
    EditorView, WidgetType,
    Decoration, DecorationSet,
    ViewUpdate, ViewPlugin
} from "@codemirror/view"
import {
    EditorState, StateField,
    StateEffect, StateEffectType,
    RangeSet, Compartment
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

interface IconTransformer<Ico> {
    make_widget(Ico, Ico): WidgetType;
    from_call_types(CallTypes): [Ico, Ico, number];
}

let set_method_call_points =
    StateEffect.define<Array<[ColorDiffIco, ColorDiffIco, number]>>();

let method_call_points = StateField.define<DecorationSet>({
    create: () => Decoration.none,
    update(points, transactions) {
        console.log(transactions);
        for (let e of transactions.effects) {
            if (e.is(set_method_call_points)) {
                return RangeSet.of(e.value.map(([ico_o, ico_m, from]) =>
                    method_call_point(ico_o, ico_m).range(from)));
            }
        }

        return points;
    },
    provide: f => EditorView.decorations.from(f),
});

let method_call_point = (ico_o: Ico, ico_m: Ico): WidgetType => {
    return Decoration.widget({
        widget: ShapeT.make_widget(ico_o, ico_m),
        side: 0,
    });
};

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
                method_call_points
            ],
        });

        let initial_view = new EditorView({
            state: initial_state,
            parent: dom,
        });


        this.set_method_call_points
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
            effects: [ set_method_call_points.of([]) ]
        });
    }

    // XXX this method gets called when the receiver types are received from
    // the backend, however, on `update` (e.g. someone is typing code) we
    // probably want them to go away.
    public show_receiver_types(method_call_points: ReceiverTypes): void {
        this.view.dispatch({
            effects: [
                set_method_call_points.of(
                    method_call_points.map(ShapeT.from_call_types))
            ]
        });
    }
}

// NOTE (to self) XXX: last night I tried to make Editor generic
// over a class and an "IconTransformer" (there must be a better
// name for that) but the `ShapeEffectType.is` method didn't seem
// to be identifying the effect type. Not sure why that was, things
// are currently working but I would like them to be more generic
// so I can easily switch between things like Icons, what differentiates,
// etc so we can try out a myriad of things.

// ----------------------------------------
// Types to use as an icon transformer

type RGBA = `rgba(${number},${number},${number},${number})`;

type ColorDiffIco {
    class_name: string,
    color_expected: RGBA,
    color_actual: RGBA,
}

export let ShapeT: IconTransformer<ColorDiffIco> = {

    make_widget(ico_l, ico_r) {
        return new CallTypesWidget(ico_l, ico_r);
    }

    from_call_types(call_type: CallTypes): [ColorDiffIco, ColorDiffIco, number] {
        let high_color = `rgba(112,128,144,1)`;
        let low_color = `rgba(233,236,238,1)`;
        let own_shape = "fa-square";
        let mut_shape = "fa-circle";

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

        let owned_ico: ColorDiffIco = {
            class_name: own_shape,
            color_expected: color(e_o),
            color_actual: color(a_o)
        };

        let mut_ico: ColorDiffIco = {
            class_name: mut_shape,
            color_expected: color(e_m),
            color_actual: color(a_m)
        };

        // HACK the ending character of the actual type
        // might not actually be right before the dereference
        // operator `.`, do some testing and then we can probably
        // use the character preceding the expected `char_start`.
        let loc = call_type.expected.range.char_start - 1;
        return [owned_ico, mut_ico, loc];
    }
}

// Example Widget from the codemirror 6 api doc.
class CallTypesWidget extends WidgetType {
    constructor(readonly owner: ColorDiffIco, readonly mut: ColorDiffIco) { super() }

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
