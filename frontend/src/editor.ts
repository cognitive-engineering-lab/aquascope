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
import * as ty from "./types"

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

let readOnly = new Compartment;
let mainKeybinding = new Compartment;

interface IconField<Ico, T> {
    effect_type: StateEffectType<Array<T>>;
    state_field: StateField<DecorationSet>;
    make_decoration(ico_o: Ico, ico_m: Ico): Decoration;
    from_call_types(call_types: ty.CallTypes): T;
}

export class Editor {
    private view: EditorView;

    public constructor(dom: HTMLElement, supported_fields: Array<StateField<DecorationSet>>) {
        let initial_state = EditorState.create({
            doc: initial_code,
            extensions: [
                mainKeybinding.of(vim()),
                basicSetup,
                rust(),
                readOnly.of(EditorState.readOnly.of(false)),
                indentUnit.of("    "),
                ...supported_fields
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


    public toggle_vim(b: boolean): void {
        let t = b ? [vim(), basicSetup] : [basicSetup];
        this.view.dispatch({
            effects: [mainKeybinding.reconfigure(t)]
        });
    }

    public remove_icon_field<T, Ico, F extends IconField<Ico, T>>(f: F) {
        this.view.dispatch({
            effects: [ f.effect_type.of([]) ]
        });
    }

    public add_icon_field<T, Ico, F extends IconField<Ico, T>>(
        f: F,
        method_call_points: ty.ReceiverTypes
    ) {
        this.view.dispatch({
            effects: [ f.effect_type.of(
                method_call_points.map(f.from_call_types)
            ) ]
        });
    }
}

// ----------------------------------------
// Types to use in an Icon Field

type RGBA = `rgba(${number},${number},${number},${number})`;

type Color = RGBA;

type ColorDiffIco = {
    class_name: string,
    color_expected: Color,
    color_actual: Color,
};

type StackedPointIco<Ico> = [Ico, Ico, number];

let stacked_color_ico_type = StateEffect.define<Array<StackedPointIco<ColorDiffIco>>>();

let make_cdico_decoration = (ico_o: ColorDiffIco, ico_m: ColorDiffIco): Decoration => {
    return Decoration.widget({
        widget: new CallTypesWidget(ico_o, ico_m),
        side: 0,
    });
};

export let square_circle_field: IconField<ColorDiffIco, StackedPointIco<ColorDiffIco>> = {

    effect_type: stacked_color_ico_type,

    state_field: StateField.define<DecorationSet>({
        create: () => Decoration.none,
        update(points, transactions) {
            console.log(transactions);
            for (let e of transactions.effects) {
                if (e.is(stacked_color_ico_type)) {
                    return RangeSet.of(e.value.map(([ico_o, ico_m, from]) =>
                        make_cdico_decoration(ico_o, ico_m).range(from)));
                }
            }

            return points;
        },
        provide: f => EditorView.decorations.from(f),
    }),

    make_decoration(ico_o: ColorDiffIco, ico_m: ColorDiffIco): Decoration {
        return make_cdico_decoration(ico_o, ico_m);
        // return Decoration.widget({
        //     widget: new CallTypesWidget(ico_o, ico_m),
        //     side: 0,
        // });
    },

    from_call_types(call_type: ty.CallTypes): StackedPointIco<ColorDiffIco> {
        let high_color: RGBA = `rgba(${112},${128},${144},${1})`;
        let low_color: RGBA = `rgba(${233},${236},${238},${1})`;
        let own_shape = "fa-square";
        let mut_shape = "fa-circle";

        let color = (b: boolean) => (b ? high_color : low_color);
        let ts_actual: ty.TypeState = call_type.actual.of_type;
        let ts_expected: ty.TypeState = call_type.expected.of_type;

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
        let gen_ico = (name: string, color1: Color, color2: Color) => {
            let make_name_at_size = (ico_name: string) =>
                (n: number) => `fa ${ico_name} fa-stack-${n}x`;
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
