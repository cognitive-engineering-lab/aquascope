import { basicSetup } from "./setup";
import * as ty from "./types";
import { rust } from "@codemirror/lang-rust";
import { indentUnit } from "@codemirror/language";
import {
  EditorState,
  StateField,
  StateEffect,
  StateEffectType,
  RangeSet,
  Compartment,
  Range,
} from "@codemirror/state";
import {
  EditorView,
  WidgetType,
  Decoration,
  DecorationSet,
  ViewUpdate,
  ViewPlugin,
} from "@codemirror/view";
import { vim } from "@replit/codemirror-vim";

const initial_code: string = `// Please start typing :)

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

fn foo(v: &mut Vec<i32>) {
  for (i, t) in v.iter().enumerate().rev() {
    if *t == 0 {
      v.remove(i);
    }
  }
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

let readOnly = new Compartment();
let mainKeybinding = new Compartment();

export interface Icon {
  readonly display: boolean;
  toDom(): HTMLElement;
}

interface IconField<C, Ico extends Icon, T> {
  effect_type: StateEffectType<Array<T>>;
  state_field: StateField<DecorationSet>;
  make_decoration(icos: Array<Ico>): Decoration;
  from_output(call_types: C): T;
}

export class Editor {
  private view: EditorView;

  public constructor(
    dom: HTMLElement,
    supported_fields: Array<StateField<DecorationSet>>
  ) {
    let initial_state = EditorState.create({
      doc: initial_code,
      extensions: [
        mainKeybinding.of(vim()),
        basicSetup,
        rust(),
        readOnly.of(EditorState.readOnly.of(false)),
        indentUnit.of("    "),
        ...supported_fields,
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
      effects: [readOnly.reconfigure(EditorState.readOnly.of(b))],
    });
  }

  public toggle_vim(b: boolean): void {
    let t = b ? [vim(), basicSetup] : [basicSetup];
    this.view.dispatch({
      effects: [mainKeybinding.reconfigure(t)],
    });
  }

  public remove_icon_field<
    B,
    T,
    Ico extends Icon,
    F extends IconField<B, Ico, T>
  >(f: F) {
    this.view.dispatch({
      effects: [f.effect_type.of([])],
    });
  }

  public add_call_types_field<
    B,
    T,
    Ico extends Icon,
    F extends IconField<B, Ico, T>
  >(f: F, method_call_points: Array<B>) {
    let new_effects = method_call_points.map(f.from_output);
    console.log(new_effects);
    this.view.dispatch({
      effects: [f.effect_type.of(new_effects)],
    });
  }
}

// ----------------------------------------
// Types to use in an Icon Field

// function obj_hash(o: object): number {
//     let str: string = JSON.stringify(o);
//     var h: number = 0;
//     for (var i = 0; i < str.length; i++) {
//         h = 31 * h + str.charCodeAt(i);
//     }
//     return h & 0xFFFFFFFF
// }

type RGBA = `rgba(${number},${number},${number},${number})`;
type RGB = `rgb(${number},${number},${number})`;

type Color = RGB | RGBA;

// Default colors
let read_color: RGB = `rgb(${93},${202},${54})`;
let write_color: RGB = `rgb(${78},${190},${239})`;
let drop_color: RGB = `rgb(${255},${66},${68})`;

let permission_state_ico_type =
  StateEffect.define<Array<PermissionPoint<TextIco>>>();

type PermissionPoint<I extends Icon> = [I, I, I, number];

let glyph_width = 12;

class RegionEnd extends WidgetType {
  constructor(readonly elem: HTMLElement) {
    super();
  }

  eq(other: RegionEnd) {
    return false;
  }

  toDOM() {
    return this.elem;
  }
}

let makeBraceElem = (content: string, color: Color) => {
  let wrap = document.createElement("span");
  wrap.classList.add("cm-region-end");
  wrap.textContent = content;
  wrap.style.color = color;
  wrap.style.fontSize = `${glyph_width * 2}`;
  return wrap;
};

class TextIco implements Icon {
  readonly display: boolean;
  readonly start: HTMLElement;
  readonly end: HTMLElement;
  constructor(
    readonly contents: string,
    readonly expected: boolean,
    readonly actual: boolean,
    readonly color: Color,
    readonly on_hover: ty.RefinementRegion | null
  ) {
    this.display = expected;
    this.start = makeBraceElem("{ ", color);
    this.end = makeBraceElem(" }", color);
  }

  getAuxiliary(): Array<Range<Decoration>> {
    if (this.on_hover == null || !this.display) {
      return [];
    }

    // TODO loan highlighting?

    let loan_deco = Decoration.mark({ class: "cm-loan" }).range(
      this.on_hover.loan_location.char_start,
      this.on_hover.loan_location.char_end
    );

    let start_deco = Decoration.widget({
      widget: new RegionEnd(this.start),
      side: 0,
    }).range(this.on_hover.start.char_start);

    let end_deco = Decoration.widget({
      widget: new RegionEnd(this.end),
      side: 0,
    }).range(this.on_hover.end.char_end);

    let extra_decos = [loan_deco, start_deco, end_deco];

    return extra_decos;
  }

  toDom(): HTMLElement {
    let tt = document.createElementNS("http://www.w3.org/2000/svg", "text");
    tt.classList.add("permission");
    tt.setAttribute("font-family", "IBM Plex Sans");
    tt.setAttribute("font-size", `${glyph_width}px`);
    tt.setAttribute("font-weight", "bold");
    tt.setAttribute("stroke-width", this.actual == this.expected ? "1" : "2");
    tt.setAttribute("paint-order", "stroke");
    tt.textContent = this.contents;

    tt.addEventListener("mouseenter", (_) => {
      this.start.style.width = "15px";
      this.end.style.width = "15px";
    });
    tt.addEventListener("mouseleave", (_) => {
      this.start.style.width = "0px";
      this.end.style.width = "0px";
    });

    return tt as HTMLElement & SVGTextElement;
  }
}

class RWDPermissions<I extends TextIco> extends WidgetType {
  constructor(readonly read: I, readonly write: I, readonly drop: I) {
    super();
  }

  eq(other: RWDPermissions<I>) {
    return (
      other.read == this.read &&
      other.write == this.write &&
      other.drop == this.drop
    );
  }

  toDOM() {
    let all: Array<I> = [this.read, this.write, this.drop];
    let icons: Array<I> = all.filter((t) => t.display);

    let wrap = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    wrap.classList.add("svg-perm");
    let my_height = icons.length * glyph_width;
    let my_width = glyph_width;
    wrap.setAttribute("width", `${my_width + 10}px`);
    wrap.setAttribute("height", `${my_height}px`);

    icons.forEach((ico_i: I, idx: number) => {
      let ico: HTMLElement = ico_i.toDom();
      let y = (idx / icons.length) * 100 + 100 / icons.length - 5;
      ico.setAttribute("text-anchor", "middle");
      ico.setAttribute("x", "50%");
      ico.setAttribute("y", `${y}%`);
      let fill_color: Color = ico_i.actual ? ico_i.color : "rgb(255,255,255)";
      ico.setAttribute("fill", fill_color);
      ico.setAttribute("stroke", ico_i.color);
      wrap.appendChild(ico);
    });

    return wrap as HTMLElement & SVGSVGElement;
  }

  ignoreEvent() {
    return false;
  }
}

let call_types_to_permissions = (
  perm_info: ty.PermissionsInfo
): PermissionPoint<TextIco> => {
  // XXX: each text icon *could* be hoverable and onHover this should highlight
  // - The loan range
  // - The start of the loan
  // - The end of the loan

  const read_ico = new TextIco(
    "R",
    perm_info.expected.read,
    perm_info.actual.read,
    read_color,
    perm_info.refined_by == null ? null : perm_info.refined_by.read
  );
  const write_ico = new TextIco(
    "W",
    perm_info.expected.write,
    perm_info.actual.write,
    write_color,
    perm_info.refined_by == null ? null : perm_info.refined_by.write
  );
  const drop_ico = new TextIco(
    "D",
    perm_info.expected.drop,
    perm_info.actual.drop,
    drop_color,
    perm_info.refined_by == null ? null : perm_info.refined_by.drop
  );

  // HACK the ending character of the actual type
  // might not actually be right before the dereference
  // operator `.`, do some testing and then we can probably
  // use the character preceding the expected `char_start`.
  let loc = perm_info.range.char_start - 1;

  return [read_ico, write_ico, drop_ico, loc];
};

let make_text_state_field_with_icon = <I extends TextIco>(
  ty: StateEffectType<Array<PermissionPoint<I>>>,
  makePermStack: (icos: Array<I>) => Decoration
) => {
  return StateField.define<DecorationSet>({
    create: () => Decoration.none,
    update(points, transactions) {
      console.log(transactions);
      for (let e of transactions.effects) {
        if (e.is(ty)) {
          return RangeSet.of(
            e.value.flatMap(([ico_l, ico_m, ico_r, from]) => {
              let main_deco = makePermStack([ico_l, ico_m, ico_r]).range(from);
              return [
                main_deco,
                ...ico_l.getAuxiliary(),
                ...ico_m.getAuxiliary(),
                ...ico_r.getAuxiliary(),
              ].sort((r1, r2) => r1.from - r2.from);
            }),
            true
          );
        }
      }

      return transactions.docChanged ? RangeSet.of([]) : points;
    },
    provide: (f) => EditorView.decorations.from(f),
  });
};

let make_decoration_with_text_ico = <I extends TextIco>(
  icos: Array<I>
): Decoration => {
  let fst = icos[0];
  let snd = icos[1];
  let trd = icos[2];
  return Decoration.widget({
    widget: new RWDPermissions<I>(fst, snd, trd),
    side: 0,
  });
};

export let rwd_permissions_field: IconField<
  ty.PermissionsInfo,
  TextIco,
  PermissionPoint<TextIco>
> = {
  effect_type: permission_state_ico_type,
  state_field: make_text_state_field_with_icon(
    permission_state_ico_type,
    make_decoration_with_text_ico<TextIco>
  ),
  make_decoration: make_decoration_with_text_ico<TextIco>,
  from_output: call_types_to_permissions,
};
