import { rust } from "@codemirror/lang-rust";
import { indentUnit } from "@codemirror/language";
import {
  Compartment,
  EditorState,
  Range,
  RangeSet,
  StateEffect,
  StateEffectType,
  StateField,
} from "@codemirror/state";
import {
  Decoration,
  DecorationSet,
  EditorView,
  WidgetType,
} from "@codemirror/view";
import { vim } from "@replit/codemirror-vim";

import { basicSetup } from "./setup";
import {
  BackendError,
  PermissionsInfo,
  PermissionsOutput,
  RefinementRegion,
} from "./types";

const DEFAULT_SERVER_HOST = "127.0.0.1";
const DEFAULT_SERVER_PORT = "8008";

type Result<T> = { Ok: T } | { Err: BackendError };

// XXX this extra server response type is really
// annoying and I'd like to get rid of it. This change would
// require modifying how command output is read from the spawned
// docker container on the backend.
type ServerResponse = {
  success: boolean;
  stdout: string;
  stderr: string;
};

export const defaultCodeExample: string = `// Please, start typing :)

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
  effectType: StateEffectType<Array<T>>;
  stateField: StateField<DecorationSet>;
  makeDecoration(icos: Array<Ico>): Decoration;
  fromOutput(callTypes: C): T;
}

export class Editor {
  private view: EditorView;

  public constructor(
    dom: HTMLElement,
    supportedFields: Array<StateField<DecorationSet>>,
    initialCode: string = defaultCodeExample,
    readonly serverHost: string = DEFAULT_SERVER_HOST,
    readonly serverPort: string = DEFAULT_SERVER_PORT,
    readonly noInteract: boolean = false
  ) {
    let initialState = EditorState.create({
      doc: initialCode,
      extensions: [
        // mainKeybinding.of(vim()),
        readOnly.of(EditorState.readOnly.of(noInteract)),
        basicSetup,
        rust(),
        indentUnit.of("    "),
        ...supportedFields,
      ],
    });

    let initialView = new EditorView({
      state: initialState,
      parent: dom,
    });

    this.view = initialView;
  }

  public getCurrentCode(): string {
    return this.view.state.doc.toString();
  }

  public toggleVim(b: boolean): void {
    let t = b ? [vim(), basicSetup] : [basicSetup];
    this.view.dispatch({
      effects: [mainKeybinding.reconfigure(t)],
    });
  }

  public removeIconField<
    B,
    T,
    Ico extends Icon,
    F extends IconField<B, Ico, T>
  >(f: F) {
    this.view.dispatch({
      effects: [f.effectType.of([])],
    });
  }

  public addCallTypesField<
    B,
    T,
    Ico extends Icon,
    F extends IconField<B, Ico, T>
  >(f: F, method_call_points: Array<B>) {
    let new_effects = method_call_points.map(f.fromOutput);
    console.log(new_effects);
    this.view.dispatch({
      effects: [f.effectType.of(new_effects)],
    });
  }

  // Actions to communicate with the aquascope server

  async computeReceiverPermissions() {
    let inEditor = this.getCurrentCode();
    let serverResponseRaw = await fetch(
      `http://${this.serverHost}:${this.serverPort}/receiver-types`,
      {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          code: inEditor,
        }),
      }
    );

    let serverResponse: ServerResponse = await serverResponseRaw.json();

    // TODO: on errors we should have a side panel where the Rustc output is
    // placed. It would be /ideal/ to have something like Rust analyzer display
    // errors with parsing / function type errors but that is out-of-scope for now.
    let handleErrors = (e: BackendError) => {
      console.log(e);
      alert("An error occurred, check your logs");
      return;
    };

    if (serverResponse.success) {
      let out: Result<PermissionsOutput> = JSON.parse(serverResponse.stdout);
      if ("Ok" in out) {
        console.log(`Stderr: ${serverResponse.stderr}`);
        return this.addCallTypesField(receiverPermissionsField, out.Ok);
      } else {
        return handleErrors(out.Err);
      }
    } else {
      return handleErrors({
        type: "BuildError",
        error: serverResponse.stderr,
      });
    }
  }
}

// ----------------------------------------
// Types to use in an Icon Field

type RGBA = `rgba(${number},${number},${number},${number})`;
type RGB = `rgb(${number},${number},${number})`;

type Color = RGB | RGBA;

// Default colors
let read_color: RGB = `rgb(93,202,54)`; // green
let write_color: RGB = `rgb(78,190,239)`; // blue
let drop_color: RGB = `rgb(255,66,68)`; // red

let permission_state_ico_type =
  StateEffect.define<Array<PermissionPoint<TextIco>>>();

type PermissionPoint<I extends Icon> = [I, I, I, number];

let glyph_width = 12;

class RegionEnd extends WidgetType {
  constructor(readonly elem: HTMLElement) {
    super();
  }

  eq(_other: RegionEnd) {
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
    readonly on_hover: RefinementRegion | null
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

    tt.addEventListener("mouseenter", _ => {
      this.start.style.width = "15px";
      this.end.style.width = "15px";
    });
    tt.addEventListener("mouseleave", _ => {
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
    let icons: Array<I> = all.filter(t => t.display);

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
  perm_info: PermissionsInfo
): PermissionPoint<TextIco> => {
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
    provide: f => EditorView.decorations.from(f),
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

export let receiverPermissionsField: IconField<
  PermissionsInfo,
  TextIco,
  PermissionPoint<TextIco>
> = {
  effectType: permission_state_ico_type,
  stateField: make_text_state_field_with_icon(
    permission_state_ico_type,
    make_decoration_with_text_ico<TextIco>
  ),
  makeDecoration: make_decoration_with_text_ico<TextIco>,
  fromOutput: call_types_to_permissions,
};
