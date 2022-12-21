import { Range, StateEffect } from "@codemirror/state";
import {
  Decoration,
  EditorView,
  WidgetType,
  hoverTooltip,
} from "@codemirror/view";

import {
  MissingPermReason,
  PermissionsBoundary,
  RefinementRegion,
} from "../types";
import {
  Color,
  HTMLIcon,
  IconField,
  dropChar,
  dropColor,
  genStateField,
  glyphWidth,
  makeTag,
  readChar,
  readColor,
  whiteColor,
  writeChar,
  writeColor,
} from "./misc";

let permissionStateIcoType = StateEffect.define<Array<BoundaryPoint>>();

// A boundary point is the permissions stack which will
// be palced at a specific point in the editor.
type BoundaryPoint = [SinglePermIcon, SinglePermIcon, SinglePermIcon, number];

class RegionEnd extends WidgetType {
  constructor(readonly elem: HTMLElement) {
    super();
  }

  eq(_other: RegionEnd) {
    return false;
  }

  toDOM(_view: EditorView): HTMLElement {
    return this.elem;
  }
}

let makeBraceElem = (content: string, color: Color) => {
  let wrap = document.createElement("span");
  wrap.classList.add("cm-region-end");
  wrap.textContent = content;
  wrap.style.color = color.toString();
  wrap.style.fontSize = `${glyphWidth * 2}`;
  return wrap;
};

// Icon for displaying a single permission field, e.g. "R" in "RWO".
class SinglePermIcon implements HTMLIcon {
  readonly display: boolean;
  readonly start: HTMLElement;
  readonly end: HTMLElement;
  readonly loanTag: string;
  readonly regionTag: string;
  constructor(
    readonly contents: string,
    readonly expected: boolean,
    readonly actual: boolean,
    readonly color: Color,
    readonly onHover: MissingPermReason | undefined,
    readonly wasCopied: boolean = false
  ) {
    this.display = expected;
    this.start = makeBraceElem("{ ", color);
    this.end = makeBraceElem(" }", color);
    this.loanTag = makeTag(20);
    this.regionTag = makeTag(25);
  }

  getAuxiliary(): Array<Range<Decoration>> {
    if (this.onHover == undefined || !this.display) {
      return [];
    }

    const variant: string = this.onHover.type;

    if ("InsufficientType" === variant) {
      console.log("TODO: display explanation for insufficient types");
      return [];
    }

    const refinedRegion = this.onHover as RefinementRegion;

    let loanDeco = Decoration.mark({
      class: "aquascope-loan",
      tagName: this.loanTag,
    }).range(
      refinedRegion.refiner_point.char_start,
      refinedRegion.refiner_point.char_end
    );

    let regionDecos = refinedRegion.refined_ranges
      .filter(range => range.char_start != range.char_end)
      .map(range => {
        console.log(range);
        let highlightedRange = Decoration.mark({
          class: "aquascope-live-region",
          tagName: this.regionTag,
        }).range(range.char_start, range.char_end);
        return highlightedRange;
      });

    // The start and end are the colored braces which used
    // to indicate the start and end of a refinement region.
    // We aren't using them currently but who knows if they'll
    // be wanted in the future for anything.
    let start = this.start;
    let charStart = refinedRegion.start.char_start;
    let _startDeco = Decoration.widget({
      widget: new RegionEnd(start),
    }).range(charStart);

    let end = this.end;
    let charEnd = refinedRegion.end.char_end;
    let _endDeco = Decoration.widget({
      widget: new RegionEnd(end),
    }).range(charEnd);

    let extraDecos = [loanDeco, ...regionDecos];

    return extraDecos;
  }

  toDOM(): HTMLElement {
    let tt = document.createElementNS("http://www.w3.org/2000/svg", "text");
    tt.classList.add("permission");
    tt.setAttribute("font-family", "Verdana");
    tt.setAttribute("font-size", `${glyphWidth}px`);
    tt.setAttribute("font-weight", "bold");
    tt.setAttribute("stroke-width", this.actual == this.expected ? "1" : "2");
    tt.setAttribute("paint-order", "stroke");
    tt.textContent = this.contents;

    let myColor = this.color;
    let transparent = whiteColor.withAlpha(0.0);

    let forCustomTag = (tag: string, callback: (e: HTMLElement) => void) => {
      Array.from(
        document.getElementsByTagName(tag) as HTMLCollectionOf<HTMLElement>
      ).forEach(callback);
    };

    tt.addEventListener("mouseenter", _ => {
      this.start.style.width = "15px";
      this.end.style.width = "15px";

      forCustomTag(this.loanTag, elem => {
        elem.style.textDecoration = `underline 3px ${myColor.toString()}`;
      });

      forCustomTag(this.regionTag, elem => {
        elem.style.backgroundColor = myColor.withAlpha(0.2).toString();
      });
    });

    tt.addEventListener("mouseleave", _ => {
      this.start.style.width = "0px";
      this.end.style.width = "0px";

      forCustomTag(this.loanTag, elem => {
        elem.style.textDecoration = `underline 3px ${transparent.toString()}`;
      });

      forCustomTag(this.regionTag, elem => {
        elem.style.backgroundColor = whiteColor.withAlpha(0).toString();
      });
    });

    return tt as HTMLElement & SVGTextElement;
  }
}

// The widget which holds the internal SinglePermIcons
class BoundaryPointWidget extends WidgetType {
  constructor(
    readonly read: SinglePermIcon,
    readonly write: SinglePermIcon,
    readonly drop: SinglePermIcon,
    readonly pos: number
  ) {
    super();
  }

  eq(other: BoundaryPointWidget): boolean {
    return (
      other.read == this.read &&
      other.write == this.write &&
      other.drop == this.drop
    );
  }

  toDOM(_view: EditorView): HTMLElement {
    let all = [this.read, this.write, this.drop];
    let icons = all.filter(t => t.display);

    let wrap = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    wrap.classList.add("svg-perm");
    let myHeight = icons.length * glyphWidth;
    let myWidth = glyphWidth;
    wrap.setAttribute("width", `${myWidth + 10}px`);
    wrap.setAttribute("height", `${myHeight}px`);
    wrap.style.position = "relative";
    wrap.style.top = `${(icons.length - 1) * 4}px`;

    icons.forEach((icoI: SinglePermIcon, idx: number) => {
      let ico: HTMLElement = icoI.toDOM();
      let y = (idx / icons.length) * 100 + 100 / icons.length - 5;
      ico.setAttribute("text-anchor", "middle");
      ico.setAttribute("x", "50%");
      ico.setAttribute("y", `${y}%`);
      let fillColor: Color = icoI.actual ? icoI.color : whiteColor;
      ico.setAttribute("fill", fillColor.toString());
      ico.setAttribute("stroke", icoI.color.toString());
      ico.dataset.bufferPos = this.pos.toString();
      if (icoI.wasCopied) {
        ico.classList.add("copied-tip");
      } else if (icoI.onHover?.type === "InsufficientType") {
        ico.classList.add("insufficient-type-tip");
      }
      // TODO: we also need to include a tooltip for moves.
      wrap.appendChild(ico);
    });

    return wrap as HTMLElement & SVGSVGElement;
  }

  ignoreEvent() {
    return false;
  }
}

let callTypesToPermissions = (permInfo: PermissionsBoundary): BoundaryPoint => {
  const expl = permInfo.explanations;
  const readIco = new SinglePermIcon(
    readChar,
    permInfo.expected.read,
    permInfo.actual.read,
    readColor,
    expl?.read
  );
  const writeIco = new SinglePermIcon(
    writeChar,
    permInfo.expected.write,
    permInfo.actual.write,
    writeColor,
    expl?.write
  );
  const dropIco = new SinglePermIcon(
    dropChar,
    permInfo.expected.drop,
    permInfo.actual.drop,
    dropColor,
    expl?.drop,
    permInfo.was_copied
    // expl.was_copied
  );
  return [readIco, writeIco, dropIco, permInfo.location];
};

let makeDecorationFromIcon = (
  icos: Array<SinglePermIcon>,
  pos: number
): Decoration => {
  let fst = icos[0];
  let snd = icos[1];
  let trd = icos[2];
  return Decoration.widget({
    widget: new BoundaryPointWidget(fst, snd, trd, pos),
    side: 0,
  });
};

let boundaryStateField = genStateField<Array<BoundaryPoint>>(
  permissionStateIcoType,
  (ts: Array<BoundaryPoint>): Array<Range<Decoration>> => {
    return ts.flatMap(([icoL, icoM, icoR, pos]) => {
      let main_deco = makeDecorationFromIcon([icoL, icoM, icoR], pos).range(
        pos
      );
      return [
        main_deco,
        ...icoL.getAuxiliary(),
        ...icoM.getAuxiliary(),
        ...icoR.getAuxiliary(),
      ];
    });
  }
);

export const copiedValueHover = hoverTooltip(
  (_view, pos: number, side: number) => {
    let copyPoints = Array.from(
      document.querySelectorAll<HTMLElement>(".copied-tip")
    );

    let sPos = pos.toString();
    let hovered = copyPoints.find(s => s.dataset.bufferPos == sPos);
    if (hovered == undefined || hovered == null || side >= 0) {
      return null;
    }

    let copiedPerm = hovered.textContent!;
    return {
      pos: pos,
      above: true,
      arrow: true,
      create(_view) {
        let dom = document.createElement("div");
        dom.textContent = "Value was copied: creating 'O' permission";
        dom.classList.add("cm-tooltip-cursor");
        return { dom };
      },
    };
  }
);

export const insufficientTypeHover = hoverTooltip(
  (_view, pos: number, side: number) => {
    let copyPoints = Array.from(
      document.querySelectorAll<HTMLElement>(".insufficient-type-tip")
    );

    let sPos = pos.toString();
    let hovered = copyPoints.find(s => s.dataset.bufferPos == sPos);
    if (hovered == undefined || hovered == null || side >= 0) {
      return null;
    }

    let perm = hovered.textContent!;
    return {
      pos: pos,
      above: true,
      arrow: true,
      create(_view) {
        let dom = document.createElement("div");
        dom.textContent = `Declared type does not allow for permission '${perm}'`;
        dom.classList.add("cm-tooltip-cursor");
        return { dom };
      },
    };
  }
);

export const receiverPermissionsField: IconField<
  PermissionsBoundary,
  BoundaryPoint
> = {
  effectType: permissionStateIcoType,
  stateField: boundaryStateField,
  fromOutput: callTypesToPermissions,
};
