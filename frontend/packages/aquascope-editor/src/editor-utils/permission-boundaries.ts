import { Range, StateEffect } from "@codemirror/state";
import {
  Decoration,
  EditorView,
  WidgetType,
  hoverTooltip,
} from "@codemirror/view";

import {
  AnalysisFacts,
  LoanKey,
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
  constructor(
    readonly contents: string,
    readonly expected: boolean,
    readonly actual: boolean,
    readonly color: Color,
    readonly facts: AnalysisFacts,
    readonly typeRefined: boolean,
    readonly moveRefined: boolean,
    readonly loanRefined?: LoanKey,
    readonly wasCopied: boolean = false
  ) {
    this.display = expected;
  }

  getAuxiliary(): Array<Range<Decoration>> {
    return [];
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

    let forCustomTag = (tag: string, callback: (e: HTMLElement) => void) => {
      Array.from(
        document.getElementsByTagName(tag) as HTMLCollectionOf<HTMLElement>
      ).forEach(callback);
    };

    let insertHoverToggle = (loanKey: number) => {
      let myColor = this.color;
      let transparent = whiteColor.withAlpha(0.0);

      const loanTag = this.facts.loanPoints[loanKey];
      const regionTag = this.facts.loanRegions[loanKey];

      console.log(`inserting the toggle! ${loanTag} ${regionTag}`);

      tt.addEventListener("mouseenter", _ => {
        forCustomTag(loanTag, elem => {
          elem.style.textDecoration = `underline 3px ${myColor.toString()}`;
        });

        forCustomTag(regionTag, elem => {
          elem.style.backgroundColor = myColor.withAlpha(0.2).toString();
        });
      });

      tt.addEventListener("mouseleave", _ => {
        forCustomTag(loanTag, elem => {
          elem.style.textDecoration = `underline 3px ${transparent.toString()}`;
        });

        forCustomTag(regionTag, elem => {
          elem.style.backgroundColor = whiteColor.withAlpha(0).toString();
        });
      });
    };

    console.log(this);

    if (this.loanRefined !== undefined) {
      insertHoverToggle(this.loanRefined);
    }

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

    // TODO: this should be configurable from the embed script, not directly
    // inside aquascope-editor
    let bgVar = getComputedStyle(document.body).getPropertyValue("--bg");
    let hollowColor = bgVar !== "" ? "var(--bg)" : whiteColor.toString();

    icons.forEach((icoI: SinglePermIcon, idx: number) => {
      let ico: HTMLElement = icoI.toDOM();
      let y = (idx / icons.length) * 100 + 100 / icons.length - 5;
      ico.setAttribute("text-anchor", "middle");
      ico.setAttribute("x", "50%");
      ico.setAttribute("y", `${y}%`);
      let fillColor = icoI.actual ? icoI.color.toString() : hollowColor;
      ico.setAttribute("fill", fillColor);
      ico.setAttribute("stroke", icoI.color.toString());
      ico.dataset.bufferPos = this.pos.toString();

      // FIXME!
      // if (icoI.wasCopied) {
      //   ico.classList.add("copied-tip");
      // } else if (icoI.onHover?.type === "InsufficientType") {
      //   ico.classList.add("insufficient-type-tip");
      // }

      // TODO: we also need to include a tooltip for moves.
      wrap.appendChild(ico);
    });

    return wrap as HTMLElement & SVGSVGElement;
  }

  ignoreEvent() {
    return false;
  }
}

let fromPermissionsBoundary = (
  permInfo: PermissionsBoundary,
  facts: AnalysisFacts
): BoundaryPoint => {
  const data = permInfo.actual;

  console.log(permInfo);

  const readIco = new SinglePermIcon(
    readChar,
    permInfo.expected.read,
    data.permissions.read,
    readColor,
    facts,
    false,
    data.path_moved,
    data.loan_read_refined
  );
  const writeIco = new SinglePermIcon(
    writeChar,
    permInfo.expected.write,
    data.permissions.write,
    writeColor,
    facts,
    !data.type_writeable,
    data.path_moved,
    data.loan_write_refined
  );
  const dropIco = new SinglePermIcon(
    dropChar,
    permInfo.expected.drop,
    data.permissions.drop,
    dropColor,
    facts,
    !data.type_droppable,
    data.path_moved,
    data.loan_write_refined,
    data.type_copyable
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
  fromOutput: fromPermissionsBoundary,
};
