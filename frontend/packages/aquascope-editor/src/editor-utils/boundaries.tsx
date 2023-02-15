import {
  Line,
  Range,
  RangeSet,
  StateEffect,
  StateField,
} from "@codemirror/state";
import {
  Decoration,
  DecorationSet,
  EditorView,
  WidgetType,
} from "@codemirror/view";
import classNames from "classnames";
import _ from "lodash";
import React from "react";
import ReactDOM from "react-dom/client";

import {
  AnalysisFacts,
  BoundariesAnnotations,
  PermissionsBoundary,
} from "../types";
import {
  dropChar,
  hideLoanRegion,
  readChar,
  showLoanRegion,
  writeChar,
} from "./misc";

// FIXME: the tooltips are not currently being used. The tooltips
// provided by CM6 aren't expressive enough for what we want.
// export const copiedValueHover = hoverTooltip(
//   (_view, pos: number, side: number) => {
//     let copyPoints = Array.from(
//       document.querySelectorAll<HTMLElement>(".copied-tip")
//     );

//     let sPos = pos.toString();
//     let hovered = copyPoints.find(s => s.dataset.bufferPos == sPos);
//     if (hovered == undefined || hovered == null || side >= 0) {
//       return null;
//     }

//     let copiedPerm = hovered.textContent!;
//     return {
//       pos: pos,
//       above: true,
//       arrow: true,
//       create(_view) {
//         let dom = document.createElement("div");
//         dom.textContent = "Value was copied: creating 'O' permission";
//         dom.classList.add("cm-tooltip-cursor");
//         return { dom };
//       },
//     };
//   }
// );

// export const insufficientTypeHover = hoverTooltip(
//   (_view, pos: number, side: number) => {
//     let copyPoints = Array.from(
//       document.querySelectorAll<HTMLElement>(".insufficient-type-tip")
//     );

//     let sPos = pos.toString();
//     let hovered = copyPoints.find(s => s.dataset.bufferPos == sPos);
//     if (hovered == undefined || hovered == null || side >= 0) {
//       return null;
//     }

//     let perm = hovered.textContent!;
//     return {
//       pos: pos,
//       above: true,
//       arrow: true,
//       create(_view) {
//         let dom = document.createElement("div");
//         dom.textContent = `Declared type does not allow for permission '${perm}'`;
//         dom.classList.add("cm-tooltip-cursor");
//         return { dom };
//       },
//     };
//   }
// );

let PermChar = ({
  content,
  names,
  act,
  showit,
  hideit,
}: {
  content: string;
  names: string[];
  act: boolean;
  showit: () => void;
  hideit: () => void;
}) => (
  <div
    className={classNames(...names, { missing: !act })}
    onMouseEnter={showit}
    onMouseLeave={hideit}
  >
    <div className="small">•</div>
    <div className="big">{content}</div>
  </div>
);

let PermStack = ({
  facts,
  boundary,
}: {
  facts: AnalysisFacts;
  boundary: PermissionsBoundary;
}) => {
  const data = boundary.actual;

  let allIcons = [
    {
      content: readChar,
      names: ["permission", "read"],
      exp: boundary.expected.read,
      act: data.permissions.read,
      showit: () => showLoanRegion(facts, data.loan_read_refined, ["read"]),
      hideit: () => hideLoanRegion(facts, data.loan_read_refined, ["read"]),
    },
    {
      content: writeChar,
      names: ["permission", "write"],
      exp: boundary.expected.write,
      act: data.permissions.write,
      showit: () => showLoanRegion(facts, data.loan_write_refined, ["write"]),
      hideit: () => hideLoanRegion(facts, data.loan_write_refined, ["write"]),
    },
    {
      content: dropChar,
      names: ["permission", "drop"],
      exp: boundary.expected.drop,
      act: data.permissions.drop,
      showit: () => showLoanRegion(facts, data.loan_drop_refined, ["drop"]),
      hideit: () => hideLoanRegion(facts, data.loan_drop_refined, ["drop"]),
    },
  ];

  let icons = allIcons.filter(i => i.exp);

  return (
    <>
      {icons.map(info => (
        <PermChar key={info.content} {...info} />
      ))}
    </>
  );
  // <svg xmlns="http://www.w3.org/2000/svg" className="permission">
  //   {icons.map((info, i: number) => (
  //     <PermChar key={info.content} x="50%" y={h(i)} {...info} />
  //   ))}
  // </svg>
};

class BoundaryPointWidget extends WidgetType {
  line: Line;
  numDisplayed: number;
  constructor(
    readonly view: EditorView,
    readonly facts: AnalysisFacts,
    readonly boundary: PermissionsBoundary,
    readonly annotations?: BoundariesAnnotations
  ) {
    super();
    let toi = (b: boolean) => (b ? 1 : 0);
    this.numDisplayed = [
      toi(this.boundary.expected.read),
      toi(this.boundary.expected.write),
      toi(this.boundary.expected.drop),
    ].reduce((a, b) => a + b, 0);
    this.line = view.state.doc.lineAt(boundary.location);
  }

  eq(other: BoundaryPointWidget): boolean {
    // TODO
    return this.boundary.location === other.boundary.location;
  }

  toDOM(view: EditorView): HTMLElement {
    let precedingText = view.state.sliceDoc(
      this.boundary.location - 1,
      this.boundary.location
    );
    let container = document.createElement("div");
    container.classList.add("permission-stack");
    container.classList.add(`stack-size-${this.numDisplayed}`);
    if (precedingText === " ") container.classList.add("before-whitespace");
    if (this.annotations?.focused_lines.includes(this.line.number))
      container.classList.add("expanded");
    ReactDOM.createRoot(container).render(
      <PermStack facts={this.facts} boundary={this.boundary} />
    );
    return container;
  }

  ignoreEvent() {
    // TODO
    return false;
  }
}

export let boundaryEffect = StateEffect.define<Range<Decoration>[]>();

export let boundaryField = StateField.define<DecorationSet>({
  create: () => Decoration.none,

  update(values, trs) {
    for (let e of trs.effects) {
      if (e.is(boundaryEffect)) {
        return RangeSet.of(e.value, true);
      }
    }

    return trs.docChanged ? RangeSet.of([]) : values;
  },

  provide: f => EditorView.decorations.from(f),
});

export function makeBoundaryDecorations(
  view: EditorView,
  facts: AnalysisFacts,
  boundaries: PermissionsBoundary[],
  annotations?: BoundariesAnnotations
): Range<Decoration>[] {
  return _.sortBy(
    boundaries.map(b =>
      Decoration.widget({
        widget: new BoundaryPointWidget(view, facts, b, annotations),
      }).range(b.location)
    ),
    deco => deco.from
  );
}
