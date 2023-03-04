import { Line, Range } from "@codemirror/state";
import { Decoration, EditorView, WidgetType } from "@codemirror/view";
import classNames from "classnames";
import _ from "lodash";
import React, { useState } from "react";
import ReactDOM from "react-dom/client";

import {
  AnalysisFacts,
  BoundariesAnnotations,
  PermissionsBoundary,
} from "../types";
import {
  dropChar,
  flowChar,
  hideLoanRegion,
  hideMoveRegion,
  makeDecorationField,
  readChar,
  showLoanRegion,
  showMoveRegion,
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
      names: ["perm", "read"],
      exp: boundary.expected.read,
      act: data.permissions.read,
      showit: () => {
        showLoanRegion(facts, data.loan_read_refined, ["read"]);
        showMoveRegion(facts, data.path_moved, ["read"]);
      },
      hideit: () => {
        hideLoanRegion(facts, data.loan_read_refined, ["read"]);
        hideMoveRegion(facts, data.path_moved, ["read"]);
      },
    },
    {
      content: writeChar,
      names: ["perm", "write"],
      exp: boundary.expected.write,
      act: data.permissions.write,
      showit: () => {
        showLoanRegion(facts, data.loan_write_refined, ["write"]);
        showMoveRegion(facts, data.path_moved, ["write"]);
      },
      hideit: () => {
        hideLoanRegion(facts, data.loan_write_refined, ["write"]);
        hideMoveRegion(facts, data.path_moved, ["write"]);
      },
    },
    {
      content: dropChar,
      names: ["perm", "drop"],
      exp: boundary.expected.drop,
      act: data.permissions.drop,
      showit: () => {
        showLoanRegion(facts, data.loan_drop_refined, ["drop"]);
        showMoveRegion(facts, data.path_moved, ["drop"]);
      },
      hideit: () => {
        hideLoanRegion(facts, data.loan_drop_refined, ["drop"]);
        hideMoveRegion(facts, data.path_moved, ["drop"]);
      },
    },
    {
      content: flowChar,
      names: ["perm", "flow"],
      exp: boundary.expecting_flow !== undefined,
      act: !boundary.expecting_flow?.is_violation ?? false,
      showit: () => void null,
      hideit: () => void null,
    },
  ];

  let icons = allIcons.filter(i => i.exp);

  // Necessary so we can temporarily apply the CSS filter and remove it
  // at the end of the animation. Sadly no way to do this in pure CSS AFAIK...
  let [animating, setAnimating] = useState(false);
  let [timer, setTimer] = useState<number | undefined>();
  let triggerAnimation = () => {
    setAnimating(true);
    if (timer !== 0) clearTimeout(timer);
    setTimer(setTimeout(() => setAnimating(false), 500));
  };

  return (
    <div
      onMouseEnter={triggerAnimation}
      onMouseLeave={triggerAnimation}
      className={classNames({ animating })}
    >
      {icons.map(info => (
        <PermChar key={info.content} {...info} />
      ))}
    </div>
  );
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

export let boundariesField = makeDecorationField();

export function makeBoundaryDecorations(
  view: EditorView,
  facts: AnalysisFacts,
  boundaries: PermissionsBoundary[],
  annotations?: BoundariesAnnotations
): Range<Decoration>[] {
  return boundaries.map(b =>
    Decoration.widget({
      widget: new BoundaryPointWidget(view, facts, b, annotations),
    }).range(b.location)
  );
}
