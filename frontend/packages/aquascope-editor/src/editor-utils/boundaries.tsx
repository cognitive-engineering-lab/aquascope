import { Range, RangeSet, StateEffect, StateField } from "@codemirror/state";
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

import { AnalysisFacts, PermissionsBoundary } from "../types";
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
  exp,
  act,
  showit,
  hideit,
}: {
  content: string;
  names: string[];
  exp: boolean;
  act: boolean;
  showit: () => void;
  hideit: () => void;
}) =>
  exp ? (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      className={classNames(...names, { missing: !act })}
      onMouseEnter={showit}
      onMouseLeave={hideit}
    >
      <text textAnchor="middle" x="50%" y="100%">
        {content}
      </text>
    </svg>
  ) : null;

let PermStack = ({
  facts,
  boundary,
}: {
  facts: AnalysisFacts;
  boundary: PermissionsBoundary;
}) => {
  const data = boundary.actual;
  let readIco = (
    <PermChar
      content={readChar}
      names={["permission", "read"]}
      exp={boundary.expected.read}
      act={data.permissions.read}
      showit={() => showLoanRegion(facts, data.loan_read_refined, ["read"])}
      hideit={() => hideLoanRegion(facts, data.loan_read_refined, ["read"])}
    />
  );

  let writeIco = (
    <PermChar
      content={writeChar}
      names={["permission", "write"]}
      exp={boundary.expected.write}
      act={data.permissions.write}
      showit={() => showLoanRegion(facts, data.loan_write_refined, ["write"])}
      hideit={() => hideLoanRegion(facts, data.loan_write_refined, ["write"])}
    />
  );

  let dropIco = (
    <PermChar
      content={dropChar}
      names={["permission", "drop"]}
      exp={boundary.expected.drop}
      act={data.permissions.drop}
      showit={() => showLoanRegion(facts, data.loan_drop_refined, ["drop"])}
      hideit={() => hideLoanRegion(facts, data.loan_drop_refined, ["drop"])}
    />
  );

  return (
    <>
      {readIco}
      {writeIco}
      {dropIco}
    </>
  );
};

class BoundaryPointWidget extends WidgetType {
  numDisplayed: number;
  constructor(
    readonly facts: AnalysisFacts,
    readonly boundary: PermissionsBoundary
  ) {
    super();
    let toi = (b: boolean) => (b ? 1 : 0);
    this.numDisplayed = [
      toi(this.boundary.expected.read),
      toi(this.boundary.expected.write),
      toi(this.boundary.expected.drop),
    ].reduce((a, b) => a + b, 0);
  }

  eq(other: BoundaryPointWidget): boolean {
    // TODO
    return this.boundary.location === other.boundary.location;
  }

  toDOM(_view: EditorView): HTMLElement {
    let container = document.createElement("div");
    container.classList.add("permission-stack");
    if (this.numDisplayed === 2) {
      container.classList.add("double-stacked");
    }
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

let boundaryEffect = StateEffect.define<Range<Decoration>[]>();

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

export function renderBoundaries(
  view: EditorView,
  facts: AnalysisFacts,
  boundaries: PermissionsBoundary[]
) {
  let decos = _.sortBy(
    boundaries.map(b =>
      Decoration.widget({
        widget: new BoundaryPointWidget(facts, b),
      }).range(b.location)
    ),
    deco => deco.from
  );

  view.dispatch({
    effects: [boundaryEffect.of(decos)],
  });
}
