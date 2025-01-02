import type { Line, Range } from "@codemirror/state";
import { Decoration, type EditorView, WidgetType } from "@codemirror/view";
import classNames from "classnames";
import React, { useState } from "react";
import ReactDOM from "react-dom/client";

import type {
  AnalysisFacts,
  BoundariesAnnotations,
  PermissionsBoundary
} from "../types.js";
import {
  type PermLetter,
  flowChar,
  hideLoanRegion,
  hideMoveRegion,
  linecolToPosition,
  makeDecorationField,
  ownChar,
  permName,
  readChar,
  showLoanRegion,
  showMoveRegion,
  writeChar
} from "./misc.js";

interface PermCharProps {
  content: PermLetter;
  names: string[];
  act: boolean;
  showit: () => void;
  hideit: () => void;
}

let PermChar = ({ content, names, act, showit, hideit }: PermCharProps) => {
  let name = permName(content);
  name = name.charAt(0).toUpperCase() + name.slice(1);
  let title = `${name} permission is expected, `;
  title += act
    ? "and the place has the permission."
    : "but the place does not have the permission.";
  return (
    <div
      className={classNames(...names, { missing: !act })}
      onMouseEnter={showit}
      onMouseLeave={hideit}
      title={title}
    >
      <div className="small">•</div>
      <div className="big">{content}</div>
    </div>
  );
};

let PermStack = ({
  facts,
  boundary
}: {
  facts: AnalysisFacts;
  boundary: PermissionsBoundary;
}) => {
  const data = boundary.data;

  let allIcons: (PermCharProps & { exp: boolean })[] = [
    {
      content: readChar,
      names: ["perm", "read"],
      exp: boundary.expected.read,
      act: boundary.actual.read,
      showit: () => {
        showLoanRegion(facts, data.loan_read_refined, ["read"]);
        showMoveRegion(facts, data.path_moved, ["read"]);
      },
      hideit: () => {
        hideLoanRegion(facts, data.loan_read_refined, ["read"]);
        hideMoveRegion(facts, data.path_moved, ["read"]);
      }
    },
    {
      content: writeChar,
      names: ["perm", "write"],
      exp: boundary.expected.write,
      act: boundary.actual.write,
      showit: () => {
        showLoanRegion(facts, data.loan_write_refined, ["write"]);
        showMoveRegion(facts, data.path_moved, ["write"]);
      },
      hideit: () => {
        hideLoanRegion(facts, data.loan_write_refined, ["write"]);
        hideMoveRegion(facts, data.path_moved, ["write"]);
      }
    },
    {
      content: ownChar,
      names: ["perm", "own"],
      exp: boundary.expected.drop,
      act: boundary.actual.drop,
      showit: () => {
        showLoanRegion(facts, data.loan_drop_refined, ["own"]);
        showMoveRegion(facts, data.path_moved, ["own"]);
      },
      hideit: () => {
        hideLoanRegion(facts, data.loan_drop_refined, ["own"]);
        hideMoveRegion(facts, data.path_moved, ["own"]);
      }
    },
    {
      content: flowChar,
      names: ["perm", "flow"],
      exp: boundary.expecting_flow !== undefined,
      act: !(boundary.expecting_flow?.is_violation ?? false),
      showit: () => void null,
      hideit: () => void null
    }
  ];

  let icons = allIcons.filter(i => i.exp);

  // Necessary so we can temporarily apply the CSS filter and remove it
  // at the end of the animation. Sadly no way to do this in pure CSS AFAIK...
  let [animating, setAnimating] = useState(false);
  let [timer, setTimer] = useState<number | undefined>();
  let triggerAnimation = () => {
    setAnimating(true);
    if (timer !== 0) clearTimeout(timer);
    setTimer(window.setTimeout(() => setAnimating(false), 500));
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
      toi(this.boundary.expecting_flow !== undefined)
    ].reduce((a, b) => a + b, 0);
    this.line = view.state.doc.lineAt(
      linecolToPosition(boundary.location, view.state.doc)
    );
  }

  eq(other: BoundaryPointWidget): boolean {
    // TODO
    return this.boundary.location === other.boundary.location;
  }

  toDOM(view: EditorView): HTMLElement {
    let location = linecolToPosition(this.boundary.location, view.state.doc);
    let precedingText = view.state.sliceDoc(location - 1, location);
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
      widget: new BoundaryPointWidget(view, facts, b, annotations)
    }).range(linecolToPosition(b.location, view.state.doc))
  );
}
