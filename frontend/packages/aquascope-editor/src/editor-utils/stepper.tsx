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
  ViewPlugin,
  ViewUpdate,
  WidgetType,
} from "@codemirror/view";
import classNames from "classnames";
import _ from "lodash";
import React, { useState } from "react";
import ReactDOM from "react-dom/client";

import {
  PermissionsDataDiff,
  PermissionsStateStep,
  StepperAnnotations,
  ValueStep,
} from "../types";
import { dropChar, readChar, writeChar } from "./misc";

let PermChar = ({
  content,
  names,
  exp,
  act,
  x,
  y,
  showit,
  hideit,
}: {
  content: string;
  names: string[];
  exp: boolean;
  act: boolean;
  x: string;
  y: string;
  showit: () => void;
  hideit: () => void;
}) => (
  <text
    className={classNames(...names, { missing: !act })}
    textAnchor="end"
    x={x}
    y={y}
    onMouseEnter={showit}
    onMouseLeave={hideit}
  >
    {content}
  </text>
);

let PermRow = ({ content }: { content: [ValueStep<boolean>, string][] }) => {
  let getClassAndContent = ([diff, content]: [ValueStep<boolean>, string]) =>
    diff.type == "High"
      ? { content: content, names: ["perm-diff-add"] }
      : diff.type == "Low"
      ? { content: content, names: ["perm-diff-sub"] }
      : diff.type === "None" && diff.value
      ? { content: content, names: ["perm-diff-none-high"] }
      : diff.type === "None" && !diff.value
      ? { content: "‒", names: ["perm-diff-none-low"] }
      : null;

  let w = (idx: number) =>
    (idx / content.length) * 100 + 100 / content.length - 5 + "%";
  let nullF = () => {
    return;
  };

  return (
    <svg xmlns="http://www.w3.org/2000/svg" className="permission-row">
      {content.map(([diff, content], i: number) => (
        <PermChar
          key={content}
          x={w(i)}
          y="95%"
          exp={true}
          act={true}
          showit={nullF}
          hideit={nullF}
          {...getClassAndContent([diff, content])!}
        />
      ))}
    </svg>
  );
};

let PermDiffRow = ({
  path,
  diffs,
}: {
  path: string;
  diffs: PermissionsDataDiff;
}) => {
  interface VisualFact<K extends keyof PermissionsDataDiff> {
    fact: K;
    states: VisualFactState<K>[];
  }

  interface VisualFactState<K extends keyof PermissionsDataDiff> {
    value: PermissionsDataDiff[K];
    icon: string;
    desc: string;
  }

  type Facts =
    | "is_live"
    | "path_moved"
    | "loan_write_refined"
    | "loan_read_refined";

  // There is a sort of hierarchy to the changing permissions:
  // 1. path liveness is most important. If it changes, this radically
  //    has an affect on everything.
  // 2. A path getting moved is the second highest. Again, a moved path
  //    cannot be borrowed so it sort of trumps any loan refinements.
  // 3. At the bottom, loan refinement changes which can only have an
  //    affect if the prior two didn't change anything.

  let visualFacts: VisualFact<Facts>[] = [
    {
      fact: "is_live",
      states: [
        {
          value: { type: "High" },
          icon: "level-up",
          desc: "Path is initialized here",
        },
        {
          value: { type: "Low" },
          icon: "level-down",
          desc: "Path is no longer used here",
        },
      ],
    },
    {
      fact: "path_moved",
      states: [
        {
          value: { type: "High" },
          icon: "sign-out",
          desc: "Path is moved here",
        },
        {
          value: { type: "Low" },
          icon: "recycle",
          desc: "Path is re-initialized after move here",
        },
      ],
    },
    {
      fact: "loan_read_refined",
      states: [
        {
          value: { type: "High" },
          icon: "arrow-right",
          desc: "Path is borrowed here",
        },
        {
          value: { type: "Low" },
          icon: "rotate-left",
          desc: "Borrow on path is dropped here",
        },
      ],
    },
    {
      fact: "loan_write_refined",
      states: [
        {
          value: { type: "High" },
          icon: "arrow-right",
          desc: "Path is borrowed here",
        },
        {
          value: { type: "Low" },
          icon: "rotate-left",
          desc: "Borrow on path is no longer used here",
        },
      ],
    },
  ];

  let ico = null;
  loop: for (let { fact, states } of visualFacts) {
    for (let { value, icon, desc } of states) {
      if (_.isEqual(diffs[fact], value)) {
        ico = (
          <i
            className={`fa fa-${icon} aquascope-action-indicator`}
            title={desc}
          />
        );
        break loop;
      }
    }
  }

  let perms: [ValueStep<boolean>, string][] = [
    [diffs.permissions.read, readChar],
    [diffs.permissions.write, writeChar],
    [diffs.permissions.drop, dropChar],
  ];

  let pathCol = <td className="perm-step-path">{path}</td>;

  return (
    <tr>
      {pathCol}
      <td>{ico}</td>
      <td>
        <PermRow content={perms} />
      </td>
    </tr>
  );
};

let stepLocation = (step: PermissionsStateStep): number => {
  return step.location.char_end;
};

let StepTable = ({ rows }: { rows: [string, PermissionsDataDiff][] }) => (
  <table className="perm-step-table">
    {rows.map(([path, diffs], i: number) => (
      <PermDiffRow key={i} path={path} diffs={diffs} />
    ))}
  </table>
);

let StepTableWidget = ({
  spaces,
  focused,
  hidden,
  init,
}: {
  spaces: string;
  focused: [string, PermissionsDataDiff][];
  hidden: [string, PermissionsDataDiff][];
  init: boolean;
}): JSX.Element => {
  let [display, setDisplay] = useState(init);
  let [displayAll, setDisplayAll] = useState(false);
  let arrowOut = "»";
  let arrowIn = "«";

  let hiddenDropdown =
    hidden.length > 0 ? (
      <>
        <div className="step-table-dropdown step-widget-toggle">● ● ●</div>
        <div className={classNames({ "hidden-height": !displayAll })}>
          <StepTable rows={hidden} />
        </div>
      </>
    ) : null;

  return (
    <div className="perm-step-widget">
      {" "}
      <span className="step-widget-toggle" onClick={() => setDisplay(!display)}>
        {display ? arrowIn : arrowOut}
      </span>
      <div
        className={classNames("step-widget-container", {
          "hidden-width": !display,
        })}
      >
        {spaces}
        <div
          className="step-table-container"
          onClick={() => setDisplayAll(!displayAll)}
        >
          <StepTable rows={focused} />
          {hiddenDropdown}
        </div>
      </div>
    </div>
  );
};

class PermissionStepTableWidget extends WidgetType {
  line: Line;
  rowHTML?: string;
  constructor(
    readonly view: EditorView,
    readonly step: PermissionsStateStep,
    readonly annotations?: StepperAnnotations
  ) {
    super();
    this.line = view.state.doc.lineAt(stepLocation(step));
  }

  eq(other: PermissionStepTableWidget): boolean {
    // Only one table widget can (should) be on a line.
    return this.line.number === other.line.number;
  }

  toDOM() {
    let container = document.createElement("span");

    let doc = this.view.state.doc;
    let currLine = this.line;
    let initDisplay =
      this.annotations && this.annotations.focused_lines.length > 0
        ? this.annotations.focused_lines.includes(currLine.number)
        : true;
    let maxLineLen = 0;

    for (let line of doc.iterLines()) {
      maxLineLen = Math.max(maxLineLen, line.length);
    }

    let padding = 2 + maxLineLen - currLine.length;
    let spaces = "―".repeat(padding);

    let r = new RegExp(
      this.annotations?.focused_paths[currLine.number] ?? "(.*)?"
    );
    console.log(`regex for line ${currLine.number} :: ${r}`);
    let [focusedDiffs, hiddenDiffs] = _.partition(
      this.step.state,
      ([path, _]) => !!path.match(r)
    );

    ReactDOM.createRoot(container).render(
      <StepTableWidget
        spaces={spaces}
        focused={focusedDiffs}
        hidden={hiddenDiffs}
        init={initDisplay}
      />
    );

    return container;
  }

  ignoreEvent(): boolean {
    return true;
  }
}

let stepEffect = StateEffect.define<Range<Decoration>[]>();

export let stepField = StateField.define<DecorationSet>({
  create: () => Decoration.none,

  update(values, trs) {
    for (let e of trs.effects) {
      if (e.is(stepEffect)) {
        return RangeSet.of(e.value, true);
      }
    }

    return trs.docChanged ? RangeSet.of([]) : values;
  },

  provide: f => EditorView.decorations.from(f),
});

export function renderSteps(
  view: EditorView,
  stateSteps: PermissionsStateStep[],
  annotations?: StepperAnnotations
) {
  let decos = _.sortBy(
    stateSteps.map(step =>
      Decoration.widget({
        widget: new PermissionStepTableWidget(view, step, annotations),
      }).range(stepLocation(step))
    ),
    deco => deco.from
  );

  view.dispatch({
    effects: [stepEffect.of(decos)],
  });
}
