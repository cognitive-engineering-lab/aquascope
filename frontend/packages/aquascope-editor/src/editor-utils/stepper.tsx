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
import React, { useState } from "react";
import ReactDOM from "react-dom/client";

import {
  AnalysisFacts,
  LoanKey,
  MoveKey,
  PermissionsDataDiff,
  PermissionsLineDisplay,
  PermissionsStepTable,
  StepperAnnotations,
  ValueStep,
} from "../types";
import {
  dropChar,
  hideLoanRegion,
  hideMoveRegion,
  readChar,
  showLoanRegion,
  showMoveRegion,
  writeChar,
} from "./misc";

let PermChar = ({
  content,
  names,
  act,
  x,
  y,
  showit,
  hideit,
}: {
  content: string;
  names: string[];
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

let PermRow = ({
  content,
  facts,
}: {
  content: {
    diff: ValueStep<boolean>;
    content: string;
    lk: LoanKey | undefined;
    mk: MoveKey | undefined;
  }[];
  facts: AnalysisFacts;
}) => {
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

  // FIXME: don't reverse the abbreviated content.
  let getKind = (c: string) => {
    if (c === "R") {
      return "read";
    } else if (c === "W") {
      return "write";
    } else if (c == "O") {
      return "drop";
    } else {
      return "unknown";
    }
  };

  return (
    <svg xmlns="http://www.w3.org/2000/svg" className="permission-row">
      {content.map(({ diff, content, lk, mk }, i: number) => (
        <PermChar
          key={content}
          x={w(i)}
          y="95%"
          act={true}
          showit={() => {
            showLoanRegion(facts, lk, [getKind(content)]);
            showMoveRegion(facts, mk, [getKind(content)]);
          }}
          hideit={() => {
            hideLoanRegion(facts, lk, [getKind(content)]);
            hideMoveRegion(facts, mk, [getKind(content)]);
          }}
          {...getClassAndContent([diff, content])!}
        />
      ))}
    </svg>
  );
};

let PermDiffRow = ({
  path,
  diffs,
  facts,
}: {
  path: string;
  diffs: PermissionsDataDiff;
  facts: AnalysisFacts;
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
    | "path_uninitialized"
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
          value: { type: "High", value: 0 },
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
          value: { type: "High", value: 0 },
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
      fact: "path_uninitialized",
      states: [
        {
          value: { type: "High", value: 0 },
          icon: "sign-out",
          desc: "Path contains uninitialized data",
        },
        {
          value: { type: "Low" },
          icon: "recycle",
          desc: "Path data is initialized after move here",
        },
      ],
    },
    {
      fact: "loan_read_refined",
      states: [
        {
          value: { type: "High", value: 0 },
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
          value: { type: "High", value: 0 },
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
      if (_.isEqual(diffs[fact].type, value.type)) {
        ico = (
          <i
            className={`fa fa-${icon} aquascope-action-indicator`}
            title={desc}
          />
        );
        break loop;
      } else {
        console.log("unequal: ", diffs[fact].type, value.type);
      }
    }
  }

  let unwrap = function <T>(v: ValueStep<T>): T | undefined {
    return v.type === "None" || v.type === "High" ? v.value : undefined;
  };

  let perms = [
    {
      diff: diffs.permissions.read,
      content: readChar,
      lk: unwrap(diffs.loan_read_refined),
      mk: unwrap(diffs.path_moved),
    },
    {
      diff: diffs.permissions.write,
      content: writeChar,
      lk: unwrap(diffs.loan_write_refined),
      mk: unwrap(diffs.path_moved),
    },
    {
      diff: diffs.permissions.drop,
      content: dropChar,
      lk: unwrap(diffs.loan_drop_refined),
      mk: unwrap(diffs.path_moved),
    },
  ];

  let pathCol = <td className="perm-step-path">{path}</td>;

  return (
    <tr>
      {pathCol}
      <td>{ico}</td>
      <td>
        <PermRow facts={facts} content={perms} />
      </td>
    </tr>
  );
};

let stepLocation = (step: PermissionsLineDisplay): number => {
  return step.location.char_end;
};

let StepTable = ({
  rows,
  facts,
}: {
  rows: [string, PermissionsDataDiff][];
  facts: AnalysisFacts;
}) => (
  <table className="perm-step-table">
    <tbody>
      {rows.map(([path, diffs], i: number) => (
        <PermDiffRow key={i} path={path} diffs={diffs} facts={facts} />
      ))}
    </tbody>
  </table>
);

// An individual table goes inside of the overall container, and
// contains its own dropdown based on focused / unfocused places.
let StepTableIndividual = ({
  focused,
  hidden,
  facts,
}: {
  focused: [string, PermissionsDataDiff][];
  hidden: [string, PermissionsDataDiff][];
  facts: AnalysisFacts;
}): JSX.Element => {
  let [displayAll, setDisplayAll] = useState(false);

  let hiddenDropdown =
    hidden.length > 0 ? (
      <>
        <div className="step-table-dropdown step-widget-toggle">● ● ●</div>
        <div className={classNames({ "hidden-height": !displayAll })}>
          <StepTable rows={hidden} facts={facts} />
        </div>
      </>
    ) : null;

  return (
    <div
      className="step-table-container"
      onClick={() => setDisplayAll(!displayAll)}
    >
      <StepTable rows={focused} facts={facts} />
      {hiddenDropdown}
    </div>
  );
};

// On a single line there can be multiple tables next to each other.
// All tables go in a container that can be collapsed to the left.
let StepLine = ({
  spaces,
  focusedRegex,
  tables,
  init,
  facts,
}: {
  spaces: string;
  focusedRegex: RegExp;
  tables: PermissionsStepTable[];
  init: boolean;
  facts: AnalysisFacts;
}): JSX.Element => {
  let [display, setDisplay] = useState(init);
  let arrowOut = "»";
  let arrowIn = "«";

  let together = tables.map((table, i) => {
    let [focusedDiffs, hiddenDiffs] = _.partition(
      table.state,
      ([path, _]) => !!path.match(focusedRegex)
    );

    return (
      <StepTableIndividual
        key={i}
        focused={focusedDiffs}
        hidden={hiddenDiffs}
        facts={facts}
      />
    );
  });

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
        {together}
      </div>
    </div>
  );
};

class PermissionStepLineWidget extends WidgetType {
  line: Line;
  rowHTML?: string;
  constructor(
    readonly view: EditorView,
    readonly step: PermissionsLineDisplay,
    readonly facts: AnalysisFacts,
    readonly annotations?: StepperAnnotations
  ) {
    super();
    this.line = view.state.doc.lineAt(stepLocation(step));
  }

  eq(other: PermissionStepLineWidget): boolean {
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

    let tables = this.step.state;

    ReactDOM.createRoot(container).render(
      <StepLine
        spaces={spaces}
        focusedRegex={r}
        tables={tables}
        init={initDisplay}
        facts={this.facts}
      />
    );

    return container;
  }

  ignoreEvent(): boolean {
    return true;
  }
}

export let stepEffect = StateEffect.define<Range<Decoration>[]>();

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

export function makeStepDecorations(
  view: EditorView,
  facts: AnalysisFacts,
  stateSteps: PermissionsLineDisplay[],
  annotations?: StepperAnnotations
): Range<Decoration>[] {
  return _.sortBy(
    stateSteps.map(step =>
      Decoration.widget({
        widget: new PermissionStepLineWidget(view, step, facts, annotations),
      }).range(stepLocation(step))
    ),
    deco => deco.from
  );
}
