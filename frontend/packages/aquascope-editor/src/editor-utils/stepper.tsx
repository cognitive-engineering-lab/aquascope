import { Line, StateEffect } from "@codemirror/state";
import {
  Decoration,
  EditorView,
  ViewPlugin,
  ViewUpdate,
  WidgetType,
} from "@codemirror/view";
import _ from "lodash";
import React from "react";
import ReactDOM from "react-dom/client";

import {
  AnalysisFacts,
  PermissionsDataDiff,
  PermissionsStateStep,
  ValueStep,
} from "../types";
import {
  IconField,
  ReactIcon,
  dropChar,
  genStateField,
  readChar,
  writeChar,
} from "./misc";

export interface StepperConfig {
  focusedCharPos?: number[];
  focusedPaths?: Map<number, string>;
}

function partition<T>(
  predicate: (val: T) => boolean,
  arr: Array<T>
): [Array<T>, Array<T>] {
  const partitioned: [Array<T>, Array<T>] = [[], []];
  arr.forEach((val: T) => {
    const partitionIndex: 0 | 1 = predicate(val) ? 0 : 1;
    partitioned[partitionIndex].push(val);
  });
  return partitioned;
}

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

  let permSpans = perms.map(([diff, content]) =>
    diff.type == "High" ? (
      <span key={content} className="perm-diff-add">
        {content}
      </span>
    ) : diff.type == "Low" ? (
      <span key={content} className="perm-diff-sub">
        {content}
      </span>
    ) : diff.type === "None" && diff.value ? (
      <span key={content} className="perm-diff-none-high">
        {content}
      </span>
    ) : diff.type === "None" && !diff.value ? (
      <span key={content} className="perm-diff-none-low">
        -
      </span>
    ) : null
  );

  return (
    <tr>
      {pathCol}
      <td>{ico}</td>
      <td>{permSpans}</td>
    </tr>
  );
};

let stepLocation = (step: PermissionsStateStep): number => {
  return step.location.char_end;
};

class PermissionStepTableWidget extends WidgetType {
  pathsHidden: boolean;
  tableHidden: boolean;
  line: Line;
  rowHTML?: string;
  constructor(
    readonly view: EditorView,
    readonly config: StepperConfig,
    readonly step: PermissionsStateStep,
    readonly focusedLines: number[],
    readonly focusedPaths: Map<number, RegExp>
  ) {
    super();
    this.pathsHidden = true;
    this.tableHidden = false;
    this.line = view.state.doc.lineAt(stepLocation(step));
  }

  eq(other: PermissionStepTableWidget): boolean {
    // Only one table widget can (should) be on a line.
    return this.line.number === other.line.number;
  }

  toDOM() {
    let container = document.createElement("span");

    let doc = this.view.state.doc;
    let pos = stepLocation(this.step);
    let maxLineLen = 0;

    for (let line of doc.iterLines()) {
      maxLineLen = Math.max(maxLineLen, line.length);
    }
    let currLine = this.line;
    let hideTable = !this.focusedLines.includes(currLine.number);
    this.tableHidden = hideTable;

    let padding = 2 + maxLineLen - currLine.length;
    let spaces = " " + "_".repeat(padding);

    let arrowOut = "»";
    let arrowIn = "«";

    let toggleHiddenPaths = (_e: any) => {
      this.pathsHidden = !this.pathsHidden;
      let content = container.querySelector<HTMLDivElement>(
        ".step-table-content"
      );
      let dropdown = container.querySelector<HTMLDivElement>(
        ".step-table-dropdown"
      );
      if (content) {
        content.style.height = this.pathsHidden ? "0px" : "auto";
      }
      if (dropdown) {
        dropdown.innerHTML = this.pathsHidden ? "..." : "";
      }
    };

    let toggleWidget = (_e: any) => {
      this.tableHidden = !this.tableHidden;
      console.log("toggling widget");
      let contents = container.querySelector<HTMLDivElement>(
        ".step-widget-contents"
      );
      let btn = container.querySelector<HTMLSpanElement>(
        ".step-table-collapse"
      );

      if (contents) {
        contents.style.width = this.tableHidden ? "0px" : "auto";
      }
      if (btn) {
        btn.innerHTML = this.tableHidden ? arrowOut : arrowIn;
      }
    };
    let matchAll = new RegExp("(.*)?");
    let r = this.focusedPaths.get(currLine.number) ?? matchAll;
    let [focusedDiffs, hiddenDiffs] = partition(
      ([path, _]) => !!path.match(r),
      this.step.state
    );

    let hiddenDropdown =
      hiddenDiffs.length > 0 ? (
        <>
          <div className="step-table-dropdown">...</div>
          <div className="step-table-content">
            <table className="perm-step-table">
              {hiddenDiffs.map(([path, diffs]) => (
                <PermDiffRow path={path} diffs={diffs} />
              ))}
            </table>
          </div>
        </>
      ) : null;

    let rowStyle = {
      width: hideTable ? "0px" : "auto",
    };

    ReactDOM.createRoot(container).render(
      <div className="perm-step-widget">
        <span className="step-table-collapse" onClick={toggleWidget}>
          {hideTable ? arrowOut : arrowIn}
        </span>
        <div className="step-widget-contents" style={rowStyle}>
          {spaces}
          <div className="step-table-container" onClick={toggleHiddenPaths}>
            <div className="step-table-parent">
              <table className="perm-step-table">
                {focusedDiffs.map(([path, diffs]) => (
                  <PermDiffRow path={path} diffs={diffs} />
                ))}
              </table>
              {hiddenDropdown}
            </div>
          </div>
        </div>
      </div>
    );

    return container;
  }

  ignoreEvent(): boolean {
    return true;
  }
}

export function renderSteps(
  view: EditorView,
  container: HTMLDivElement,
  stateSteps: PermissionsStateStep[],
  config: StepperConfig
) {
  // Normalize the config
  let doc = view.state.doc;
  let focusedPaths = new Map<number, RegExp>();
  let focusedLines =
    !config.focusedCharPos || config.focusedCharPos.length == 0
      ? Array.from({ length: doc.lines }, (value, key) => key + 1)
      : config.focusedCharPos!.map(pos => doc.lineAt(pos).number);

  // Insert the regex for a specified char pos
  config.focusedPaths?.forEach((re: string, pos: number) => {
    let line = doc.lineAt(pos).number;
    focusedPaths.set(line, new RegExp(re));
  });

  let decos = _.sortBy(
    stateSteps.map(step =>
      Decoration.widget({
        widget: new PermissionStepTableWidget(
          view,
          config,
          step,
          focusedLines,
          focusedPaths
        ),
      }).range(stepLocation(step))
    ),
    deco => deco.from
  );

  let display = true;
  let plugin = ViewPlugin.fromClass(
    class {
      update(upd: ViewUpdate) {
        if (upd.docChanged) {
          display = false;
        }
      }
    },
    {
      decorations: () => {
        if (display) {
          return Decoration.set(decos);
        } else {
          return Decoration.set([]);
        }
      },
    }
  );

  view.dispatch({
    effects: [StateEffect.appendConfig.of(plugin)],
  });
}
