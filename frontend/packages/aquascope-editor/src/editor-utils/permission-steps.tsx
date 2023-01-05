import { Range, StateEffect } from "@codemirror/state";
import { Decoration, EditorView, WidgetType } from "@codemirror/view";
import _ from "lodash";
import React from "react";
import ReactDOMServer from "react-dom/server";

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

let permissionsDiffIcoType = StateEffect.define<Array<PermissionStepTable>>();

// For each permission step table we need a
// list of rows and the editor location where it will be placed.
type PermissionStepTable = [Array<PermDiffRowIcon>, number];

// TODO(gavinleroy): the frontend is currently receiving more data than initially designed.
// That is, it receives additional data about how the influencers of a permission are chaning.
// This of course is so that we can display cute icons for specific changes (the received data needs
// to be expanded a little more), but this has introduced the unsavory problem of the
// frontend having logic to compute the permissions changes based on the factors changes.
// The frontend should *not* have to do this and the backend should additionally send
// that information so it can be simply read.
class PermDiffRowIcon implements ReactIcon {
  readonly display: boolean = true;
  constructor(readonly path: string, readonly diffs: PermissionsDataDiff) {}

  getAuxiliary(): Array<Range<Decoration>> {
    return [];
  }

  IcoCol = (): JSX.Element => {
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
        if (_.isEqual(this.diffs[fact], value)) {
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

    return <td>{ico}</td>;
  };

  PermCol = (): JSX.Element => {
    let perms: [ValueStep<boolean>, string][] = [
      [this.diffs.permissions.read, readChar],
      [this.diffs.permissions.write, writeChar],
      [this.diffs.permissions.drop, dropChar],
    ];
    return (
      <td>
        {perms.map(([diff, content]) =>
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
        )}
      </td>
    );
  };

  // A single permission step <path: changes> is represented as a single table row.
  render(): JSX.Element {
    let pathCol = <td className="perm-step-path">{this.path}</td>;

    return (
      <tr>
        {pathCol}
        <this.IcoCol />
        <this.PermCol />
      </tr>
    );
  }
}

class PermissionStepTableWidget extends WidgetType {
  constructor(readonly diffs: Array<PermDiffRowIcon>, readonly pos: number) {
    super();
  }

  eq(_other: PermissionStepTableWidget): boolean {
    // TODO
    return false;
  }

  toDOM(view: EditorView): HTMLElement {
    let div = document.createElement("div");
    div.className = "perm-step-widget";

    let doc = view.state.doc;
    let maxLineLen = 0;
    for (let line of doc.iterLines()) {
      maxLineLen = Math.max(maxLineLen, line.length);
    }
    let padding = 2 + maxLineLen - doc.lineAt(this.pos).length;
    let spaces = " " + "―".repeat(padding);

    let table = (
      <table className="perm-step-table">
        {this.diffs.map((diff, i) => diff.render())}
      </table>
    );
    let inner = (
      <>
        {spaces}
        {table}
      </>
    );
    div.innerHTML = ReactDOMServer.renderToStaticMarkup(inner);

    return div;
  }

  ignoreEvent(): boolean {
    return false;
  }
}

let stateStepToPermissions = (
  stateStep: PermissionsStateStep,
  _facts: AnalysisFacts
): PermissionStepTable => {
  let icos = stateStep.state.map(([path, diff]) => {
    return new PermDiffRowIcon(path, diff);
  });
  let loc = stateStep.location.char_end;
  return [icos, loc];
};

let permDiffStateField = genStateField(
  permissionsDiffIcoType,
  (ts: Array<PermissionStepTable>) =>
    ts.map(([diffs, pos]) => makeDecorationWithDiffs(diffs, pos).range(pos))
);

let makeDecorationWithDiffs = (
  icos: Array<PermDiffRowIcon>,
  pos: number
): Decoration =>
  Decoration.widget({
    widget: new PermissionStepTableWidget(icos, pos),
    side: 1,
  });

// export const killedVarsHover = hoverTooltip((view, pos, side) => {
//   let activeSkulls = Array.from(
//     document.querySelectorAll<HTMLElement>(".aquascope-kill-indicator")
//   );

//   let sPos = pos.toString();
//   let hovered = activeSkulls.find(s => s.dataset.bufferPos == sPos);
//   if (hovered == undefined || hovered == null) {
//     return null;
//   }

//   let killedList = JSON.parse(hovered.dataset.killedVars!);
//   console.log("Kill list includes", killedList);
//   return {
//     pos: pos,
//     above: true,
//     arrow: true,
//     create(view) {
//       let dom = document.createElement("div");
//       dom.textContent = killedList;
//       dom.classList.add("cm-tooltip-cursor");
//       return { dom };
//     },
//   };
// });

export const coarsePermissionDiffs: IconField<
  PermissionsStateStep,
  PermissionStepTable
> = {
  effectType: permissionsDiffIcoType,
  stateField: permDiffStateField,
  fromOutput: stateStepToPermissions,
};
