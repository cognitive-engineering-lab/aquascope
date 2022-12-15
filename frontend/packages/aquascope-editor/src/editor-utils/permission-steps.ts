import { Range, StateEffect } from "@codemirror/state";
import {
  Decoration,
  EditorView,
  WidgetType,
  hoverTooltip,
} from "@codemirror/view";

import { BoolStep, PermissionsDataDiff, PermissionsStateStep } from "../types";
import {
  Icon,
  IconField,
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
class PermDiffRowIcon implements Icon {
  readonly display: boolean = true;
  constructor(readonly path: string, readonly diffs: PermissionsDataDiff) {}

  getAuxiliary(): Array<Range<Decoration>> {
    return [];
  }

  // A single permission step <path: changes> is represented as a single table row.
  toDom(): HTMLElement {
    let row = document.createElement("tr");
    let pathC = document.createElement("td");
    let permC = document.createElement("td");

    let pathSpan = document.createElement("span");
    let actionIcos = document.createElement("div");
    let rSpan = document.createElement("span");
    let wSpan = document.createElement("span");
    let dSpan = document.createElement("span");

    let stylePerm = (span: HTMLElement, diff: BoolStep, content: string) => {
      if (diff.type === "High") {
        span.textContent = "+" + content;
        span.classList.add("perm-diff-add");
      } else if (diff.type === "Low") {
        span.textContent = "-" + content;
        span.classList.add("perm-diff-sub");
      }
      // Currently we only display the *differences* so we'll skip the None case.
    };

    let addActionIcons = (div: HTMLElement, data: PermissionsDataDiff) => {
      let pushIcon = (name: string) => {
        let icon = document.createElement("i");
        icon.classList.add("fa-solid");
        icon.classList.add(name);
        icon.classList.add("aquascope-action-indicator");
        div.appendChild(icon);
      };
      // There is a sort of hierarchy to the changing permissions:
      // 1. path liveness is most important. If it changes, this radically
      //    has an affect on everything.
      // 2. A path getting moved is the second highest. Again, a moved path
      //    cannot be borrowed so it sort of trumps any loan refinements.
      // 3. At the bottom, loan refinement changes which can only have an
      //    affect if the prior two didn't change anything.
      if (data.is_live.type === "Low") {
        pushIcon("fa-skull");
      } else if (data.is_live.type === "High") {
        pushIcon("fa-cake-candles");
      } else if (data.path_moved.type === "High") {
        pushIcon("fa-bicycle");
      } else if (data.path_moved.type === "Low") {
        pushIcon("fa-recycle");
      } else if (data.loan_write_refined.type === "High") {
        pushIcon("fa-share");
      } else if (data.loan_read_refined.type === "High") {
        pushIcon("fa-share");
      } else if (data.loan_write_refined.type === "Low") {
        // I wanted to use share-all and reply-all for mutable
        // loans but share-all is a pro icon :(
        // pushIcon("fa-reply-all");
        pushIcon("fa-reply");
      } else if (data.loan_read_refined.type === "Low") {
        pushIcon("fa-reply");
      }
    };

    pathSpan.classList.add("perm-step-path");
    pathSpan.textContent = this.path;

    stylePerm(rSpan, this.diffs.permissions.read, readChar);
    stylePerm(wSpan, this.diffs.permissions.write, writeChar);
    stylePerm(dSpan, this.diffs.permissions.drop, dropChar);

    addActionIcons(actionIcos, this.diffs);

    pathC.appendChild(pathSpan);
    pathC.appendChild(actionIcos);

    permC.appendChild(rSpan);
    permC.appendChild(wSpan);
    permC.appendChild(dSpan);

    row.appendChild(pathC);
    row.appendChild(permC);

    return row;
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
    div.append(" " + "―".repeat(padding));

    let table = document.createElement("table");
    this.diffs.forEach(diff => {
      let ico = diff.toDom();
      table.appendChild(ico);
    });
    table.classList.add("perm-step-table");
    div.appendChild(table);

    return div;
  }

  ignoreEvent(): boolean {
    return false;
  }
}

let stateStepToPermissions = (
  stateStep: PermissionsStateStep
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
    ts.map(([diffs, from]) => makeDecorationWithDiffs(diffs, from).range(from))
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
