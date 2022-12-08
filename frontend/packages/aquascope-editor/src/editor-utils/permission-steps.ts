import { Range, StateEffect } from "@codemirror/state";
import { Decoration, EditorView, WidgetType } from "@codemirror/view";

import { PermDiff, PermissionsStateStep, PermsDiff } from "../types";
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

class PermDiffRowIcon implements Icon {
  readonly display: boolean = true;
  constructor(readonly path: string, readonly diffs: PermsDiff) {}

  getAuxiliary(): Array<Range<Decoration>> {
    return [];
  }

  // A single permission step <path: changes> is represented as a single table row.
  toDom(): HTMLElement {
    let row = document.createElement("tr");
    let pathC = document.createElement("td");
    let permC = document.createElement("td");

    let pathSpan = document.createElement("span");
    let rSpan = document.createElement("span");
    let wSpan = document.createElement("span");
    let dSpan = document.createElement("span");

    let stylePerm = (span: HTMLElement, diff: PermDiff, content: string) => {
      if (diff.type === "Add") {
        span.textContent = "+" + content;
        span.classList.add("perm-diff-add");
      } else if (diff.type === "Sub") {
        span.textContent = "-" + content;
        span.classList.add("perm-diff-sub");
      }
      // Currently we only display the *differences* so we'll skip the None case.
    };

    pathSpan.classList.add("perm-step-path");
    pathSpan.textContent = this.path;

    stylePerm(rSpan, this.diffs.read, readChar);
    stylePerm(wSpan, this.diffs.write, writeChar);
    stylePerm(dSpan, this.diffs.drop, dropChar);

    pathC.appendChild(pathSpan);

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
  (ts: Array<PermissionStepTable>) => {
    // let maxLineLen = 0;
    // for (let line of _view.state.doc.iterLines()) {
    //   maxLineLen = Math.max(maxLineLen, line.length);
    // }
    // let padding = this

    return ts.map(([diffs, from]) => {
      return makeDecorationWithDiffs(diffs, from).range(from);
    });
  }
);

let makeDecorationWithDiffs = (
  icos: Array<PermDiffRowIcon>,
  pos: number
): Decoration => {
  return Decoration.widget({
    widget: new PermissionStepTableWidget(icos, pos),
    side: 1,
  });
};

export let coarsePermissionDiffs: IconField<
  PermissionsStateStep,
  PermDiffRowIcon,
  PermissionStepTable
> = {
  effectType: permissionsDiffIcoType,
  stateField: permDiffStateField,
  makeDecoration: makeDecorationWithDiffs,
  fromOutput: stateStepToPermissions,
};