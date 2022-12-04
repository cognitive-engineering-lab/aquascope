import { Range, RangeSet, StateEffect, StateField } from "@codemirror/state";
import {
  Decoration,
  DecorationSet,
  EditorView,
  WidgetType,
} from "@codemirror/view";

import { PermDiff, PermissionsStateStep, PermsDiff } from "../types";
import { Icon, IconField, dropChar, readChar, writeChar } from "./misc";

let permissionsDiffIcoType =
  StateEffect.define<Array<ExtendedPermissionStack<PermDiffIco>>>();

type ExtendedPermissionStack<I extends Icon> = [Array<I>, number];
class PermDiffIco implements Icon {
  readonly display: boolean = true;
  constructor(readonly path: string, readonly diffs: PermsDiff) {}

  getAuxiliary(): Array<Range<Decoration>> {
    // TODO
    return [];
  }

  // TODO
  toDom(): HTMLElement {
    let wrap = document.createElement("div");
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
      } else {
        span.textContent = diff.value ? content : "-";
        span.classList.add("perm-diff-none");
      }
    };

    pathSpan.classList.add("perm-step-path");
    pathSpan.textContent = this.path;

    stylePerm(rSpan, this.diffs.read, readChar);
    stylePerm(wSpan, this.diffs.write, writeChar);
    stylePerm(dSpan, this.diffs.drop, dropChar);

    wrap.appendChild(pathSpan);
    wrap.appendChild(rSpan);
    wrap.appendChild(wSpan);
    wrap.appendChild(dSpan);

    return wrap;
  }
}

class StepPermissions<I extends PermDiffIco> extends WidgetType {
  constructor(readonly diffs: Array<I>) {
    super();
  }

  eq(_other: StepPermissions<I>): boolean {
    // TODO
    return false;
  }

  // TODO: rewrite this to use a table instead of the hacky divs
  toDOM(_view: EditorView): HTMLElement {
    let wrap = document.createElement("div");
    let bufferDiv = document.createElement("div");
    wrap.appendChild(bufferDiv);
    this.diffs.forEach(diff => {
      let ico = diff.toDom();
      wrap.appendChild(ico);
    });
    wrap.classList.add("perm-step-stack");
    return wrap;
  }

  ignoreEvent(): boolean {
    return false;
  }
}

let stateStepToPermissions = (
  stateStep: PermissionsStateStep
): ExtendedPermissionStack<PermDiffIco> => {
  let icos = stateStep.state.map(([path, diff]) => {
    return new PermDiffIco(path, diff);
  });
  let loc = stateStep.location.char_end;
  return [icos, loc];
};

// TODO FIXME: collapse this into a simpler more generic function
let permDiffStateField = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update(points, transactions) {
    console.log(transactions);
    for (let e of transactions.effects) {
      if (e.is(permissionsDiffIcoType)) {
        return RangeSet.of(
          // TODO if we need auxiliary then they should be expanded
          // here and flattened.
          e.value.map(([diffs, from]) => {
            return makeDecorationWithDiffs(diffs).range(from);
          }),
          true
        );
      }
    }

    return transactions.docChanged ? RangeSet.of([]) : points;
  },
  provide: f => EditorView.decorations.from(f),
});

let makeDecorationWithDiffs = <I extends PermDiffIco>(
  icos: Array<I>
): Decoration => {
  return Decoration.widget({
    widget: new StepPermissions(icos),
    side: 1,
  });
};

// TODO
export let coarsePermissionDiffs: IconField<
  PermissionsStateStep,
  PermDiffIco,
  ExtendedPermissionStack<PermDiffIco>
> = {
  effectType: permissionsDiffIcoType,
  stateField: permDiffStateField,
  makeDecoration: makeDecorationWithDiffs<PermDiffIco>,
  fromOutput: stateStepToPermissions,
};
