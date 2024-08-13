import type { Range } from "@codemirror/state";
import type { Decoration, EditorView } from "@codemirror/view";
import _ from "lodash";

import type { AnalysisOutput, AquascopeAnnotations } from "../types.js";
import { boundariesField, makeBoundaryDecorations } from "./boundaries.js";
import {
  type ActionFacts,
  generateAnalysisDecorationFacts,
  loanFactsStateType
} from "./misc";
import { makeStepDecorations, stepField } from "./stepper.js";

export interface PermissionsCfg {
  stepper?: any;
  boundaries?: any;
}

export interface PermissionsDecorations {
  stepper: Range<Decoration>[];
  boundaries: Range<Decoration>[];
  facts: ActionFacts[];
}

export function makePermissionsDecorations(
  view: EditorView,
  results: AnalysisOutput[],
  annotations?: AquascopeAnnotations
): PermissionsDecorations {
  let stepDecos: Range<Decoration>[][] = [];
  let boundaryDecos: Range<Decoration>[][] = [];
  let actionDecos: ActionFacts[][] = [];

  results.forEach(res => {
    let [facts, actionFacts] = generateAnalysisDecorationFacts(res);

    let bs = makeBoundaryDecorations(
      view,
      facts,
      res.boundaries,
      annotations?.boundaries
    );
    let ss = makeStepDecorations(view, facts, res.steps, annotations?.stepper);
    boundaryDecos.push(bs);
    stepDecos.push(ss);
    actionDecos.push(actionFacts);
  });

  return {
    stepper: stepDecos.flat(),
    boundaries: _.uniqBy(boundaryDecos.flat(), d => d.from),
    facts: actionDecos.flat()
  };
}

export function renderPermissions(
  view: EditorView,
  decorations?: PermissionsDecorations,
  cfg?: PermissionsCfg
) {
  console.debug("rendering permissions with", cfg);
  if (decorations !== undefined) {
    let useSteps = String(cfg?.stepper) === "true";
    let useBoundaries = String(cfg?.boundaries) === "true";
    view.dispatch({
      effects: [
        loanFactsStateType.of(decorations.facts),
        stepField.setEffect.of(useSteps ? decorations.stepper : []),
        boundariesField.setEffect.of(
          useBoundaries ? decorations.boundaries : []
        )
      ]
    });
  }
}
