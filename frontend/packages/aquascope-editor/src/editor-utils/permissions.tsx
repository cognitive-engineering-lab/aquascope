import { Range } from "@codemirror/state";
import { Decoration, EditorView } from "@codemirror/view";
import _ from "lodash";

import { AnalysisOutput, AquascopeAnnotations } from "../types";
import { boundaryEffect, makeBoundaryDecorations } from "./boundaries";
import {
  ActionFacts,
  generateAnalysisDecorationFacts,
  loanFactsStateType,
} from "./misc";
import { makeStepDecorations, stepEffect } from "./stepper";

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

    let bs = makeBoundaryDecorations(view, facts, res.boundaries);
    let ss = makeStepDecorations(view, facts, res.steps, annotations?.stepper);
    boundaryDecos.push(bs);
    stepDecos.push(ss);
    actionDecos.push(actionFacts);
  });

  return {
    stepper: stepDecos.flat(),
    boundaries: _.uniqBy(boundaryDecos.flat(), d => d.from),
    facts: actionDecos.flat(),
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
        stepEffect.of(useSteps ? decorations.stepper : []),
        boundaryEffect.of(useBoundaries ? decorations.boundaries : []),
      ],
    });
  }
}
