import { Range } from "@codemirror/state";
import { Decoration, EditorView } from "@codemirror/view";

import { AnalysisOutput, AquascopeAnnotations } from "../types";
import { boundaryEffect, makeBoundaryDecorations } from "./boundaries";
import {
  LoanFacts,
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
  facts: LoanFacts[];
}

export function makePermissionsDecorations(
  view: EditorView,
  results: AnalysisOutput[],
  annotations?: AquascopeAnnotations
): PermissionsDecorations {
  let stepDecos: Range<Decoration>[][] = [];
  let boundaryDecos: Range<Decoration>[][] = [];
  let loanDecos: LoanFacts[][] = [];

  results.forEach(res => {
    let [facts, loanFacts] = generateAnalysisDecorationFacts(res);

    let bs = makeBoundaryDecorations(
      view,
      facts,
      res.boundaries,
      annotations?.boundaries
    );
    let ss = makeStepDecorations(view, facts, res.steps, annotations?.stepper);
    boundaryDecos.push(bs);
    stepDecos.push(ss);
    loanDecos.push(loanFacts);
  });

  return {
    stepper: stepDecos.flat(),
    boundaries: boundaryDecos.flat(),
    facts: loanDecos.flat(),
  };
}

export function renderPermissions(
  view: EditorView,
  decorations?: PermissionsDecorations,
  cfg?: PermissionsCfg
) {
  console.log("rendering permissions with", cfg);
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
