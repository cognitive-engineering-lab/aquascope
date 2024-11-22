import type { AquascopeError } from "./bindings/AquascopeError";
import type { CharRange } from "./bindings/CharRange";
import type { LoanKey } from "./bindings/LoanKey";
import type { MTrace } from "./bindings/MTrace";
import type { MoveKey } from "./bindings/MoveKey";
import type { AnalysisOutput } from "./lib";

// Re-export auto-generated types

export { CharRange } from "./bindings/CharRange";
export { CharPos } from "./bindings/CharPos";

export { Permissions } from "./bindings/Permissions";
export { PermissionsBoundary } from "./bindings/PermissionsBoundary";

export { AquascopeError } from "./bindings/AquascopeError";
export { AnalysisOutput } from "./bindings/AnalysisOutput";
export { ValueStep } from "./bindings/ValueStep";

export { LoanKey } from "./bindings/LoanKey";
export { LoanPoints } from "./bindings/LoanPoints";
export { LoanRegions } from "./bindings/LoanRegions";

export { MoveKey } from "./bindings/MoveKey";
export { MovePoints } from "./bindings/MovePoints";
export { MoveRegions } from "./bindings/MoveRegions";

export { Refiner } from "./bindings/Refiner";
export { RefinementRegion } from "./bindings/RefinementRegion";

export { PermissionsLineDisplay } from "./bindings/PermissionsLineDisplay";
export { PermissionsStepTable } from "./bindings/PermissionsStepTable";
export { PermissionsDataDiff } from "./bindings/PermissionsDataDiff";
export { PermissionsDiff } from "./bindings/PermissionsDiff";

export { MMemorySegment } from "./bindings/MMemorySegment";
export { MPathSegment } from "./bindings/MPathSegment";
export { MPath } from "./bindings/MPath";
export { MValue } from "./bindings/MValue";
export { MLocal } from "./bindings/MLocal";
export { MFrame } from "./bindings/MFrame";
export { MStack } from "./bindings/MStack";
export { MHeap } from "./bindings/MHeap";
export { MStep } from "./bindings/MStep";
export { MTrace } from "./bindings/MTrace";
export { MResult } from "./bindings/MResult";
export { MUndefinedBehavior } from "./bindings/MUndefinedBehavior";
export { MHeapAllocKind } from "./bindings/MHeapAllocKind";
export { Abbreviated } from "./bindings/Abbreviated";

export { StepperAnnotations } from "./bindings/StepperAnnotations";
export { PathMatcher } from "./bindings/PathMatcher";
export { InterpAnnotations } from "./bindings/InterpAnnotations";
export { BoundariesAnnotations } from "./bindings/BoundariesAnnotations";
export { AquascopeAnnotations } from "./bindings/AquascopeAnnotations";

export interface StderrMsg {
  type: "ServerStderr";
  error: string;
}

export type BackendError = AquascopeError | StderrMsg;

export interface BackendOutput<T> {
  type: "output";
  value: T;
}

export type BackendResult<T> = BackendOutput<T> | BackendError;

export type AnalysisFacts = {
  loanPoints: Record<LoanKey, string>;
  loanRegions: Record<LoanKey, string>;
  movePoints: Record<MoveKey, string>;
  moveRegions: Record<MoveKey, string>;
};

export let ok = <T>(value: T): BackendResult<T> => ({
  type: "output",
  value
});

export const is_ok = <T>(res: BackendResult<T>): res is BackendOutput<T> =>
  res.type === "output";

export type AquascopePluginArgs = {
  should_fail: boolean;
  command: AquascopeCommand;
};

export type PermIncludeMode = "Changes" | "All";

export type AquascopeCommand =
  | {
      Permissions: {
        steps_include_mode: PermIncludeMode | null;
        show_flows: boolean;
      };
    }
  | "Interpreter";

export type Result<T, E> = { Ok: T } | { Err: E };

export type Command = "interpreter" | "permissions";

export function isInterp(c: Command): c is "interpreter" {
  return c === "interpreter";
}
export function isPerm(c: Command): c is "permissions" {
  return c === "permissions";
}

export type CommandResult<C extends Command> = C extends "interpreter"
  ? Result<MTrace<CharRange>, BackendError>
  : C extends "permissions"
    ? Result<AnalysisOutput, BackendError>[]
    : never;

export type Maybe<T> = T | undefined;
