import { AquascopeError } from "./bindings/AquascopeError";
import { LoanKey } from "./bindings/LoanKey";
import { MoveKey } from "./bindings/MoveKey";

// Re-export auto-generated types

export { Range } from "./bindings/Range";

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

export { RegionFlow } from "./bindings/RegionFlow";
export { RegionViolation } from "./bindings/RegionViolation";
export { RegionViolationKind } from "./bindings/RegionViolationKind";

export { MMemorySegment } from "./bindings/MMemorySegment";
export { MPathSegment } from "./bindings/MPathSegment";
export { MPath } from "./bindings/MPath";
export { MValue } from "./bindings/MValue";
export { MFrame } from "./bindings/MFrame";
export { MStack } from "./bindings/MStack";
export { MHeap } from "./bindings/MHeap";
export { MStep } from "./bindings/MStep";
export { MTrace } from "./bindings/MTrace";
export { MResult } from "./bindings/MResult";
export { MUndefinedBehavior } from "./bindings/MUndefinedBehavior";
export { MHeapAllocKind } from "./bindings/MHeapAllocKind";
export { Abbreviated } from "./bindings/Abbreviated";

export { CharPos } from "./bindings/CharPos";
export { LinePos } from "./bindings/LinePos";
export { StepperAnnotations } from "./bindings/StepperAnnotations";
export { PathMatcher } from "./bindings/PathMatcher";
export { InterpAnnotations } from "./bindings/InterpAnnotations";
export { BoundariesAnnotations } from "./bindings/BoundariesAnnotations";
export { AquascopeAnnotations } from "./bindings/AquascopeAnnotations";

interface FileNotFound {
  type: "FileNotFound";
}

interface StderrMsg {
  type: "ServerStderr";
  error: string;
}

export type BackendError = AquascopeError | StderrMsg | FileNotFound;

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
  value,
});

export const is_ok = <T>(res: BackendResult<T>): res is BackendOutput<T> =>
  res.type === "output";
