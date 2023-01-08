import { LoanKey } from "./bindings/LoanKey";

// Re-export auto-generated types

export { Range } from "./bindings/Range";

export { Permissions } from "./bindings/Permissions";
export { PermissionsBoundary } from "./bindings/PermissionsBoundary";

export { PermissionsBoundaryOutput } from "./bindings/PermissionsBoundaryOutput";
export { PermissionsDiffOutput } from "./bindings/PermissionsDiffOutput";

export { AnalysisOutput } from "./bindings/AnalysisOutput";
export { ValueStep } from "./bindings/ValueStep";
export { LoanPoints } from "./bindings/LoanPoints";
export { LoanKey } from "./bindings/LoanKey";
export { LoanRegions } from "./bindings/LoanRegions";

export { Refiner } from "./bindings/Refiner";
export { RefinementRegion } from "./bindings/RefinementRegion";

export { PermissionsStateStep } from "./bindings/PermissionsStateStep";
export { PermissionsDiff } from "./bindings/PermissionsDiff";
export { PermissionsDataDiff } from "./bindings/PermissionsDataDiff";

export { MMemorySegment } from "./bindings/MMemorySegment";
export { MPathSegment } from "./bindings/MPathSegment";
export { MPath } from "./bindings/MPath";
export { MValue } from "./bindings/MValue";
export { MFrame } from "./bindings/MFrame";
export { MStack } from "./bindings/MStack";
export { MHeap } from "./bindings/MHeap";
export { MStep } from "./bindings/MStep";
export { MHeapAllocKind } from "./bindings/MHeapAllocKind";
export { Abbreviated } from "./bindings/Abbreviated";

interface BuildError {
  type: "BuildError";
  error: string;
}

interface AnalysisError {
  type: "AnalysisError";
  error: string;
}

interface FileNotFound {
  type: "FileNotFound";
}

export type BackendError = BuildError | AnalysisError | FileNotFound;

export interface BackendOutput<T> {
  type: "output";
  value: T;
}

export type BackendResult<T> = BackendOutput<T> | BackendError;

export type AnalysisFacts = {
  loanPoints: Record<LoanKey, string>;
  loanRegions: Record<LoanKey, string>;
};

export let ok = <T>(value: T): BackendResult<T> => ({
  type: "output",
  value,
});

export const is_ok = <T>(res: BackendResult<T>): res is BackendOutput<T> =>
  res.type === "output";
