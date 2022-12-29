// Re-export auto-generated types

export { AquascopeError } from "./bindings/AquascopeError";
export { Range } from "./bindings/Range";

export { Permissions } from "./bindings/Permissions";
export { PermissionsBoundary } from "./bindings/PermissionsBoundary";

export { PermissionsBoundaryOutput } from "./bindings/PermissionsBoundaryOutput";
export { PermissionsDiffOutput } from "./bindings/PermissionsDiffOutput";

export { Refiner } from "./bindings/Refiner";
export { RefinementInfo } from "./bindings/RefinementInfo";
export { RefinementRegion } from "./bindings/RefinementRegion";

export { MissingPermsInfo } from "./bindings/MissingPermsInfo";
export { MissingPermReason } from "./bindings/MissingPermReason";

export { PermissionsStateStep } from "./bindings/PermissionsStateStep";
export { PermissionsDiff } from "./bindings/PermissionsDiff";
export { PermissionsDataDiff } from "./bindings/PermissionsDataDiff";
export { BoolStep } from "./bindings/BoolStep";

export { MValue } from "./bindings/MValue";
export { MFrame } from "./bindings/MFrame";
export { MStack } from "./bindings/MStack";
export { MHeap } from "./bindings/MHeap";
export { MStep } from "./bindings/MStep";

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

export let ok = <T>(value: T): BackendResult<T> => ({
  type: "output",
  value,
});

export const is_ok = <T>(res: BackendResult<T>): res is BackendOutput<T> =>
  res.type === "output";
