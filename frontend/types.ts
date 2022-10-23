// Re-export auto-generated types

export { AquascopeError } from "./interface/AquascopeError";
export { CallTypes } from "./interface/CallTypes";
export { Range } from "./interface/Range";
export { ReceiverTypesOutput as ReceiverTypes } from "./interface/ReceiverTypes";
export { TypeInfo } from "./interface/TypeInfo";
export { TypeState } from "./interface/TypeState";

interface BuildError {
  type: "BuildError";
  error: string;
}
interface AnalysisError {
  type: "AnalysisError";
  error: string;
}
interface FileNotFound {
  type: "FileNotFound"
}
export type BackendError = BuildError | AnalysisError | FileNotFound;

interface BackendOutput<T> {
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
