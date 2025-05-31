import type { LeaderLine } from "./bindings/leader-line";

//@ts-ignore
import _mod from "./leader-line";

let mod: typeof LeaderLine = _mod;

export type * from "./bindings/leader-line";
export default mod;
