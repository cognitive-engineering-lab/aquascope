// This file was generated by [ts-rs](https://github.com/Aleph-Alpha/ts-rs). Do not edit this file manually.
import type { CharPos } from "./CharPos";
import type { FlowBoundary } from "./FlowBoundary";
import type { Permissions } from "./Permissions";
import type { PermissionsData } from "./PermissionsData";

export interface PermissionsBoundary { location: CharPos, expected: Permissions, actual: PermissionsData, expecting_flow?: FlowBoundary, }