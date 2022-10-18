import * as cp from "child_process"
import { BackendError, BackendResult } from "./types"
import { get_contents } from "./editor"
import { Sandbox } from "./sandbox"

export type Oracle = <T>(
    _args: string[],
    _expect_output?: boolean,
) => Promise<BackendResult<T>>;

// serde-compatible type
type Result<T> = { Ok: T } | { Err: BackendError };

export async function initialize(): Promise<Oracle> {
    // TODO initialize the backend
    // TODO check version
    // TODO check the environemnt
    return async <T>(args: string[], no_output: boolean = false) => {
        let output;
        let editor_contents = get_contents();

        let env = new Sandbox(editor_contents);

        return  {
            type: "output",
            value: undefined as any,
        };

        // try {
        //     let editor = vscode.window.activeTextEditor;
        //     if (editor) {
        //         await editor.document.save();
        //     }

        //     output = await exec_notify(
        //         cargo,
        //         [...cargo_args, "flowistry", ...args],
        //         "Waiting for Flowistry...",
        //         flowistry_opts
        //     );
        // } catch (e: any) {
        //     context.workspaceState.update("err_log", e);

        //     return {
        //         type: "BuildError",
        //         error: e,
        //     };
        // }

        // if (no_output) {
        //     return {
        //         type: "output",
        //         value: undefined as any,
        //     };
        // }

        // let output_typed: Result<T> = JSON.parse(output);
        // if ("Err" in output_typed) {
        //     return output_typed.Err;
        // } else {
        //     return {
        //         type: "output",
        //         value: output_typed.Ok,
        //     };
        // }

    };
}
