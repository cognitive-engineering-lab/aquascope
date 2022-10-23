import { Editor } from "./editor"
import * as cp from "child_process"
import os from "os"
import { BackendResult, BackendError, ReceiverTypes } from "./types"

const SERVER_HOST = "127.0.0.1";
const SERVER_PORT = "8008";

export let globals: {
    editor: Editor;
};

type Result<T> = { Ok: T } | { Err: BackendError };

// XXX why this extra server response type is really
// annoying and I'd like to get rid of it. This change would
// require modifying how command output is read from the spawned
// docker container on the backend.
type ServerResponse = {
    success: boolean,
    stdout: string,
    stderr: string,
};

window.onload = async () => {
    let recv_t_btn: HTMLElement = document.getElementById("receiver_types_btn");
    let editor_element: HTMLElement = document.getElementById("editor");

    globals = {
        editor: new Editor(editor_element);
        backend: () => {
            throw new Error('TODO');
        },
    };

    recv_t_btn.addEventListener("click", (e:Event) =>
       refresh_receiver_vis());

}

async function refresh_receiver_vis() {
    get_receiver_types()
        .then((output: BackendOutput<ReceiverTypes>) => {
            if (output.type === "output") {
                console.log("output is successful");
                console.log(output.value);
                return globals.editor.swap_receiver_type_widgets(output.value);
            } else {
                console.log("An error occurred");
                console.log(output);
                return;
            }
        });
}

function get_receiver_types(): Promise<BackendResult<ReceiverTypes>> {
    let code_in_editor = globals.editor.get_current_contents();
    return fetch(`http://${SERVER_HOST}:${SERVER_PORT}/receiver-types`, {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
        },
        body: JSON.stringify({
            code: code_in_editor,
        }),
    })
        .then((response) => response.json())
        .then((data: ServerResponse) => JSON.parse(data.stdout))
        .then((data: Result<ReceiverTypes>) => {
            return {
                type: "output",
                value: data.Ok,
            };
        });
}
