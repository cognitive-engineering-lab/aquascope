import { Editor } from "./editor"
import * as cp from "child_process"
import os from "os"
import {
    BackendResult, BackendError,
    ReceiverTypes, BackendOutput
} from "./types"

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
    const show_rcvr_types_toggle = document.getElementById("show_receiver_types") as HTMLInputElement | null;
    const editor_element = document.getElementById("editor") as HTMLElement | null;

    if (show_rcvr_types_toggle == null || editor_element == null) {
        throw new Error ("document elements cannot be null");
    }

    globals = {
        editor: new Editor(editor_element),
    };

    show_rcvr_types_toggle.addEventListener("click", (e:Event) => {
        globals.editor.toggle_readonly(show_rcvr_types_toggle?.checked);
        if (show_rcvr_types_toggle.checked) {
            return refresh_receiver_vis();
        }

        return globals.editor.remove_receiver_types();
    });
}

async function refresh_receiver_vis() {
    get_receiver_types()
        .then((output: BackendOutput<ReceiverTypes>) => {
            if (output.type === "output") {
                console.log("output is successful");
                console.log(output.value);
                return globals.editor.show_receiver_types(output.value);
            } else {
                console.log("An error occurred");
                console.log(output);
                return;
            }
        });
}

function get_receiver_types(): Promise<BackendOutput<ReceiverTypes>> {
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
            if ('Ok' in data) {
                return {
                    type: "output",
                    value: data.Ok,
                };
            } else {
                throw new Error("something bad happened.");
            }
        });
}
