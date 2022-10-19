import * as ed from "./editor"
import * as cp from "child_process"
import os from "os"
import { Oracle, initialize } from "./init"

export let globals: {
    backend: Oracle;
};

window.onload = async () => {
    // Probably best to turn the Editor into an object, on which I can call run,
    // but additionally things like "get the editor contents", etc...
    ed.run_editor();

    let recv_t_btn = document.getElementById("receiver_types_btn");

    globals = {
        backend: () => {
            throw new Error(`Unreachable`);
        },
    };

    let oracle = await initialize();

    if (oracle === null) {
        throw Error(`Handle me pls`);
    }

    globals.backend = oracle;

    recv_t_btn.addEventListener("click", (e:Event) =>
       get_receiver_types());

    let get_receiver_types = async () => {

        // Get the contents of the editor.
        let contents = ed.get_contents()

        alert(`The current editor contents are:\n ${contents}`);
    };
}
