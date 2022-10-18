import * as ed from "./editor"
import * as cp from "child_process"
import os from "os"
import { Oracle, initialize } from "./init"

export let globals: {
    backend: Oracle;
};

window.onload = () => {

    let recv_t_btn = document.getElementById("receiver_types_btn");

    let globals = {
        backend = () => {
            throw new Error(`Unreachable`);
        },
    };

    let oracle = await initialize();

    if (oracle === null) {
        throw Error(`Handle Me Pls`);
    }

    globals.backend = oracle;

    recv_t_btn.addEventListener("click", (e:Event) =>
       get_receiver_types());


    let get_receiver_types = async () => {

        // Get the current

        alert('You got the receiver types!');

    };

    ed.run_editor();
}
