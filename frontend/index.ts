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
        let code_in_editor = ed.get_contents()

        fetch("http://127.0.0.1:8008/receiver-types", {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({
                code: code_in_editor,
            }),
        })
            .then((response) => response.json())
            .then((json) => console.log(json));

        // fetch(
        //     "http://127.0.0.1:8008/receiver-types", {
        //     method: 'POST',
        //     body: code,
        //     headers: {'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8'}
        //     }).then(function(response){

        //         if (!response.ok) {
        //             throw new Error(response.statusText);
        //         }

        //         // If you care about a response:
        //         if (response.body !== null) {
        //             return response.json();

        //             // body is ReadableStream<Uint8Array>
        //             // parse as needed, e.g. reading directly, or
        //             // let asString = new TextDecoder("utf-8")
        //             //     .decode(response.body);
        //             // and further:
        //             // let asJSON = JSON.parse(asString);  // implicitly 'any', make sure to verify type on runtime.
        //             // alert(`Received back ${asString} ${asJSON}`);
        //         }
        //     }).then(function(data) {
        //         console.log(data);
        //     })
    };
}
