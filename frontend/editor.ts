import { basicSetup } from "./setup"
import { EditorView } from "@codemirror/view"
import { EditorState } from "@codemirror/state"
import { rust } from "@codemirror/lang-rust"

// This module needs to get updated.
let cm = undefined;

export const run_editor = () => {
    const app = document.getElementById("editor");

    const initial_state = EditorState.create({
        doc: "// please, start typing!",
        extensions: [
            basicSetup,
            rust(),
        ],
    });

    const view = new EditorView({
        state: initial_state,
        parent: app,
    });

    cm = view;

};

export const get_contents = () => {
    return cm
        .state
        .doc
        .toString();
}
