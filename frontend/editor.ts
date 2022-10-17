import { basicSetup } from "./setup"
import { EditorView } from "@codemirror/view"
import { EditorState } from "@codemirror/state"
import { rust } from "@codemirror/lang-rust"

export const runEditor = () => {
    const app = document.getElementById("editor");

    const initialState = EditorState.create({
        doc: "// please, start typing!",
        extensions: [
            basicSetup,
            rust(),
        ],
    });

    const view = new EditorView({
        state: initialState,
        parent: app,
    });

};
