import { Editor, receiverPermissionsField } from "aquascope-editor";

import { setup } from "./setup";
import "./styles.scss";

// What Gavin thinks the example usage of this is:
//
// <pre class="aquascope"
//      data-server-url="http://127.0.0.1:8008"
//      data-no-interact="true"
//      >
// fn main() {
//     println!("Hello, Rust Book!");
// }
// </pre>
//
// The above would render an aquascope editor, pre-loaded with the main function.
// This editor would send requests to `http://127.0.0.1:8008/...` and it would
// be *read only* (because no-interact is true).
let initEditors = () => {
  // embedded aquascope editors should be <div> tags with the class 'aquascope'
  document
    .querySelectorAll<HTMLDivElement>(".aquascope-embed")
    .forEach(elem => {
      elem.classList.remove("aquascope-embed");
      elem.classList.add("aquascope");

      let readOnly = elem.dataset.noInteract! == "true";

      let computePermBtn: HTMLButtonElement | undefined;
      if (!readOnly) {
        // container for the button
        let btnWrap = document.createElement("div");
        btnWrap.classList.add("top-right");

        // button for computing the receiver permissions
        computePermBtn = document.createElement("button");
        computePermBtn.className = "fa fa-refresh cm-button";

        btnWrap.appendChild(computePermBtn);
        elem.appendChild(btnWrap);
      }

      let initialCode = JSON.parse(elem.dataset.code!);

      let serverUrl = elem.dataset.serverUrl
        ? new URL(elem.dataset.serverUrl)
        : undefined;

      let ed = new Editor(
        elem,
        setup,
        [receiverPermissionsField.stateField],
        err => {
          if (err.type == "BuildError") console.error(err.error);
          else if (err.type == "AnalysisError") console.error(err.error);
          else console.error(err);
        },
        initialCode,
        serverUrl,
        readOnly
      );

      let operation = elem.dataset.operation;
      if (operation) {
        let response = elem.dataset.response
          ? JSON.parse(elem.dataset.response)
          : undefined;

        let config = elem.dataset.config
          ? JSON.parse(elem.dataset.config)
          : undefined;

        ed.renderOperation(operation, response, config);

        computePermBtn?.addEventListener("click", _ => {
          ed.renderOperation(operation!);
        });
      }
    });
};

window.addEventListener("load", initEditors, false);
