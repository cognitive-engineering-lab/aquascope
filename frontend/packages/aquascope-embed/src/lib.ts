import {
  Editor,
  coarsePermissionDiffs,
  receiverPermissionsField,
} from "aquascope-editor";

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
  document.querySelectorAll<HTMLElement>(".aquascope").forEach(elem => {
    // the containing html element
    let pre = elem;

    // container for the button
    let btnWrap = document.createElement("div");
    btnWrap.classList.add("top-right");

    // button for computing the receiver permissions
    let computePermBtn = document.createElement("button");
    computePermBtn.className = "fa fa-refresh cm-button";

    let initialCode = pre.textContent!.trim();
    pre.textContent = "";

    btnWrap.appendChild(computePermBtn);
    pre.appendChild(btnWrap);

    let serverUrl = pre.dataset.serverUrl
      ? new URL(pre.dataset.serverUrl)
      : undefined;
    let readOnly = pre.dataset.noInteract! == "true";

    let ed = new Editor(
      pre,
      setup,
      [receiverPermissionsField.stateField, coarsePermissionDiffs.stateField],
      err => {
        if (err.type == "BuildError") console.error(err.error);
        else if (err.type == "AnalysisError") console.error(err.error);
        else console.error(err);
      },
      initialCode,
      serverUrl,
      readOnly
    );

    let operation = pre.dataset.operation;
    if (operation) {
      let response = pre.dataset.response
        ? JSON.parse(pre.dataset.response)
        : undefined;

      ed.renderOperation(operation, response);

      computePermBtn.addEventListener("click", _ => {
        ed.renderOperation(operation!);
      });
    }
  });
};

window.addEventListener("load", initEditors, false);
