import { Editor, receiverPermissionsField } from "aquascope-editor";

// What Gavin thinks the example usage of this is:
//
// <pre class="aquascope"
//      data-server-host="127.0.0.1"
//      data-server-port="8008"
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
    computePermBtn.classList.add("cm-button");

    let initialCode = pre.textContent!;
    pre.textContent = "";

    btnWrap.appendChild(computePermBtn);
    pre.appendChild(btnWrap);
    computePermBtn.textContent = "Compute Permissions";

    let serverHost = pre.dataset.serverHost!;
    let serverPort = pre.dataset.serverPort!;
    let readOnly = pre.dataset.noInteract! == "true";

    let ed = new Editor(
      pre,
      [receiverPermissionsField.stateField],
      _ => {
        alert("You aren't handling errors!");
      },
      initialCode,
      serverHost,
      serverPort,
      readOnly
    );

    computePermBtn.addEventListener("click", _ => {
      ed.computeReceiverPermissions();
    });

    // start loading the permissions
    ed.computeReceiverPermissions();
  });
};

window.addEventListener("load", initEditors, false);
