import { Editor, receiverPermissionsField } from "./editor";

// What Gavin thinks the example usage of this is:
//
// <div class="aquascope"
//      data-server-host="127.0.0.1"
//      data-server-port="8008"
//      data-no-interact="true"
//      >
// fn main() {
//     println!("Hello, Rust Book!");
// }
// </div>
//
// The above would render an aquascope editor, pre-loaded with the main function.
// This editor would send requests to `http://127.0.0.1:8008/...` and it would
// be *read only* (because no-interact is true).
let initEditors = () => {
  // embedded aquascope editors should be <div> tags with the class 'aquascope'
  document.querySelectorAll(".aquascope").forEach(elem => {
    // the containing html element
    let div = elem as HTMLDivElement;

    // container for the button
    let btnWrap = div.appendChild(document.createElement("div"));
    btnWrap.classList.add("top-right");

    // button for computing the receiver permissions
    let computePermBtn = btnWrap.appendChild(document.createElement("button"));
    computePermBtn.classList.add("cm-button");

    let initialCode = div.textContent!;
    let serverHost = div.dataset.serverHost!;
    let serverPort = div.dataset.serverPort!;
    let readOnly = div.dataset.noInteract! == "true";

    let ed = new Editor(
      div,
      [receiverPermissionsField.stateField],
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
