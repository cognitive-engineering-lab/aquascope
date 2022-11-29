import * as Ed from "./editor";
import { BackendError } from "./types";

export let globals: {
  editor: Ed.Editor;
};

window.addEventListener("load", () => {
  const showRcvrTypesButton = document.getElementById(
    "showReceiverTypes"
  ) as HTMLInputElement;
  const vimKeybindingToggle = document.getElementById(
    "vimKeybindings"
  ) as HTMLInputElement;

  const editorElement = document.querySelector<HTMLElement>(".static-editor")!;
  const tabs = document.querySelectorAll<HTMLElement>(".tab");
  const stdErr = document.querySelector<HTMLElement>(".aquascope-stderr")!;

  // Insert support for multiple tabs

  tabs.forEach(self => {
    self.addEventListener("click", function () {
      var data = this.getAttribute("data-tab");
      document
        .querySelectorAll(".tab-pane.active")[0]
        .classList.remove("active");
      document
        .querySelectorAll(`.tab-pane[data-pane="${data}"]`)[0]
        .classList.add("active");
      document.querySelectorAll(".tab.active")[0].classList.remove("active");
      this.classList.add("active");
    });
  });

  globals = {
    editor: new Ed.Editor(
      editorElement,
      [Ed.receiverPermissionsField.stateField],
      (err: BackendError) => {
        if (err.type === "FileNotFound") {
          alert("A backend problem occurred!");
        } else {
          stdErr.textContent = err.error;
        }
      }
    ),
  };

  let toggleVim = (_: any) => {
    globals.editor.toggleVim(vimKeybindingToggle?.checked);
  };

  // Set the initial state of the VIM bindings
  toggleVim(0);

  vimKeybindingToggle.addEventListener("click", toggleVim);

  showRcvrTypesButton.addEventListener("click", _ => {
    return globals.editor.computeReceiverPermissions();
  });
});
