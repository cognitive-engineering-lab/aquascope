import { Card } from "@mui/material";
import { vim } from "@replit/codemirror-vim";
import * as Ed from "aquascope-editor";

import { basicSetup } from "./setup";

declare global {
  var SERVER_URL: string;
}

export let globals: {
  editor: Ed.Editor;
};

window.addEventListener("load", () => {
  const showBoundariesChck = document.getElementById(
    "showBoundaries"
  ) as HTMLInputElement;

  const showStepsChck = document.getElementById(
    "showSteps"
  ) as HTMLInputElement;

  const showPermissionsButton = document.getElementById(
    "showPermissions"
  ) as HTMLInputElement;

  const interpretButton = document.getElementById(
    "showInterpret"
  ) as HTMLInputElement;

  const editorElement =
    document.querySelector<HTMLDivElement>(".static-editor")!;
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
      basicSetup,
      (err: Ed.types.BackendError) => {
        if (err.type === "FileNotFound") {
          alert("A backend problem occurred!");
        } else if (err.type === "ServerStderr") {
          stdErr.textContent = err.error;
        } else {
          console.error("an unknown error occurred:", err);
        }
      },
      Ed.defaultCodeExample,
      new URL(SERVER_URL)
    ),
  };

  // let toggleVim = (_: any) => {
  //   let extensions = vimKeybindingToggle?.checked ? [vim()] : [];
  //   globals.editor.reconfigure(extensions);
  // };

  // Set the initial state of the VIM bindings
  // toggleVim(0);

  // vimKeybindingToggle.addEventListener("click", toggleVim);

  showPermissionsButton.addEventListener("click", _ => {
    return globals.editor.renderPermissions({
      stepper: showStepsChck.checked,
      boundaries: showBoundariesChck.checked,
    });
  });

  interpretButton.addEventListener("click", () =>
    globals.editor.renderOperation("interpreter")
  );
});