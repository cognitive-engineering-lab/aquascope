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

  const buildErrorCard = document.getElementById(
    "buildErrorCard"
  ) as HTMLElement;

  const analysisErrorCard = document.getElementById(
    "analysisErrorCard"
  ) as HTMLElement;

  const analysisErrorMsg = document.getElementById(
    "analysisErrorMsg"
  ) as HTMLElement;

  const analysisErrClose = document.getElementById("close") as HTMLElement;

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
        } else if (err.type === "BuildError") {
          console.debug("showing the build error card");
          buildErrorCard.classList.add("live");
          window.setTimeout(() => {
            console.debug("removing the build error card");
            buildErrorCard.classList.remove("live");
          }, 2500);
        } else if (err.type === "AnalysisError") {
          analysisErrorCard.classList.add("live");
          analysisErrorMsg.innerText = err.msg;
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

  analysisErrClose.addEventListener("click", _ =>
    analysisErrorCard.classList.remove("live")
  );

  showPermissionsButton.addEventListener("click", _ =>
    globals.editor.renderPermissions({
      stepper: showStepsChck.checked,
      boundaries: showBoundariesChck.checked,
    })
  );

  interpretButton.addEventListener("click", () =>
    globals.editor.renderOperation("interpreter")
  );
});
