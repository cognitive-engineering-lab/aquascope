// import { vim } from "@replit/codemirror-vim";
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

  const passesBorrowCheckerButton = document.getElementById(
    "passesBorrowChecker"
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
  let changeTab = (el: HTMLElement) => {
    var data = el.getAttribute("data-tab");
    document.querySelectorAll(".tab-pane.active")[0].classList.remove("active");
    document
      .querySelectorAll(`.tab-pane[data-pane="${data}"]`)[0]
      .classList.add("active");
    document.querySelectorAll(".tab.active")[0].classList.remove("active");
    el.classList.add("active");
  };
  tabs.forEach(tab => tab.addEventListener("click", () => changeTab(tab)));

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
          changeTab(tabs[1]);
          console.debug("showing the build error card");
          buildErrorCard.classList.add("live");
          window.setTimeout(() => {
            console.debug("removing the build error card");
            buildErrorCard.classList.remove("live");
          }, 5000);
        } else if (err.type === "AnalysisError") {
          changeTab(tabs[1]);
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

  let withLoadingButton = async (
    btn: HTMLInputElement,
    inner: () => Promise<void>
  ) => {
    btn.classList.add("waiting");
    btn.disabled = true;

    try {
      await inner();
    } finally {
      btn.classList.remove("waiting");
      btn.disabled = false;
    }
  };

  let renderPermissions = () =>
    withLoadingButton(showPermissionsButton, () =>
      globals.editor.renderPermissions({
        stepper: showStepsChck.checked,
        boundaries: showBoundariesChck.checked,
      })
    );

  let renderInterpreter = () =>
    withLoadingButton(interpretButton, () =>
      globals.editor.renderOperation("interpreter", {
        config: {
          shouldFail: !passesBorrowCheckerButton.checked,
        },
      })
    );

  renderPermissions();
  renderInterpreter();

  showPermissionsButton.addEventListener("click", renderPermissions);
  interpretButton.addEventListener("click", renderInterpreter);
});
