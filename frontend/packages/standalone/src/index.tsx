import { Editor, defaultCodeExample, type types } from "@aquascope/editor";
import { initializeAquascopeInstance } from "@aquascope/system";
import { basicSetup } from "./setup";

declare global {
  var SERVER_URL: string;
}

export let globals: {
  editor: Editor;
};

window.addEventListener("load", async () => {
  if (!self.crossOriginIsolated) {
    console.error(
      "The page is not cross-origin isolated, cannot initialize WASM modules."
    );
  } else {
    console.debug("Cross-origin isolation is enabled.");
  }

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
  const changeTab = (el: HTMLElement) => {
    const data = el.getAttribute("data-tab");
    document.querySelectorAll(".tab-pane.active")[0].classList.remove("active");
    document
      .querySelectorAll(`.tab-pane[data-pane="${data}"]`)[0]
      .classList.add("active");
    document.querySelectorAll(".tab.active")[0].classList.remove("active");
    el.classList.add("active");
  };
  tabs.forEach(tab => tab.addEventListener("click", () => changeTab(tab)));

  const reportStdErr = (err: types.BackendError) => {
    if (err.type === "ServerStderr") {
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
  };

  const backend = await initializeAquascopeInstance();

  const editor = new Editor(
    editorElement,
    basicSetup,
    backend,
    reportStdErr,
    defaultCodeExample
  );

  globals = { editor };

  analysisErrClose.addEventListener("click", _ =>
    analysisErrorCard.classList.remove("live")
  );

  const withLoadingButton = async (
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

  const renderPermissions = () =>
    withLoadingButton(showPermissionsButton, () =>
      globals.editor.renderPermissions({
        stepper: showStepsChck.checked,
        boundaries: showBoundariesChck.checked
      })
    );

  const renderInterpreter = () =>
    withLoadingButton(interpretButton, () =>
      globals.editor.renderOperation("interpreter", {
        config: {
          shouldFail: !passesBorrowCheckerButton.checked
        }
      })
    );

  renderPermissions();
  renderInterpreter();

  showPermissionsButton.addEventListener("click", renderPermissions);
  interpretButton.addEventListener("click", renderInterpreter);
});
