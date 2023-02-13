import { Editor, Result, types } from "aquascope-editor";

import { setup } from "./setup";
import "./styles.scss";

const AQUASCOPE_NAME = "aquascope";
const EMBED_NAME = "aquascope-embed";

declare global {
  function initAquascopeBlocks(root: HTMLElement): void;
}

window.initAquascopeBlocks = (root: HTMLElement) => {
  root.querySelectorAll<HTMLDivElement>("." + EMBED_NAME).forEach(elem => {
    elem.classList.remove(EMBED_NAME);
    elem.classList.add(AQUASCOPE_NAME);

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

    let maybeParseJson = <T>(s: string | undefined): T | undefined =>
      s ? JSON.parse(s) : undefined;

    let initialCode = maybeParseJson<string>(elem.dataset.code);
    if (!initialCode) throw new Error("Missing data-code attribute");

    let serverUrl = elem.dataset.serverUrl
      ? new URL(elem.dataset.serverUrl)
      : undefined;

    let shouldFailHtml = `
  <div class="ferris-container">
    <a href="ch00-00-introduction.html#ferris" target="_blank">
      <img
        src="img/ferris/does_not_compile.svg"
        title="This code does not compile!"
        class="ferris ferris-large"
      />
    </a>
  </div>
  `;

    let ed = new Editor(
      elem,
      setup,
      err => {
        console.error(err);
      },
      initialCode,
      serverUrl,
      readOnly,
      shouldFailHtml,
      ["copy"]
    );

    let operations = maybeParseJson<string[]>(elem.dataset.operations);
    if (!operations) throw new Error("Missing data-operations attribute");

    let responses = maybeParseJson<{ [op: string]: Result<any> }>(
      elem.dataset.responses
    );
    let config = maybeParseJson<any>(elem.dataset.config);
    let annotations = maybeParseJson<types.AquascopeAnnotations>(
      elem.dataset.annotations
    );

    if (responses) {
      operations.forEach(operation => {
        ed.renderOperation(operation, {
          response: responses![operation],
          config,
          annotations,
        });
      });
    }

    computePermBtn?.addEventListener("click", _ => {
      operations!.forEach(operation => ed.renderOperation(operation!, {}));
    });
  });
};

window.addEventListener(
  "load",
  () => initAquascopeBlocks(document.body),
  false
);
