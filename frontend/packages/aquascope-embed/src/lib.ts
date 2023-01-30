import { Editor, Result, types } from "aquascope-editor";

import { setup } from "./setup";
import "./styles.scss";

const AQUASCOPE_NAME = "aquascope";
const EMBED_NAME = "aquascope-embed";

let initEditors = () => {
  // embedded aquascope editors should be <div> tags with the class 'aquascope'
  document.querySelectorAll<HTMLDivElement>("." + EMBED_NAME).forEach(elem => {
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

    let ed = new Editor(
      elem,
      setup,
      err => {
        console.error(err);
      },
      initialCode,
      serverUrl,
      readOnly
    );

    let operation = elem.dataset.operation;
    if (!operation) throw new Error("Missing data-operation attribute");

    let response = maybeParseJson<Result<any>>(elem.dataset.response);
    let config = maybeParseJson<any>(elem.dataset.config);
    let annotations = maybeParseJson<types.AquascopeAnnotations>(
      elem.dataset.annotations
    );

    ed.renderOperation(operation, {
      response,
      config,
      annotations,
    });

    computePermBtn?.addEventListener("click", _ => {
      ed.renderOperation(operation!, {});
    });
  });
};

window.addEventListener("load", initEditors, false);
