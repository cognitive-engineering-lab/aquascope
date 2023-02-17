import { useFloating } from "@floating-ui/react-dom";
import { Editor, Result, types } from "aquascope-editor";
import React, { useContext, useLayoutEffect, useState } from "react";
import ReactDOM from "react-dom/client";

import { setup } from "./setup";
import "./styles.scss";

const AQUASCOPE_NAME = "aquascope";
const EMBED_NAME = "aquascope-embed";

// Making this global is a hack to expose the prerendered Aquascope visualizer
// to other tools like mdbook-quiz.
//
// TODO: figure out a more elegant / composable solution to this. Probably involves
// moving some of this functionality into aquascope-editor and distributing that
// package on npm.
declare global {
  function initAquascopeBlocks(root: HTMLElement): void;

  var telemetry:
    | {
        log: (endpoint: string, payload: any) => void;
      }
    | undefined;
}

let CodeContext = React.createContext("");

// TODO: this is duplicated with mdbook-quiz. Should factor out into a mdbook-js-utils maybe?
let useCaptureMdbookShortcuts = (capture: boolean) => {
  useLayoutEffect(() => {
    if (capture) {
      let captureKeyboard = (e: KeyboardEvent) => e.stopPropagation();

      // This gets added specifically to document.documentElement rather than document
      // so bubbling events will hit this listener before ones added via document.addEventListener(...).
      // All of the problematic mdBook interactions are created that way, so we ensure that
      // the keyboard event does not propagate to those listeners.
      //
      // However, some widgets like Codemirror require keydown events but on local elements.
      // So we can't just stopPropagation in the capture phase, or those widgets will break.
      // This is the compromise!
      document.documentElement.addEventListener(
        "keydown",
        captureKeyboard,
        false
      );

      return () =>
        document.documentElement.removeEventListener(
          "keydown",
          captureKeyboard,
          false
        );
    }
  }, [capture]);
};

let ContextProvider = ({
  title,
  buttonText,
  children,
}: {
  title: string;
  buttonText: string;
  children: (close: () => void) => JSX.Element;
}) => {
  let [open, setOpen] = useState(false);
  let { x, y, strategy, refs } = useFloating({
    placement: "bottom-end",
    open,
  });

  // Disable mdbook shortcuts if the bug reporter is opened and we're not
  // fullscreen
  useCaptureMdbookShortcuts(open);

  let childEl = children(() => setOpen(false));

  return (
    <div className="context-provider">
      <button
        title={title}
        onClick={() => setOpen(!open)}
        ref={refs.setReference}
      >
        {buttonText}
      </button>
      {open ? (
        <div
          className="popup-context"
          ref={refs.setFloating}
          style={{
            position: strategy,
            top: y ?? 0,
            left: x ?? 0,
          }}
        >
          <button className="close" onClick={() => setOpen(false)}>
            ✕
          </button>
          {childEl}
        </div>
      ) : null}
    </div>
  );
};

let BugReporter = () => (
  <ContextProvider title={"Report a bug"} buttonText={"🐞"}>
    {close => {
      let code = useContext(CodeContext);
      let onSubmit: React.FormEventHandler<HTMLFormElement> = event => {
        let data = new FormData(event.target as any);
        let feedback = data.get("feedback")!.toString();
        window.telemetry!.log("aquascope-bug", { code, feedback });
        event.preventDefault();
        close();
      };
      return (
        <>
          <h3>Report a bug</h3>
          <p>
            If you found an issue in this diagram (e.g. a typo, a visual bug,
            anything confusing), please describe the issue and report it:
          </p>
          <form onSubmit={onSubmit}>
            <div>
              <textarea name="feedback" aria-label="Bug feedback"></textarea>
            </div>
            <div>
              <input type="submit" aria-label="Submit bug feedback" />
            </div>
          </form>
        </>
      );
    }}
  </ContextProvider>
);

let QuestionMark = () => (
  <ContextProvider title={"What is this diagram?"} buttonText={"❓"}>
    {() => (
      <>
        <h3>What is this diagram?</h3>
        <p>
          This diagram is a new way of visualizing Rust programs. To learn more
          about it, read the updated{" "}
          <a href="ch04-00-understanding-ownership.html" target="_blank">
            Chapter 4: Understanding Ownership
          </a>
          .
        </p>
      </>
    )}
  </ContextProvider>
);

let ExtraInfo = () => (
  <div className="extra-info">
    <BugReporter />
    <QuestionMark />
  </div>
);

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

    let maybeParseJson = <T,>(s: string | undefined): T | undefined =>
      s ? JSON.parse(s) : undefined;

    let initialCode = maybeParseJson<string>(elem.dataset.code);
    if (!initialCode) throw new Error("Missing data-code attribute");

    let extraInfo = document.createElement("div");
    elem.appendChild(extraInfo);
    ReactDOM.createRoot(extraInfo).render(
      <CodeContext.Provider value={initialCode}>
        <ExtraInfo />
      </CodeContext.Provider>
    );

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
