import { rust } from "@codemirror/lang-rust";
import { indentUnit } from "@codemirror/language";
import { Compartment, EditorState, Extension } from "@codemirror/state";
import { EditorView, ViewUpdate } from "@codemirror/view";
import _ from "lodash";
import React, { useEffect, useState } from "react";
import ReactDOM from "react-dom/client";

import { boundaryField, renderBoundaries } from "./editor-utils/boundaries";
import { renderInterpreter } from "./editor-utils/interpreter";
import {
  IconField,
  LoanFacts,
  generateAnalysisDecorationFacts,
  hiddenLines,
  hideLine,
  loanFactsField,
  loanFactsStateType,
} from "./editor-utils/misc";
import { renderSteps, stepField } from "./editor-utils/stepper";
import "./styles.scss";
import { AnalysisFacts, AquascopeAnnotations, BackendError } from "./types";

export * as types from "./types";

const DEFAULT_SERVER_URL = new URL("http://127.0.0.1:8008");

export type Result<T> = { Ok: T } | { Err: BackendError };

// XXX this extra server response type is really
// annoying and I'd like to get rid of it. This change would
// require modifying how command output is read from the spawned
// docker container on the backend.
type ServerResponse = {
  success: boolean;
  stdout: string;
  stderr: string;
};

export const defaultCodeExample: string = `
fn main() {
  let mut v = vec![1, 2, 3];
  let n = &v[0];
  v.push(0);
  let x = *n;
}
`.trim();

let readOnly = new Compartment();
let mainKeybinding = new Compartment();

interface Button {
  icon: string;
  index: number;
}

type ButtonName = "copy" | "eye";
const BUTTON_ORDER: ButtonName[] = ["copy", "eye"];

let CopyButton = ({ view }: { view: EditorView }) => (
  <i
    className="fa fa-copy"
    onClick={() => {
      let contents = view.state.doc.toJSON().join("\n");
      navigator.clipboard.writeText(contents);
    }}
  />
);

let HideButton = ({ container }: { container: HTMLDivElement }) => {
  let [hidden, setHidden] = useState(true);
  useEffect(() => {
    if (!hidden) container.classList.add("show-hidden");
    else container.classList.remove("show-hidden");
  }, [hidden]);
  return (
    <i
      className={`fa ${hidden ? "fa-eye" : "fa-eye-slash"}`}
      onClick={() => setHidden(!hidden)}
    />
  );
};

interface CommonConfig {
  shouldFail?: boolean;
}

export class Editor {
  private view: EditorView;
  private interpreterContainer: HTMLDivElement;
  private editorContainer: HTMLDivElement;
  private metaContainer: ReactDOM.Root;
  private buttons: Set<ButtonName>;
  private shouldFail: boolean = false;

  public constructor(
    dom: HTMLDivElement,
    readonly setup: Extension,
    readonly reportStdErr: (err: BackendError) => void = function (err) {
      console.log("An error occurred: ");
      console.log(err);
    },
    code: string = defaultCodeExample,
    readonly serverUrl: URL = DEFAULT_SERVER_URL,
    readonly noInteract: boolean = false,
    readonly shouldFailHtml: string = "This code does not compile!"
  ) {
    let resetMarkedRangesOnEdit = EditorView.updateListener.of(
      (upd: ViewUpdate) => {
        if (upd.docChanged) {
          // this.cfg = { markedRanges: [], stepsCfg: {} };
        }
      }
    );

    this.buttons = new Set(["copy"]);

    let initialState = EditorState.create({
      doc: code,
      extensions: [
        mainKeybinding.of(setup),
        readOnly.of(EditorState.readOnly.of(noInteract)),
        resetMarkedRangesOnEdit,
        setup,
        rust(),
        indentUnit.of("  "),
        hiddenLines,
        loanFactsField,
        boundaryField,
        stepField,
      ],
    });

    this.editorContainer = document.createElement("div");
    this.view = new EditorView({
      state: initialState,
      parent: this.editorContainer,
    });

    let buttonContainer = document.createElement("div");
    this.metaContainer = ReactDOM.createRoot(buttonContainer);
    this.renderMeta();

    this.editorContainer.appendChild(buttonContainer);

    this.interpreterContainer = document.createElement("div");

    dom.appendChild(this.editorContainer);
    dom.appendChild(this.interpreterContainer);
  }

  renderMeta() {
    this.metaContainer.render(
      <div className="meta-container">
        <div className="top-right">
          {Array.from(this.buttons).map((button, i) => (
            <button className="cm-button" key={i}>
              {button == "copy" ? (
                <CopyButton view={this.view} />
              ) : button == "eye" ? (
                <HideButton container={this.editorContainer} />
              ) : null}
            </button>
          ))}
        </div>
        {this.shouldFail ? (
          <div dangerouslySetInnerHTML={{ __html: this.shouldFailHtml }} />
        ) : null}
      </div>
    );
  }

  public getCurrentCode(): string {
    return this.view.state.doc.toString();
  }

  public reconfigure(extensions: Extension[]): void {
    this.view.dispatch({
      effects: [mainKeybinding.reconfigure([...extensions, this.setup])],
    });
  }

  public removeIconField<B, T, F extends IconField<B, T>>(f: F) {
    this.view.dispatch({
      effects: [f.effectType.of([])],
    });
  }

  // NOTE: Exchanges the analysis facts for loan points and regions. Currently,
  // this is only used by the permission boundaries (stacks) visualization.
  public addAnalysisFacts(vs: Array<LoanFacts>) {
    this.view.dispatch({
      effects: [loanFactsStateType.of(vs)],
    });
  }

  public addPermissionsField<B, T, F extends IconField<B, T>>(
    f: F,
    methodCallPoints: Array<B>,
    facts: AnalysisFacts
  ) {
    let newEffects = methodCallPoints.map(v => f.fromOutput(v, facts));
    this.view.dispatch({
      effects: [f.effectType.of(newEffects)],
    });
  }

  // Actions to communicate with the aquascope server
  async callBackendWithCode(
    endpoint: string,
    config?: any
  ): Promise<ServerResponse> {
    let inEditor = this.getCurrentCode();
    let endpointUrl = new URL(endpoint, this.serverUrl);
    let serverResponseRaw = await fetch(endpointUrl, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        code: inEditor,
        config,
      }),
    });
    let serverResponse: ServerResponse = await serverResponseRaw.json();
    return serverResponse;
  }

  async renderOperation(
    operation: string,
    {
      response,
      config,
      annotations,
    }: {
      response?: Result<any>;
      config?: CommonConfig & object;
      annotations?: AquascopeAnnotations;
    } = {}
  ) {
    console.debug(`Rendering operation: ${operation}`);

    if (!response) {
      let serverResponse = await this.callBackendWithCode(operation, config);
      if (serverResponse.success) {
        response = JSON.parse(serverResponse.stdout);
        this.reportStdErr({
          type: "BuildError",
          error: serverResponse.stderr,
        });
      } else {
        return this.reportStdErr({
          type: "BuildError",
          error: serverResponse.stderr,
        });
      }
    }

    let result = (response as any).Ok;

    if (
      annotations &&
      annotations.hidden_lines &&
      annotations.hidden_lines.length > 0
    ) {
      this.view.dispatch({
        effects: annotations.hidden_lines.map(line => hideLine.of({ line })),
      });
      this.buttons.add("eye");
    }

    if (config?.shouldFail) {
      this.shouldFail = true;
    }

    this.renderMeta();

    if (operation == "interpreter") {
      renderInterpreter(
        this.view,
        this.interpreterContainer,
        result,
        this.view.state.doc.toJSON().join("\n"),
        config as any,
        annotations?.interp
      );
    } else if (operation == "stepper") {
      renderSteps(this.view, result.values, annotations?.stepper);
    } else if (operation == "boundaries") {
      let [facts, loanFacts] = generateAnalysisDecorationFacts(result);
      this.addAnalysisFacts(loanFacts);
      renderBoundaries(this.view, facts, result.values);
    }
  }
}
