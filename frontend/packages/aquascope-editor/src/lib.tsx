import { rust } from "@codemirror/lang-rust";
import { indentUnit } from "@codemirror/language";
import { Compartment, EditorState, Extension } from "@codemirror/state";
import { EditorView, ViewUpdate } from "@codemirror/view";
import _ from "lodash";
import React, { useEffect, useState } from "react";
import ReactDOM from "react-dom/client";

import { boundaryField } from "./editor-utils/boundaries";
import {
  InterpreterConfig,
  renderInterpreter,
} from "./editor-utils/interpreter";
import {
  IconField,  
  hiddenLines,
  hideLine,
  loanFactsField,
} from "./editor-utils/misc";
import {
  PermissionsCfg,
  PermissionsDecorations,
  makePermissionsDecorations,
  renderPermissions,
} from "./editor-utils/permissions";
import { stepField } from "./editor-utils/stepper";
import "./styles.scss";
import {
  AnalysisFacts,
  AnalysisOutput,
  AquascopeAnnotations,
  BackendError,
  InterpAnnotations,
  MTrace,
  Range,
} from "./types";

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
  stepper?: any;
  boundaries?: any;
}

export class Editor {
  private view: EditorView;
  private interpreterContainer: HTMLDivElement;
  private editorContainer: HTMLDivElement;
  private permissionsDecos?: PermissionsDecorations;
  private metaContainer: ReactDOM.Root;
  private buttons: Set<ButtonName>;
  private shouldFail: boolean = false;

  public constructor(
    dom: HTMLDivElement,
    readonly setup: Extension,
    readonly reportStdErr: (err: BackendError) => void = function (err) {
      console.error("An error occurred: ");
      console.error(err);
    },
    code: string = defaultCodeExample,
    readonly serverUrl: URL = DEFAULT_SERVER_URL,
    readonly noInteract: boolean = false,
    readonly shouldFailHtml: string = "This code does not compile!",
    readonly buttonList: ButtonName[] = []
  ) {
    let resetMarkedRangesOnEdit = EditorView.updateListener.of(
      (upd: ViewUpdate) => {
        if (upd.docChanged) {
          this.permissionsDecos = undefined;
        }
      }
    );

    this.buttons = new Set(buttonList);

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

  public renderPermissions(cfg?: PermissionsCfg) {
    if (this.permissionsDecos === undefined) {
      this.renderOperation("permissions", {
        config: cfg,
      });
    }

    renderPermissions(this.view, this.permissionsDecos, cfg);
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

  renderInterpreter(
    trace: MTrace<Range>,
    config?: InterpreterConfig,
    annotations?: InterpAnnotations
  ) {
    if (config && config.hideCode) {
      this.view.destroy();
      this.metaContainer.unmount();
    }

    let contents = this.view.state.doc.toJSON().join("\n");
    renderInterpreter(
      this.view,
      this.interpreterContainer,
      trace,
      contents,
      config,
      annotations
    );
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
          type: "ServerStderr",
          error: serverResponse.stderr,
        });
      } else {
        return this.reportStdErr({
          type: "ServerStderr",
          error: serverResponse.stderr,
        });
      }
    }

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
      let result = (response as any).Ok;
      this.renderInterpreter(result, config as any, annotations?.interp);
    } else if (operation == "permissions") {
      // The permissions analysis results are sent as an array of
      // body analyses. Each body could have analyzed successfuly,
      // or had a
      // 1. analysis error
      // 2. build error
      // A build error signifies that something went wrong *before*
      // our analysis was run. This should be reported to the user,
      // currently, information is available on stderr but nothing
      // more specific (or visual) is given TODO.
      // For an analysis error, this is something that went wrong
      // internally, usually means a feature was used that we don't support
      // or something actually went terribly wrong. These should be logged
      // somewhere, but the user should also be prompted to open a GitHub issue.
      let cast = response as any as Result<AnalysisOutput>[];
      let results: AnalysisOutput[] = [];

      for (var res of cast) {
        if ("Ok" in res) {
          results.push(res.Ok);
        } else {
          this.reportStdErr(res.Err);
        }
      }

      this.permissionsDecos = makePermissionsDecorations(
        this.view,
        results,
        annotations
      );
      renderPermissions(this.view, this.permissionsDecos, config);
    }
  }
}
