import { rust } from "@codemirror/lang-rust";
import { indentUnit } from "@codemirror/language";
import { Compartment, EditorState, Extension } from "@codemirror/state";
import { EditorView, ViewUpdate } from "@codemirror/view";
import _ from "lodash";

import { boundaryField } from "./editor-utils/boundaries";
import { renderInterpreter } from "./editor-utils/interpreter";
import {
  IconField,
  LoanFacts,
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

export class Editor {
  private view: EditorView;
  private interpreterContainer: HTMLDivElement;
  private editorContainer: HTMLDivElement;
  private permissionsDecos?: PermissionsDecorations;

  public constructor(
    dom: HTMLDivElement,
    readonly setup: Extension,
    readonly reportStdErr: (err: BackendError) => void = function (err) {
      console.error("An error occurred: ");
      console.error(err);
    },
    code: string = defaultCodeExample,
    readonly serverUrl: URL = DEFAULT_SERVER_URL,
    readonly noInteract: boolean = false
  ) {
    let resetMarkedRangesOnEdit = EditorView.updateListener.of(
      (upd: ViewUpdate) => {
        if (upd.docChanged) {
          this.permissionsDecos = undefined;
        }
      }
    );

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

    let editorContainer = document.createElement("div");
    let initialView = new EditorView({
      state: initialState,
      parent: editorContainer,
    });

    this.interpreterContainer = document.createElement("div");

    dom.appendChild(editorContainer);
    dom.appendChild(this.interpreterContainer);

    this.editorContainer = dom;
    this.view = initialView;
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
  // Actions to communicate with the aquascope server

  public renderPermissions(cfg?: PermissionsCfg) {
    if (this.permissionsDecos === undefined) {
      this.renderOperation("permissions", {
        config: cfg,
      });
    }

    renderPermissions(this.view, this.permissionsDecos, cfg);
  }

  async callBackendWithCode(endpoint: string): Promise<ServerResponse> {
    let inEditor = this.getCurrentCode();
    let endpointUrl = new URL(endpoint, this.serverUrl);
    let serverResponseRaw = await fetch(endpointUrl, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        code: inEditor,
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
      config?: any;
      annotations?: AquascopeAnnotations;
    } = {}
  ) {
    console.debug(`Rendering operation: ${operation}`);

    if (!response) {
      let serverResponse = await this.callBackendWithCode(operation);
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

    if (annotations?.hidden_lines) {
      this.view.dispatch({
        effects: annotations.hidden_lines.map(line => hideLine.of({ line })),
      });
    }

    if (operation == "interpreter") {
      let result = (response as any).Ok;
      renderInterpreter(
        this.view,
        this.interpreterContainer,
        result,
        this.view.state.doc.toJSON().join("\n"),
        config,
        annotations?.interp
      );
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
