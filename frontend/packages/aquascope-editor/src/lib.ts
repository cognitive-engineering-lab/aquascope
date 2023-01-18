import { rust } from "@codemirror/lang-rust";
import { indentUnit } from "@codemirror/language";
import { Compartment, EditorState, Extension } from "@codemirror/state";
import { EditorView, ViewUpdate } from "@codemirror/view";
import _ from "lodash";

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
  let mut s = String::from("hello, ");
  let b = &mut s;
  s.push_str("world!");
  hi_world(b);
  println!("{s} {b}");
}

fn hi_world(s: &mut String) {
  s.push_str("world!");
}
`.trim();

let readOnly = new Compartment();
let mainKeybinding = new Compartment();

export class Editor {
  private view: EditorView;
  private interpreterContainer: HTMLDivElement;
  private editorContainer: HTMLDivElement;

  public constructor(
    dom: HTMLDivElement,
    readonly setup: Extension,
    readonly reportStdErr: (err: BackendError) => void = function (err) {
      console.log("An error occurred: ");
      console.log(err);
    },
    code: string = defaultCodeExample,
    readonly serverUrl: URL = DEFAULT_SERVER_URL,
    readonly noInteract: boolean = false
  ) {
    let resetMarkedRangesOnEdit = EditorView.updateListener.of(
      (upd: ViewUpdate) => {
        if (upd.docChanged) {
          // this.cfg = { markedRanges: [], stepsCfg: {} };
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
    if (!response) {
      let serverResponse = await this.callBackendWithCode(operation);
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

    if (annotations?.hidden_lines) {
      this.view.dispatch({
        effects: annotations.hidden_lines.map(line => hideLine.of({ line })),
      });
    }

    if (operation == "interpreter") {
      renderInterpreter(
        this.view,
        this.interpreterContainer,
        result,
        this.view.state.doc.toJSON().join("\n"),
        config,
        annotations?.interp
      );
    } else if (operation == "stepper") {
      renderSteps(this.view, result, annotations?.stepper);
    } else if (operation == "boundaries") {
      let [facts, loanFacts] = generateAnalysisDecorationFacts(result);
      this.addAnalysisFacts(loanFacts);
      renderBoundaries(this.view, facts, result.values);
    }
  }
}
