import { rust } from "@codemirror/lang-rust";
import { indentUnit } from "@codemirror/language";
import {
  Compartment,
  EditorState,
  Extension,
  StateEffect,
  StateField,
} from "@codemirror/state";
import { DecorationSet, EditorView, ViewUpdate } from "@codemirror/view";
import _ from "lodash";

import { renderInterpreter } from "./editor-utils/interpreter";
import {
  IconField,
  LoanFacts,
  generateAnalysisDecorationFacts,
  loanFactsField,
  loanFactsStateType,
} from "./editor-utils/misc";
import {
  copiedValueHover,
  insufficientTypeHover,
  receiverPermissionsField,
} from "./editor-utils/permission-boundaries";
import { StepperConfig, renderSteps } from "./editor-utils/stepper";
import "./styles.scss";
import {
  AnalysisFacts,
  AnalysisOutput,
  BackendError,
  PermissionsBoundary,
  PermissionsDiffOutput,
  Range,
} from "./types";

export { receiverPermissionsField } from "./editor-utils/permission-boundaries";
export * as types from "./types";

const DEFAULT_SERVER_URL = new URL("http://127.0.0.1:8008");

type Result<T> = { Ok: T } | { Err: BackendError };

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
  \`[let n = 5;]\`
  \`[let y = plus_one(n);]\` \`(step:focus,.*[n].*)\`
  println!("The value of y is: {y}");
}

\`[fn plus_one(x: i32)]\` -> i32 {
  x + 1
}
`.trim();

let readOnly = new Compartment();
let mainKeybinding = new Compartment();

type ParseResult =
  | { type: "ok"; code: string; ranges: Range[] }
  | { type: "err"; error: string };

let parseWithDelimiters = (
  code: string,
  delimiters: [string, string][],
  retainMatched: (toks: string[], range: Range) => boolean = _ => true
): ParseResult => {
  let [open, close] = _.unzip(delimiters);
  let makeCheck = (arr: string[]) => {
    let r = new RegExp(`^${arr.map(s => _.escapeRegExp(s)).join("|")}`);
    return (s: string) => {
      let match = s.match(r);
      return match ? match[0].length : null;
    };
  };
  let [openCheck, closeCheck] = [makeCheck(open), makeCheck(close)];
  let popN = <T>(arr: T[], n: number) => {
    while (n-- > 0) {
      arr.pop();
    }
  };

  let index = 0;
  let inSeq = null;
  let ranges: Range[] = [];
  let outputCode = [];
  let i = 0;
  while (i < code.length) {
    if (inSeq === null) {
      let n = openCheck(code.substring(i));
      if (n) {
        i += n;
        inSeq = index;
        continue;
      }
    } else {
      let n = closeCheck(code.substring(i));
      if (n) {
        let seqN = index - inSeq!;
        let range = {
          char_start: inSeq!,
          char_end: index,
          byte_start: 0,
          byte_end: 0,
          filename: "",
        };
        if (!retainMatched(outputCode.slice(-seqN), range)) {
          popN(outputCode, seqN);
        }
        i += n;
        ranges.push(range);
        inSeq = null;
        continue;
      }
    }

    index += 1;
    outputCode.push(code[i]);
    i += 1;
  }

  return { type: "ok", code: outputCode.join(""), ranges };
};

let buildStepperConfig = (config: StepperConfig) => {
  config.focusedCharPos = [];
  config.focusedPaths = new Map();
  return (toks: string[], range: Range): boolean => {
    let hasTag = toks.length >= 5 && toks.slice(0, 5).join("") === "step:";
    return (
      !hasTag ||
      (() => {
        let remaining = toks.slice(5).join("");
        let commaSep = remaining.split(",", 2).map(s => s.trim());
        if (commaSep.includes("focus")) {
          config.focusedCharPos!.push(range.char_start);
          commaSep.splice(commaSep.indexOf("focus"), 1);
        }
        if (commaSep[0]) {
          config.focusedPaths!.set(range.char_start, commaSep[0]);
        }
        return false;
      })()
    );
  };
};

export class Editor {
  private view: EditorView;
  private interpreterContainer: HTMLDivElement;
  private editorContainer: HTMLDivElement;
  private markedRanges: Range[];
  private focusStepConfigs: StepperConfig;

  public constructor(
    dom: HTMLDivElement,
    readonly setup: Extension,
    supportedFields: Array<StateField<DecorationSet>>,
    readonly reportStdErr: (err: BackendError) => void = function (err) {
      console.log("An error occurred: ");
      console.log(err);
    },
    initialCode: string = defaultCodeExample,
    readonly serverUrl: URL = DEFAULT_SERVER_URL,
    readonly noInteract: boolean = false
  ) {
    let interpParseResult = parseWithDelimiters(initialCode, [["`[", "]`"]]);
    if (interpParseResult.type == "err")
      throw new Error(interpParseResult.error);

    this.focusStepConfigs = {};
    let stepParseResult = parseWithDelimiters(
      interpParseResult.code,
      [["`(", ")`"]],
      buildStepperConfig(this.focusStepConfigs)
    );
    if (stepParseResult.type == "err") throw new Error(stepParseResult.error);

    let cleanedCode = stepParseResult.code;

    let resetMarkedRangesOnEdit = EditorView.updateListener.of(
      (upd: ViewUpdate) => {
        if (upd.docChanged) {
          this.markedRanges = [];
          this.focusStepConfigs = {};
        }
      }
    );

    let initialState = EditorState.create({
      doc: cleanedCode,
      extensions: [
        mainKeybinding.of(setup),
        readOnly.of(EditorState.readOnly.of(noInteract)),
        resetMarkedRangesOnEdit,
        setup,
        rust(),
        indentUnit.of("  "),
        copiedValueHover,
        insufficientTypeHover,
        loanFactsField,
        ...supportedFields,
      ],
    });

    this.markedRanges = interpParseResult.ranges;
    console.debug("Marked ranges:", this.markedRanges);

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
    console.log(newEffects);
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

  async renderOperation(operation: string, out?: Result<any>, config?: any) {
    if (!out) {
      let serverResponse = await this.callBackendWithCode(operation);
      if (serverResponse.success) {
        out = JSON.parse(serverResponse.stdout);
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

    let result = (out as any).Ok;

    if (operation == "interpreter") {
      renderInterpreter(
        this.view,
        this.interpreterContainer,
        result,
        this.view.state.doc.toJSON().join("\n"),
        this.markedRanges,
        config
      );
    } else if (operation == "permission-diffs") {
      renderSteps(
        this.view,
        this.editorContainer,
        result,
        this.focusStepConfigs
      );
    } else if (operation == "receiver-types") {
      let [facts, loanFacts] = generateAnalysisDecorationFacts(result);
      this.addAnalysisFacts(loanFacts);
      this.addPermissionsField(receiverPermissionsField, result.values, facts);
    }
  }
}
