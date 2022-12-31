import { rust } from "@codemirror/lang-rust";
import { indentUnit } from "@codemirror/language";
import {
  Compartment,
  EditorState,
  Extension,
  StateField,
} from "@codemirror/state";
import { DecorationSet, EditorView } from "@codemirror/view";
import _ from "lodash";

import { renderInterpreter } from "./editor-utils/interpreter";
import { IconField } from "./editor-utils/misc";
import {
  copiedValueHover,
  insufficientTypeHover,
  receiverPermissionsField,
} from "./editor-utils/permission-boundaries";
import { coarsePermissionDiffs } from "./editor-utils/permission-steps";
import "./styles.scss";
import { BackendError, Range } from "./types";

export { receiverPermissionsField } from "./editor-utils/permission-boundaries";
export { coarsePermissionDiffs } from "./editor-utils/permission-steps";
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

// export const defaultCodeExample: string = `
// #[derive(Debug, Default)]
// struct Box {
//     value: i32,
// }

// impl Box {
//     fn inc(&mut self) {
//         self.value += 1;
//     }

//     fn destroy(mut self) {}
// }

// fn bar() {
//     let b = Box::default();
//     let refine_all = &mut b.value;
//     b.inc();
//     println!("{refine_all}");
//     b.inc();
// }

// fn foo(v: &mut Vec<i32>) {
//   for (i, t) in v.iter().enumerate().rev() {
//     if *t == 0 {
//       v.remove(i);
//     }
//   }
// }

// fn main() {

//     let v1 = vec![1, 2, 3];
//     v1.push(0);

//     let v2 = &mut vec![1, 2, 3];
//     v2.push(0);

//     let b1 = &Box::default();
//     b1.inc();

//     let mut b2 = Box::default();
//     b2.inc();

//     Box::default().destroy();

//     println!("Gruëzi, Weltli");
// }
// `;

// `
// fn main() {
//   let mut a = Box::new(2);
//   *a += 1;
//   let b = a;
//   println!("{b}");
// }
// `

// `
// fn main() {
//   let n = 5;
//   // L1
//   let y = plus_one(n);
//   // L3
//   println!("The value of y is: {y}");
// }

// fn plus_one(x: i32) -> i32 {
//   // L2
//   x + 1
// }
// `

export const defaultCodeExample: string = `
fn main() {
  \`[let n = 5;]\`
  \`[let y = plus_one(n);]\`  
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
  delimiters: [string, string][]
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
        i += n;
        ranges.push({
          char_start: inSeq!,
          char_end: index,
          byte_start: 0,
          byte_end: 0,
          filename: "",
        });
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

export class Editor {
  private view: EditorView;
  private interpreterContainer: HTMLDivElement;
  private markedRanges: Range[];

  public constructor(
    dom: HTMLElement,
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
    let parseResult = parseWithDelimiters(initialCode, [["`[", "]`"]]);
    if (parseResult.type == "err") throw new Error(parseResult.error);

    let initialState = EditorState.create({
      doc: parseResult.code,
      extensions: [
        mainKeybinding.of(setup),
        readOnly.of(EditorState.readOnly.of(noInteract)),
        setup,
        rust(),
        indentUnit.of("  "),
        copiedValueHover,
        insufficientTypeHover,
        ...supportedFields,
      ],
    });

    this.markedRanges = parseResult.ranges;

    let editorContainer = document.createElement("div");
    let initialView = new EditorView({
      state: initialState,
      parent: editorContainer,
    });

    this.interpreterContainer = document.createElement("div");

    dom.appendChild(editorContainer);
    dom.appendChild(this.interpreterContainer);

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
    methodCallPoints: Array<B>
  ) {
    let newEffects = methodCallPoints.map(f.fromOutput);
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

  async renderOperation(operation: string, out?: Result<any>) {
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
        this.interpreterContainer,
        result,
        this.view.state.doc.toJSON().join("\n"),
        this.markedRanges
      );
    } else if (operation == "permission-diffs") {
      this.addPermissionsField(coarsePermissionDiffs, result);
    } else if (operation == "receiver-types") {
      this.addPermissionsField(receiverPermissionsField, result);
    }
  }
}
