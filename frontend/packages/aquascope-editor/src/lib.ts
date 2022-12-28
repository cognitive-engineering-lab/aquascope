import { rust } from "@codemirror/lang-rust";
import { indentUnit } from "@codemirror/language";
import {
  Compartment,
  EditorState,
  Extension,
  StateField,
} from "@codemirror/state";
import { DecorationSet, EditorView } from "@codemirror/view";

import { IconField } from "./editor-utils/misc";
import {
  copiedValueHover,
  insufficientTypeHover,
  receiverPermissionsField,
} from "./editor-utils/permission-boundaries";
import { coarsePermissionDiffs } from "./editor-utils/permission-steps";
import "./styles.scss";
import {
  BackendError,
  PermissionsBoundaryOutput,
  PermissionsDiffOutput,
} from "./types";

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

export const defaultCodeExample: string = `
trait Kill {
  fn kill(self);
}

impl Kill for String {
  fn kill(self) {}
}

fn main() {
  let mut s1 = String::from("hi");
  let mut s2 = String::from("hello");

  // something here

  s2.kill();
  s1.kill();
}`;

let readOnly = new Compartment();
let mainKeybinding = new Compartment();

export class Editor {
  private view: EditorView;

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
    let initialState = EditorState.create({
      doc: initialCode,
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

    let initialView = new EditorView({
      state: initialState,
      parent: dom,
    });

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

  async computePermissionSteps() {
    let serverResponse = await this.callBackendWithCode("permission-diffs");
    if (serverResponse.success) {
      let out: Result<PermissionsDiffOutput> = JSON.parse(
        serverResponse.stdout
      );
      if ("Ok" in out) {
        this.reportStdErr({
          type: "BuildError",
          error: serverResponse.stderr,
        });
        return this.addPermissionsField(coarsePermissionDiffs, out.Ok);
      } else {
        return this.reportStdErr(out.Err);
      }
    } else {
      return this.reportStdErr({
        type: "BuildError",
        error: serverResponse.stderr,
      });
    }
  }

  async computeReceiverPermissions() {
    let serverResponse = await this.callBackendWithCode("receiver-types");
    if (serverResponse.success) {
      let out: Result<PermissionsBoundaryOutput> = JSON.parse(
        serverResponse.stdout
      );
      if ("Ok" in out) {
        this.reportStdErr({
          type: "BuildError",
          error: serverResponse.stderr,
        });
        return this.addPermissionsField(receiverPermissionsField, out.Ok);
      } else {
        return this.reportStdErr(out.Err);
      }
    } else {
      return this.reportStdErr({
        type: "BuildError",
        error: serverResponse.stderr,
      });
    }
  }
}
