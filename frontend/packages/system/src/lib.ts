import type {
  AquascopePluginArgs,
  Command,
  CommandResult
} from "@aquascope/types";

// TODO: replace with an outside library
import {
  Directory,
  Fd,
  File,
  type OpenDirectory,
  PreopenDirectory,
  WASI,
  strace
} from "./wasi/exports";

import AQUASCOPE_URL from "./assets/wasm-builds/aquascope/aquascope-driver.wasm.gz?url";

// TODO: figure out why import.meta.glob isn't working...
import lib0Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libaddr2line-c29e40a4e2fdf368.rlib?url";
import lib1Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libadler-073ed9cec2dc6e64.rlib?url";
import lib2Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/liballoc-d14450876311ad97.rlib?url";
import lib3Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libcfg_if-5cae6b617799c3ac.rlib?url";
import lib4Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libcompiler_builtins-11caaeb037a0ea74.rlib?url";
import lib5Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libcore-49f36c523504c876.rlib?url";
import lib6Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libgetopts-4228669065cf2a14.rlib?url";
import lib7Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libgimli-f0e2a59d5e2bd7f2.rlib?url";
import lib8Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libhashbrown-76a484aaeaf14d39.rlib?url";
import lib9Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/liblibc-a7bc56b7163103b1.rlib?url";
import lib10Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libmemchr-59fdcccdf0805fe0.rlib?url";
import lib11Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libminiz_oxide-4a67c87bba0364d0.rlib?url";
import lib12Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libobject-735902f2c481bfbd.rlib?url";
import lib13Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libpanic_abort-69bd61e4d206cf8d.rlib?url";
import lib14Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libpanic_unwind-737f6929eb556676.rlib?url";
import lib15Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libproc_macro-a8abca8e4ac43480.rlib?url";
import lib16Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/librustc_demangle-913a23c04e6e5815.rlib?url";
import lib17Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/librustc_std_workspace_alloc-898aef33c176fbba.rlib?url";
import lib18Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/librustc_std_workspace_core-e16d2497af8de769.rlib?url";
import lib19Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/librustc_std_workspace_std-9137d24898b588f2.rlib?url";
import lib20Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libstd-e47ed36a6842c762.rlib?url";
import lib21Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libstd_detect-d803094e4f28ade7.rlib?url";
import lib22Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libsysroot-d83eb78beaa24a3c.rlib?url";
import lib23Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libtest-72c90de0b5a83655.rlib?url";
import lib24Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libunicode_width-d2de2e4ffa294667.rlib?url";
import lib25Url from "./assets/wasm-builds/rustc/lib/rustlib/x86_64-unknown-linux-gnu/lib/libunwind-391d13236fab4b4e.rlib?url";

// This entire library was taken (graciously) from @bjorn3's example of
// Rustc running in the browser with WASI. It's been modified and updated
// to work with Aquascope, but a huge thanks to them for their work. The
// original source is here: https://github.com/bjorn3/browser_wasi_shim

const TARGET_SYSTEM = "x86_64-unknown-linux-gnu";
const RLIB_URLS = [
  lib0Url,
  lib1Url,
  lib2Url,
  lib3Url,
  lib4Url,
  lib5Url,
  lib6Url,
  lib7Url,
  lib8Url,
  lib9Url,
  lib10Url,
  lib11Url,
  lib12Url,
  lib13Url,
  lib14Url,
  lib15Url,
  lib16Url,
  lib17Url,
  lib18Url,
  lib19Url,
  lib20Url,
  lib21Url,
  lib22Url,
  lib23Url,
  lib24Url,
  lib25Url
] as const;

const loadExternalFile = async (path: string) =>
  new File(await (await (await fetch(path)).blob()).arrayBuffer());

const fileFromUrl = async (url: string): Promise<[string, File]> => [
  url.split("/").at(-1)!,
  await loadExternalFile(url)
];

const loadRlibs = async () =>
  new Map(await Promise.all(RLIB_URLS.map(fileFromUrl)));

const compileAquascopeModule = async () =>
  WebAssembly.compileStreaming(fetch(AQUASCOPE_URL));

const buildSysroot = async () =>
  new PreopenDirectory(
    "/sysroot",
    new Map([
      [
        "lib",
        new Directory([
          [
            "rustlib",
            new Directory([
              ["wasm32-wasi", new Directory([["lib", new Directory([])]])],
              [
                TARGET_SYSTEM,
                new Directory([["lib", new Directory(await loadRlibs())]])
              ]
            ])
          ]
        ])
      ]
    ])
  );

class Stdio extends Fd {
  private out: Uint8Array[];

  constructor(out: Uint8Array[] = []) {
    super();
    this.out = out;
  }

  fd_write(data: Uint8Array): { ret: number; nwritten: number } {
    this.out.push(data);
    return { ret: 0, nwritten: data.byteLength };
  }

  clear() {
    this.out.length = 0;
  }

  text(): string {
    const decoder = new TextDecoder("utf-8");
    let string = "";
    for (const b of this.out) {
      string += decoder.decode(b);
    }
    return string;
  }
}

type CommandArgs<C extends Command> = {
  should_fail: boolean;
} & (C extends "interpreter"
  ? { command: "Interpreter" }
  : C extends "permissions"
    ? {
        command: {
          Permissions: {
            show_flows: boolean;
            steps_include_mode: "Changes";
          };
        };
      }
    : never);

export interface AquascopeBackend {
  call<C extends Command>(
    cmd: C,
    code: string,
    cfg?: {
      shouldFail?: boolean;
    }
  ): Promise<CommandResult<C>>;
}

/**
 * Setup a WASI instance with the given arguments and load the rustc module.
 */
export async function initializeAquascopeInstance(): Promise<AquascopeBackend> {
  const [mod, sysroot] = await Promise.all([
    compileAquascopeModule(),
    buildSysroot()
  ]);
  return new Aquascope(mod, sysroot);
}

class Aquascope implements AquascopeBackend {
  static PROGRAM_NAME = "aq-main.rs";
  static ARGS: string[] = [
    "aquascope-driver",
    "rustc",
    "--edition=2021",
    "--sysroot=/sysroot",
    `--target=${TARGET_SYSTEM}`,
    Aquascope.PROGRAM_NAME
  ];

  private stdin: Stdio;
  private stdout: Stdio;
  private stderr: Stdio;

  private tmp: PreopenDirectory;
  private root: PreopenDirectory;
  private fds: [
    Stdio,
    Stdio,
    Stdio,
    OpenDirectory,
    OpenDirectory,
    OpenDirectory
  ];

  next_thread_id = 1;

  constructor(
    readonly aquascope: WebAssembly.Module,
    readonly sysroot: PreopenDirectory,
    readonly debug: boolean = false
  ) {
    this.stdin = new Stdio();
    this.stdout = new Stdio();
    this.stderr = new Stdio();

    this.tmp = new PreopenDirectory("/tmp", new Map([]));
    this.root = new PreopenDirectory(
      "/",
      new Map([[Aquascope.PROGRAM_NAME, new File([])]])
    );

    this.fds = [
      this.stdin,
      this.stdout,
      this.stderr,
      this.tmp,
      this.sysroot,
      this.root
    ];
  }

  private clearBuffers() {
    this.stdin.clear();
    this.stdout.clear();
    this.stderr.clear();
    this.saveProgram(`
      // Default program
      fn main() {
        println!("You forgot to save the program!");
      }
    `);
  }

  private buildEnv(args: AquascopePluginArgs): string[] {
    return [
      // Needed to force rustc_plugin to not compile as normal Rust
      "CARGO_PRIMARY_PACKAGE=",
      // Pass commands to the plugin
      `PLUGIN_ARGS=${JSON.stringify(args)}`
    ];
  }

  private async run<C extends Command>(
    code: string,
    command: CommandArgs<C>
  ): Promise<CommandResult<C>> {
    this.clearBuffers();
    this.saveProgram(code);

    const env = this.buildEnv(command);    
    const wasi = new WASI(Aquascope.ARGS, env, this.fds, { debug: this.debug });

    const inst = await WebAssembly.instantiate(this.aquascope, {
      env: {
        memory: new WebAssembly.Memory({
          initial: 256,
          maximum: 1024 * 4,
          shared: false
        })
      },
      wasi: {
        // @ts-ignore
        "thread-spawn": function (start_arg) {
          // @ts-ignore
          const thread_id = this.next_thread_id++;
          // @ts-ignore
          inst.exports.wasi_thread_start(thread_id, start_arg);
          return thread_id;
        }
      },
      wasi_snapshot_preview1: strace(wasi.wasiImport, ["fd_prestat_get"])
    });

    try {
      // @ts-ignore
      wasi.start(inst);
      return JSON.parse(this.stdout.text());
    } catch (e: any) {
      console.info("Caught error", e);
      const msg = this.stderr.text() || e.message;
      return {
        Err: {
          type: "ServerStderr",
          error: msg
        }
      } as CommandResult<C>;
    }
  }

  private saveProgram(code: string) {
    this.fds[5].dir.get_file(Aquascope.PROGRAM_NAME)!.data =
      new TextEncoder().encode(code);
  }

  // TODO: figure out how to properly type this without using `as` and the hard-coded strings
  async call<C extends Command>(
    cmd: C,
    code: string,
    cfg?: {
      shouldFail?: boolean;
    }
  ): Promise<CommandResult<typeof cmd>> {
    if (cmd === "interpreter") {
      return (await this.run<"interpreter">(code, {
        should_fail: cfg?.shouldFail ?? false,
        command: "Interpreter"
      })) as CommandResult<typeof cmd>;
    } else if (cmd === "permissions") {
      return (await this.run<"permissions">(code, {
        should_fail: cfg?.shouldFail ?? false,
        command: {
          Permissions: {
            show_flows: false,
            steps_include_mode: "Changes"
          }
        }
      })) as CommandResult<typeof cmd>;
    } else {
      throw new Error("Unknown command", cmd);
    }
  }
}
