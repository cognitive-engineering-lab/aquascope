import { StateEffect } from "@codemirror/state";
import {
  Decoration,
  EditorView,
  ViewPlugin,
  WidgetType,
} from "@codemirror/view";
import LeaderLine from "leader-line-new";
import _ from "lodash";
import React, {
  CSSProperties,
  useContext,
  useEffect,
  useRef,
  useState,
} from "react";
import ReactDOM from "react-dom/client";

import {
  Abbreviated,
  MFrame,
  MHeap,
  MStack,
  MStep,
  MValue,
  Range,
} from "../types";

const DEBUG: boolean = false;

interface InterpreterConfig {
  horizontal?: boolean;
  concreteTypes?: boolean;
  hideCode?: boolean;
}

let ConfigContext = React.createContext<InterpreterConfig>({});
let CodeContext = React.createContext("");
let PathContext = React.createContext<string[]>([]);

let codeRange = (code: string, range: Range) => {
  return code.slice(range.char_start, range.char_end);
};

let AbbreviatedView = ({ value }: { value: Abbreviated<MValue> }) => {
  let pathCtx = useContext(PathContext);
  let IndexedContainer: React.FC<
    React.PropsWithChildren<{ index: number }>
  > = ({ children, index }) => {
    let path = [...pathCtx, "index", index.toString()];
    return (
      <PathContext.Provider value={path}>
        <td className={path.join("-")} data-connector="bottom">
          {children}
        </td>
      </PathContext.Provider>
    );
  };

  // TODO: handle indexes into abbreviated + end regions
  return (
    <table className="array">
      <tbody>
        <tr>
          {value.type == "All" ? (
            value.value.map((el, i) => (
              <IndexedContainer key={i} index={i}>
                <ValueView value={el} />
              </IndexedContainer>
            ))
          ) : (
            <>
              {value.value[0].map((el, i) => (
                <IndexedContainer key={i} index={i}>
                  <ValueView value={el} />
                </IndexedContainer>
              ))}
              <td>...</td>
              <IndexedContainer index={100}>
                <ValueView value={value.value[1]} />
              </IndexedContainer>
            </>
          )}
        </tr>
      </tbody>
    </table>
  );
};

type MValueStruct = MValue & { type: "Struct" };
type MStruct = MValueStruct["value"];

type MValuePointer = MValue & { type: "Pointer" };
type MPointer = MValuePointer["value"];

let StructView = ({ value }: { value: MStruct }) => {
  let pathCtx = useContext(PathContext);
  let config = useContext(ConfigContext);

  if (value.alloc_kind !== null && !config.concreteTypes) {
    let alloc_type = value.alloc_kind.type;

    let read_field = (v: MStruct, k: string): MStruct => {
      let field = v.fields.find(([k2]) => k == k2);
      if (!field) {
        let v_pretty = JSON.stringify(v, undefined, 2);
        throw new Error(`Could not find field "${k}" in struct: ${v_pretty}`);
      }
      return (field[1] as MValueStruct).value;
    };

    let read_unique = (unique: MStruct): MStruct => {
      let non_null = read_field(unique, "pointer");
      return non_null;
    };

    let read_vec = (vec: MStruct): MStruct => {
      let raw_vec = read_field(vec, "buf");
      let unique = read_field(raw_vec, "ptr");
      return read_unique(unique);
    };

    let non_null: MStruct;
    if (alloc_type == "String") {
      let vec = read_field(value, "vec");
      non_null = read_vec(vec);
    } else if (alloc_type == "Vec") {
      non_null = read_vec(value);
    } else if (alloc_type == "Box") {
      let unique = read_field(value, "0");
      non_null = read_unique(unique);
    } else {
      throw new Error(`Unimplemented alloc type: ${alloc_type}`);
    }

    let ptr = non_null.fields[0][1];

    return <ValueView value={ptr} />;
  }

  if (value.fields.length == 1) {
    let path = [...pathCtx, "field", "0"];
    return (
      <span className={path.join("-")}>
        <PathContext.Provider value={path}>
          {value.name} /&nbsp;
          <ValueView value={value.fields[0][1]} />
        </PathContext.Provider>
      </span>
    );
  }

  return (
    <>
      {value.name}
      <table>
        <tbody>
          {value.fields.map(([k, v], i) => {
            let path = [...pathCtx, "field", i.toString()];
            return (
              <tr key={k}>
                <td>{k}</td>
                <td className={path.join("-")}>
                  <PathContext.Provider value={path}>
                    <ValueView value={v} />
                  </PathContext.Provider>
                </td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </>
  );
};

let PointerView = ({ value: { path, range } }: { value: MPointer }) => {
  let config = useContext(ConfigContext);

  let segment =
    path.segment.type == "Heap"
      ? `heap-${path.segment.value.index}`
      : `stack-${path.segment.value.frame}-${path.segment.value.local}`;

  let parts = [...path.parts];
  let lastPart = _.last(parts);
  let slice =
    lastPart && lastPart.type == "Subslice" ? lastPart.value : undefined;
  if (lastPart && lastPart.type == "Index" && lastPart.value == 0) parts.pop();
  let partClass = parts.map(part =>
    part.type == "Index"
      ? `index-${part.value}`
      : part.type == "Field"
      ? `field-${part.value}`
      : part.type == "Subslice"
      ? `index-${part.value[0]}`
      : ""
  );

  let attrs: { [key: string]: string } = {
    ["data-point-to"]: [segment, ...partClass].join("-"),
  };
  if (slice) {
    attrs["data-point-to-range"] = [
      segment,
      ...partClass.slice(0, -1),
      `index-${slice[1]}`,
    ].join("-");
  }

  let ptrView = (
    <span className="pointer" {...attrs}>
      ●
    </span>
  );

  return config.concreteTypes && range ? (
    <table>
      <tbody>
        <tr>
          <td>ptr</td>
          <td>{ptrView}</td>
        </tr>
        <tr>
          <td>len</td>
          <td>{range.toString()}</td>
        </tr>
      </tbody>
    </table>
  ) : (
    ptrView
  );
};

let ValueView = ({ value }: { value: MValue }) => {
  let pathCtx = useContext(PathContext);
  return (
    <>
      {value.type == "Bool" ||
      value.type == "Uint" ||
      value.type == "Int" ||
      value.type == "Float" ? (
        value.value.toString()
      ) : value.type == "Char" ? (
        String.fromCharCode(value.value).replace(" ", "\u00A0")
      ) : value.type == "Tuple" ? (
        <>
          <table>
            <tbody>
              <tr>
                {value.value.map((v, i) => {
                  let path = [...pathCtx, "field", i.toString()];
                  return (
                    <td key={i} className={path.join("-")}>
                      <PathContext.Provider value={path}>
                        <ValueView value={v} />
                      </PathContext.Provider>
                    </td>
                  );
                })}
              </tr>
            </tbody>
          </table>
        </>
      ) : value.type == "Struct" ? (
        <StructView value={value.value} />
      ) : value.type == "Enum" ? (
        <>
          {value.value.name} ({value.value.variant})
          <table>
            <tbody>
              {value.value.fields.map(([k, v], i) => {
                let path = [...pathCtx, "field", i.toString()];
                return (
                  <tr key={k}>
                    <td>{k}</td>
                    <td className={path.join("-")}>
                      <PathContext.Provider value={path}>
                        <ValueView value={v} />
                      </PathContext.Provider>
                    </td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </>
      ) : value.type == "Pointer" ? (
        <PointerView value={value.value} />
      ) : value.type == "Array" ? (
        <AbbreviatedView value={value.value} />
      ) : value.type == "Unallocated" ? (
        <>X</>
      ) : (
        <>TODO</>
      )}
    </>
  );
};

let LocalsView = ({
  index,
  locals,
}: {
  index: number;
  locals: [string, MValue][];
}) =>
  locals.length == 0 ? (
    <div className="locals empty-frame">(empty frame)</div>
  ) : (
    <table className="locals">
      <tbody>
        {locals.map(([key, value], i) => {
          let path = ["stack", index.toString(), key];
          return (
            <tr key={i}>
              <td>{key}</td>
              <td className={path.join("-")} data-connector="right">
                <PathContext.Provider value={path}>
                  <ValueView value={value} />
                </PathContext.Provider>
              </td>
            </tr>
          );
        })}
      </tbody>
    </table>
  );

let Header: React.FC<React.PropsWithChildren<{ className: string }>> = ({
  children,
  className,
}) => (
  <div className={`header ${className || ""}`}>
    <div className="header-text">{children}</div>
    <div className="header-bg" />
  </div>
);

let FrameView = ({ index, frame }: { index: number; frame: MFrame<Range> }) => {
  let code = useContext(CodeContext);
  let snippet = codeRange(code, frame.location);
  return (
    <div className="frame">
      <Header className="frame-header">{frame.name}</Header>
      {DEBUG ? <pre>{snippet}</pre> : null}
      <LocalsView index={index} locals={frame.locals} />
    </div>
  );
};

let StackView = ({ stack }: { stack: MStack<Range> }) => (
  <div className="memory stack">
    <Header className="memory-header">Stack</Header>
    <div className="frames">
      {stack.frames.map((frame, i) => (
        <FrameView key={i} index={i} frame={frame} />
      ))}
    </div>
  </div>
);

let HeapView = ({ heap }: { heap: MHeap }) => (
  <div className="memory heap">
    <Header className="memory-header">Heap</Header>
    <table>
      <tbody>
        {heap.locations.map((value, i) => {
          let path = ["heap", i.toString()];
          return (
            <tr key={i}>
              <td className={path.join("-")} data-connector="left">
                <PathContext.Provider value={path}>
                  <ValueView value={value} />
                </PathContext.Provider>
              </td>
            </tr>
          );
        })}
      </tbody>
    </table>
  </div>
);

(LeaderLine as any).positionByWindowResize = false;

let StepView = ({
  container,
  step,
  index,
}: {
  container: React.RefObject<HTMLDivElement>;
  step: MStep<Range>;
  index: number;
}) => {
  let ref = useRef<HTMLDivElement>(null);
  let configCtx = useContext(ConfigContext);
  useEffect(() => {
    let stepContainer = ref.current!;
    let query = (sel: string): HTMLElement => {
      let dst = stepContainer.querySelector<HTMLElement>("." + sel);
      if (!dst)
        throw new Error(`Could not find endpoint for pointer selector: ${sel}`);
      return dst;
    };
    let pointers = stepContainer.querySelectorAll<HTMLSpanElement>(".pointer");
    let mdbookEmbed = getComputedStyle(document.body).getPropertyValue(
      "--inline-code-color"
    );
    let color = mdbookEmbed ? "var(--inline-code-color)" : "black";
    let lines = Array.from(pointers)
      .map(src => {
        try {
          let dstSel = src.dataset.pointTo!;
          let dst = query(dstSel);
          let dstRange = src.dataset.pointToRange
            ? query(src.dataset.pointToRange)
            : undefined;
          let endSocket = dst.dataset.connector as LeaderLine.SocketType;

          let dstAnchor = dstRange
            ? LeaderLine.areaAnchor(dst, {
                shape: "rect",
                width: dstRange.offsetLeft + dst.offsetWidth - dst.offsetLeft,
                height: 2,
                y: "100%",
                fillColor: mdbookEmbed ? "var(--search-mark-bg)" : "red",
              })
            : dstSel.startsWith("stack")
            ? LeaderLine.pointAnchor(dst, { x: "100%", y: "75%" })
            : dst;

          let line = new LeaderLine(src, dstAnchor, {
            color,
            size: 1,
            endPlugSize: 2,
            startSocket: "right",
            endSocket,
          });
          return line;
        } catch (e: any) {
          console.error("Leader line failed to render", e.stack);
          return undefined;
        }
      })
      .filter(l => l) as LeaderLine[];

    let reposition = () => lines.forEach(line => line.position());

    let lastPos = stepContainer.getBoundingClientRect();
    let interval = setInterval(() => {
      let curPos = stepContainer.getBoundingClientRect();
      if (curPos.x != lastPos.x || curPos.y != lastPos.y) reposition();
      lastPos = curPos;
    }, 300);
    let timeout = setTimeout(() => reposition(), 300);

    return () => {
      clearInterval(interval);
      clearTimeout(timeout);
      lines.forEach(line => line.remove());
    };
  }, [configCtx.concreteTypes]);

  return (
    <div className="step">
      <div className="step-header">
        <span className="step-marker">L{index + 1}</span>
      </div>
      <div className="memory-container" ref={ref}>
        <StackView stack={step.stack} />
        {step.heap.locations.length > 0 ? <HeapView heap={step.heap} /> : null}
      </div>
    </div>
  );
};

let InterpreterView = ({
  steps,
  config,
}: {
  steps: MStep<Range>[];
  config: InterpreterConfig;
}) => {
  let ref = useRef<HTMLDivElement>(null);
  let [concreteTypes, setConcreteTypes] = useState(
    config.concreteTypes || false
  );
  let [buttonVisible, setButtonVisible] = useState(false);

  let flexDirection: CSSProperties["flexDirection"] = config.horizontal
    ? "row"
    : "column";

  return (
    <ConfigContext.Provider value={{ ...config, concreteTypes: concreteTypes }}>
      <div
        ref={ref}
        className="interpreter"
        style={{ flexDirection }}
        onMouseEnter={() => setButtonVisible(true)}
        onMouseLeave={() => setButtonVisible(false)}
      >
        <button
          className="concrete-types"
          onClick={() => setConcreteTypes(!concreteTypes)}
          style={{ opacity: buttonVisible ? "1" : "0" }}
        >
          <i className={`fa fa-${concreteTypes ? "eye-slash" : "eye"}`} />
        </button>
        {steps.map((step, i) => (
          <StepView key={i} index={i} step={step} container={ref} />
        ))}
      </div>
    </ConfigContext.Provider>
  );
};

let filterSteps = (
  steps: MStep<Range>[],
  ranges: Range[]
): [Range[], MStep<Range>[]] => {
  let stepsRev = [...steps].reverse();
  let indexedRanges: [number, Range, MStep<Range>][] = ranges.map(range => {
    let stepRevIdx = stepsRev.findIndex((step, i) => {
      let frame = _.last(step.stack.frames)!;
      let rangeInFrame =
        frame.body_span.char_start <= range.char_start &&
        range.char_end <= frame.body_span.char_end;
      let rangeAfterLoc = range.char_end > frame.location.char_start;
      return rangeInFrame && rangeAfterLoc;
    });
    if (stepRevIdx == -1)
      throw new Error(
        `Could not find step for range: ${JSON.stringify(range, undefined, 2)}`
      );
    return [steps.length - stepRevIdx, range, stepsRev[stepRevIdx]];
  });
  let sortedRanges = _.sortBy(indexedRanges, ([idx]) => idx);
  return [
    sortedRanges.map(([_stepIdx, range]) => range),
    sortedRanges.map(([_stepIdx, _range, step]) => step),
  ];
};

let StepMarkerView = ({ index }: { index: number }) => {
  return (
    <span className="step-marker">
      <span>L{index + 1}</span>
    </span>
  );
};

class StepMarkerWidget extends WidgetType {
  constructor(readonly index: number) {
    super();
  }

  toDOM() {
    let container = document.createElement("span");
    ReactDOM.createRoot(container).render(
      <StepMarkerView index={this.index} />
    );
    return container;
  }
}

export function renderInterpreter(
  view: EditorView,
  container: HTMLDivElement,
  steps: MStep<Range>[],
  contents: string,
  markedRanges: Range[],
  config: InterpreterConfig = {}
) {
  if (config.hideCode) {
    view.destroy();
  }

  let root = ReactDOM.createRoot(container);
  if (markedRanges.length > 0) {
    let [sortedRanges, filteredSteps] = filterSteps(steps, markedRanges);
    steps = filteredSteps;

    let decos = _.sortBy(
      sortedRanges.map((range, i) =>
        Decoration.widget({
          widget: new StepMarkerWidget(i),
        }).range(range.char_start)
      ),
      deco => deco.from
    );

    let plugin = ViewPlugin.fromClass(class {}, {
      decorations: () => Decoration.set(decos),
    });
    view.dispatch({
      effects: [StateEffect.appendConfig.of(plugin)],
    });
  }

  root.render(
    <CodeContext.Provider value={contents}>
      <InterpreterView steps={steps} config={config} />
    </CodeContext.Provider>
  );
}
