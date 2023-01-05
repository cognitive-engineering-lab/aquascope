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
  useMemo,
  useRef,
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
}

let ConfigContext = React.createContext<InterpreterConfig>({});
let CodeContext = React.createContext("");
let PathContext = React.createContext<string[]>([]);

let codeRange = (code: string, range: Range) => {
  return code.slice(range.char_start, range.char_end);
};

let intersperse = <T,>(arr: T[], sep: T): T[] => {
  let outp = [];
  for (let i = 0; i < arr.length; ++i) {
    outp.push(arr[i]);
    if (i != arr.length - 1) {
      outp.push(sep);
    }
  }
  return outp;
};

let AbbreviatedView = <T,>({
  value,
  renderEl,
  sep = ",",
}: {
  value: Abbreviated<T>;
  renderEl: (t: T) => JSX.Element;
  sep?: string;
}) => {
  let pathCtx = useContext(PathContext);
  let IndexedContainer: React.FC<
    React.PropsWithChildren<{ index: number }>
  > = ({ children, index }) => {
    let path = [...pathCtx, "index", index.toString()];
    return (
      <PathContext.Provider value={path}>
        <span className={path.join("-")} data-connector="bottom">
          {children}
        </span>
      </PathContext.Provider>
    );
  };
  // TODO: handle indexes into abbreviated + end regions
  return value.type == "All" ? (
    <>
      {intersperse(
        value.value.map((el, i) => (
          <IndexedContainer index={i}>{renderEl(el)}</IndexedContainer>
        )),
        <>{sep}</>
      )}
    </>
  ) : (
    <>
      {intersperse(
        value.value[0].map((el, i) => (
          <IndexedContainer index={i}>{renderEl(el)}</IndexedContainer>
        )),
        <>{sep}</>
      )}
      {sep}...{sep}
      <IndexedContainer index={100}>
        {renderEl(value.value[1])}
      </IndexedContainer>
    </>
  );
};

let ValueView = ({ value }: { value: MValue }) => {
  let pathCtx = useContext(PathContext);
  return (
    <>
      {value.type == "Bool" ||
      value.type == "Char" ||
      value.type == "Uint" ||
      value.type == "Int" ||
      value.type == "Float" ? (
        value.value.toString()
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
        <>
          {value.value.name}
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
        (() => {
          let ptr = value.value;
          let segment =
            ptr.segment.type == "Heap"
              ? `heap-${ptr.segment.value.index}`
              : `stack-${ptr.segment.value.frame}-${ptr.segment.value.local}`;
          let parts = ptr.parts.map(part =>
            part.type == "Index" ? `index-${part.value}` : `field-${part.value}`
          );

          // Small hack so pointers to beginning of arrays point to the cell instead of
          // the first element
          let lastPart = _.last(parts);
          if (lastPart && lastPart == "index-0") parts.pop();

          let pointTo = [segment, ...parts].join("-");
          return (
            <span className="pointer" data-point-to={pointTo}>
              ●
            </span>
          );
        })()
      ) : value.type == "Array" ? (
        <>
          [
          <AbbreviatedView
            value={value.value}
            renderEl={el => <ValueView value={el} />}
          />
          ]
        </>
      ) : value.type == "String" ? (
        <AbbreviatedView
          value={value.value}
          renderEl={el => <>{String.fromCharCode(Number(el))}</>}
          sep={""}
        />
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
    <div className="empty-frame">(empty frame)</div>
  ) : (
    <table>
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
  useEffect(() => {
    let stepContainer = ref.current!;
    let pointers = stepContainer.querySelectorAll<HTMLSpanElement>(".pointer");
    let color = getComputedStyle(document.body).getPropertyValue("--fg")
      ? "var(--fg)"
      : "black";
    let lines = Array.from(pointers).map(src => {
      let dstSel = src.dataset.pointTo!;
      let dst = stepContainer.querySelector<HTMLElement>("." + dstSel);
      if (!dst)
        throw new Error(
          `Could not find endpoint for pointer selector: ${dstSel}`
        );
      let endSocket = dst.dataset.connector as LeaderLine.SocketType;
      let dstAnchor = dstSel.startsWith("stack")
        ? LeaderLine.pointAnchor(dst, { x: "100%", y: "75%" })
        : dst;
      return new LeaderLine(src, dstAnchor, {
        color,
        size: 1,
        endPlugSize: 2,
        startSocket: "right",
        endSocket,
        // path: "grid",
      });
    });

    let reposition = () => lines.forEach(line => line.position());
    window.addEventListener("resize", reposition);

    let interpreterContainer = container.current!;
    interpreterContainer.addEventListener("scroll", reposition);

    return () => {
      window.removeEventListener("resize", reposition);
      interpreterContainer.removeEventListener("scroll", reposition);
      lines.forEach(line => line.remove());
    };
  }, []);
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

let InterpreterView = ({ steps }: { steps: MStep<Range>[] }) => {
  let ref = useRef<HTMLDivElement>(null);
  let config = useContext(ConfigContext);
  let flexDirection: CSSProperties["flexDirection"] = config.horizontal
    ? "row"
    : "column";

  return (
    <div ref={ref} className="interpreter" style={{ flexDirection }}>
      {steps.map((step, i) => (
        <StepView key={i} index={i} step={step} container={ref} />
      ))}
    </div>
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
      <ConfigContext.Provider value={config}>
        <InterpreterView steps={steps} />
      </ConfigContext.Provider>
    </CodeContext.Provider>
  );
}
