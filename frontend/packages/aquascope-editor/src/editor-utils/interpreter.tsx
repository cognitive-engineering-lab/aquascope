import { Decoration, EditorView, WidgetType } from "@codemirror/view";
import classNames from "classnames";
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
  InterpAnnotations,
  MFrame,
  MHeap,
  MStack,
  MStep,
  MTrace,
  MUndefinedBehavior,
  MValue,
  Range,
} from "../types";
import { evenlySpaceAround, makeDecorationField } from "./misc";

const DEBUG: boolean = false;

export interface InterpreterConfig {
  horizontal?: boolean;
  concreteTypes?: boolean;
  hideCode?: boolean;
}

let ConfigContext = React.createContext<InterpreterConfig>({});
let CodeContext = React.createContext("");
let PathContext = React.createContext<string[]>([]);
let ErrorContext = React.createContext<MUndefinedBehavior | undefined>(
  undefined
);

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

type MValueAdt = MValue & { type: "Adt" };
type MAdt = MValueAdt["value"];

type MValuePointer = MValue & { type: "Pointer" };
type MPointer = MValuePointer["value"];

let read_field = (v: MAdt, k: string): MAdt => {
  let field = v.fields.find(([k2]) => k == k2);
  if (!field) {
    let v_pretty = JSON.stringify(v, undefined, 2);
    throw new Error(`Could not find field "${k}" in struct: ${v_pretty}`);
  }
  return (field[1] as MValueAdt).value;
};

let read_unique = (unique: MAdt): MAdt => {
  let non_null = read_field(unique, "pointer");
  return non_null;
};

let read_vec = (vec: MAdt): MAdt => {
  let raw_vec = read_field(vec, "buf");
  let unique = read_field(raw_vec, "ptr");
  return read_unique(unique);
};

let AdtView = ({ value }: { value: MAdt }) => {
  let pathCtx = useContext(PathContext);
  let config = useContext(ConfigContext);

  if (value.alloc_kind !== null && !config.concreteTypes) {
    let alloc_type = value.alloc_kind.type;
    let non_null: MAdt;
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

  if (value.name == "Iter" && !config.concreteTypes) {
    let non_null = read_field(value, "ptr");
    let ptr = non_null.fields[0][1];
    return <ValueView value={ptr} />;
  }

  let adtName = (
    <>
      {value.name}
      {value.variant ? <>::{value.variant}</> : null}
    </>
  );

  let isTuple = value.fields.length > 0 && value.fields[0][0] == "0";

  if (isTuple && value.fields.length == 1) {
    let path = [...pathCtx, "field", "0"];
    return (
      <span className={path.join("-")}>
        <PathContext.Provider value={path}>
          {adtName} /&nbsp;
          <ValueView value={value.fields[0][1]} />
        </PathContext.Provider>
      </span>
    );
  }

  let cells = value.fields.map(([k, v], i) => {
    let path = [...pathCtx, "field", i.toString()];
    return (
      <td key={k} className={path.join("-")}>
        <PathContext.Provider value={path}>
          <ValueView value={v} />
        </PathContext.Provider>
      </td>
    );
  });

  return (
    <>
      {adtName}
      <table>
        <tbody>
          {isTuple ? (
            <tr>{cells}</tr>
          ) : (
            value.fields.map(([k, _v], i) => (
              <tr key={i}>
                <td>{k}</td>
                {cells[i]}
              </tr>
            ))
          )}
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
  let error = useContext(ErrorContext);
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
      ) : value.type == "Adt" ? (
        <AdtView value={value.value} />
      ) : value.type == "Pointer" ? (
        <PointerView value={value.value} />
      ) : value.type == "Array" ? (
        <AbbreviatedView value={value.value} />
      ) : value.type == "Unallocated" ? (
        (() => {
          let isError =
            error &&
            error.type == "PointerUseAfterFree" &&
            error.value.alloc_id == value.value.alloc_id;
          return (
            <span className={classNames("unallocated", { error: isError })}>
              ⦻
            </span>
          );
        })()
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
  <div className={`header ${className ?? ""}`}>
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

const PALETTE = {
  // sns.color_palette("rocket", 15)[:6]
  light: ["#221331", "#451c47", "#691f55", "#921c5b", "#b91657", "#d92847"],
  // sns.color_palette("rocket_r", 20, desat=0.5).as_hex()[:6]
  dark: ["#ebdbd0", "#e3cbbc", "#dcbca9", "#d6ac98", "#d19d88", "#cb8c7a"],
};

let renderArrows = (
  containerRef: React.RefObject<HTMLDivElement>,
  stepContainerRef: React.RefObject<HTMLDivElement>,
  arrowContainerRef: React.RefObject<HTMLDivElement>
) => {
  let config = useContext(ConfigContext);
  useEffect(() => {
    let container = containerRef.current!;
    let stepContainer = stepContainerRef.current!;
    let arrowContainer = arrowContainerRef.current!;

    let sources = stepContainer.querySelectorAll<HTMLSpanElement>(".pointer");

    // TODO: this should be configurable from the embed script, not directly
    // inside aquascope-editor
    let mdbookEmbed = getComputedStyle(document.body).getPropertyValue(
      "--inline-code-color"
    );

    let query = (sel: string): HTMLElement => {
      let dst = stepContainer.querySelector<HTMLElement>("." + CSS.escape(sel));
      if (!dst)
        throw new Error(
          `Could not find endpoint for pointer selector: ${CSS.escape(sel)}`
        );
      return dst;
    };

    type MemoryRegion = "stack" | "heap";
    let getMemoryRegion = (el: HTMLElement): MemoryRegion =>
      el.closest(".heap") !== null ? "heap" : "stack";

    interface Pointer {
      src: HTMLElement;
      dst: HTMLElement;
      dstSel: string;
      dstRange?: HTMLElement;
      endSocket: LeaderLine.SocketType;
      dstIndex: number;
      group: {
        srcRegion: MemoryRegion;
        dstRegion: MemoryRegion;
      };
    }

    // First, collect metadata about all the pointers we're rendering
    // like what HTML elements are pointed, and what region of the digram
    // they lie in.
    let dstCounts: { [sel: string]: number } = {};
    let pointers = Array.from(sources).map<Pointer>(src => {
      let dstSel = src.dataset.pointTo!;
      let dst = query(dstSel);
      let dstRange = src.dataset.pointToRange
        ? query(src.dataset.pointToRange)
        : undefined;
      let endSocket = dst.dataset.connector as LeaderLine.SocketType;
      let group = {
        srcRegion: getMemoryRegion(src),
        dstRegion: getMemoryRegion(dst),
      };

      if (!(dstSel in dstCounts)) dstCounts[dstSel] = 0;
      let dstIndex = dstCounts[dstSel];
      dstCounts[dstSel] += 1;

      return { src, dst, dstRange, dstSel, endSocket, dstIndex, group };
    });

    // Then, group the pointers by their regions.
    // That way we know how many pointers are e.g. pointing from stack->heap
    // so we can stagger them correctly.
    let groups = _.groupBy(pointers, "group");

    interface RenderedPointer {
      line: LeaderLine;
      svgElements: Element[];
    }

    // Then we render each pointer, conditioned on its group.
    let renderPtr = (ptr: Pointer, i: number): RenderedPointer | undefined => {
      try {
        let { srcRegion, dstRegion } = ptr.group;

        // Heap -> stack pointers should start on the left and
        // everything else starts on the right
        let startSocket: LeaderLine.SocketType =
          srcRegion == "heap" && dstRegion == "stack" ? "left" : "right";

        let dstAnchor: LeaderLine.AnchorAttachment;
        if (ptr.dstRange) {
          // Pointers to ranges (eg string slices) need an area anchor
          // corresponding to the range of the slice
          // TODO: this doesn't handle abbreviations
          dstAnchor = LeaderLine.areaAnchor(ptr.dst, {
            shape: "rect",
            width:
              ptr.dstRange.offsetLeft +
              ptr.dst.offsetWidth -
              ptr.dst.offsetLeft,
            height: 2,
            y: "100%",
            fillColor: mdbookEmbed ? "var(--search-mark-bg)" : "red",
          });
        } else if (srcRegion == "stack" && dstRegion == "stack") {
          // Stack -> stack pointers should point a little below the middle
          // to avoid conflicting with outgoing pointers.
          dstAnchor = LeaderLine.pointAnchor(ptr.dst, { x: "100%", y: "75%" });
        } else if (ptr.endSocket == "bottom") {
          dstAnchor = ptr.dst;
        } else {
          let x = dstRegion == "stack" ? 100 : 0;

          // Everything else should get evenly spaced around the
          // middle of the endpoint
          let y = evenlySpaceAround({
            center: 50,
            spacing: 30,
            index: ptr.dstIndex,
            total: dstCounts[ptr.dstSel] - 1,
          });

          dstAnchor = LeaderLine.pointAnchor(ptr.dst, {
            x: `${x}%`,
            y: `${y}%`,
          });
        }

        const MDBOOK_DARK_THEMES = ["navy", "coal", "ayu"];
        let isDark = MDBOOK_DARK_THEMES.some(s =>
          document.documentElement.classList.contains(s)
        );
        let theme: "dark" | "light" = isDark ? "dark" : "light";
        let palette = PALETTE[theme];
        let color = palette[i % palette.length];

        let startSocketGravity = undefined;
        let endSocketGravity = undefined;
        if (ptr.group.srcRegion == "stack" && ptr.group.dstRegion == "heap") {
          startSocketGravity = 60;
          endSocketGravity = 100 - i * 10;
        }

        let line = new LeaderLine(ptr.src, dstAnchor, {
          color,
          size: 1,
          endPlugSize: 2,
          startSocket,
          endSocket: ptr.endSocket,
          startSocketGravity,
          endSocketGravity,
        });

        // Make arrows local to the diagram rather than global in the body
        // See: https://github.com/anseki/leader-line/issues/54
        let svgSelectors = [".leader-line"];
        if (ptr.dstRange) svgSelectors.push(".leader-line-areaAnchor");
        let svgElements = svgSelectors.map(sel => {
          let el = document.body.querySelector(`:scope > ${sel}`);
          if (!el) throw new Error(`Missing LineLeader element: ${sel}`);
          return el;
        });

        svgElements.forEach(el => arrowContainer.appendChild(el));

        return { line, svgElements };
      } catch (e: any) {
        console.error("Leader line failed to render", e.stack);
        return undefined;
      }
    };

    let lines = Object.entries(groups)
      .flatMap(([_g, ptrs]) => ptrs.map((ptr, i) => renderPtr(ptr, i)))
      .filter(obj => obj !== undefined) as RenderedPointer[];

    // Lastly, we add a timer to reposition the arrow container
    // if necessary.
    let curCoords = (): [number, number] => {
      let stepBox = stepContainer.getBoundingClientRect();
      let x = stepBox.left + window.scrollX + container.scrollLeft;
      let y = stepBox.top + window.scrollY + container.scrollTop;
      return [x, y];
    };

    let positionArrowContainer = (x: number, y: number) => {
      lines.forEach(({ line }) => line.position());
      arrowContainer.style.transform = `translate(-${x}px, -${y}px)`;
    };
    let lastCoords = curCoords();
    positionArrowContainer(...lastCoords);

    let interval = setInterval(() => {
      let newCoords = curCoords();
      if (newCoords[0] != lastCoords[0] || newCoords[1] != lastCoords[1]) {
        positionArrowContainer(...newCoords);
      }
      lastCoords = newCoords;
    }, 300);

    return () => {
      clearInterval(interval);
      lines.forEach(({ svgElements }) => {
        svgElements.forEach(el => {
          el.parentNode!.removeChild(el);
        });
      });
    };
  });
  // Note: this effect must be re-run every time, since children *might* change
  // their contents and invalidate DOM references held within LineLeader
};

let StepView = ({
  step,
  index,
  containerRef,
}: {
  step: MStep<Range>;
  index: number;
  containerRef: React.RefObject<HTMLDivElement>;
}) => {
  let stepContainerRef = useRef<HTMLDivElement>(null);
  let arrowContainerRef = useRef<HTMLDivElement>(null);
  let error = useContext(ErrorContext);
  renderArrows(containerRef, stepContainerRef, arrowContainerRef);

  return (
    <div className="step">
      <div className="step-header">
        <StepMarkerView index={index} fail={error !== undefined} />
        {error !== undefined ? (
          <span className="undefined-behavior">
            undefined behavior:{" "}
            {error.type == "PointerUseAfterFree" ? (
              <>pointer used after its pointee is freed</>
            ) : (
              error.value
            )}
          </span>
        ) : null}
      </div>
      <div className="memory-container" ref={stepContainerRef}>
        <div className="arrow-container" ref={arrowContainerRef} />
        <div className="memory-container-flex">
          <StackView stack={step.stack} />
          {step.heap.locations.length > 0 ? (
            <HeapView heap={step.heap} />
          ) : null}
        </div>
      </div>
    </div>
  );
};

let InterpreterView = ({
  trace,
  config,
}: {
  trace: MTrace<Range>;
  config?: InterpreterConfig;
}) => {
  let ref = useRef<HTMLDivElement>(null);
  let [concreteTypes, setConcreteTypes] = useState(
    config?.concreteTypes ?? false
  );
  let [buttonVisible, setButtonVisible] = useState(false);

  let flexDirection: CSSProperties["flexDirection"] = config?.horizontal
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
          className={classNames("concrete-types", { active: concreteTypes })}
          onClick={() => setConcreteTypes(!concreteTypes)}
          style={{ opacity: buttonVisible ? "1" : "0" }}
        >
          <i className="fa fa-binoculars" />
        </button>
        {trace.steps.map((step, i) => {
          let error =
            i == trace.steps.length - 1 && trace.result.type == "Error"
              ? trace.result.value
              : undefined;
          return (
            <ErrorContext.Provider key={i} value={error}>
              <StepView index={i} step={step} containerRef={ref} />
            </ErrorContext.Provider>
          );
        })}
      </div>
    </ConfigContext.Provider>
  );
};

let filterSteps = (
  steps: MStep<Range>[],
  marks: number[]
): [number[], MStep<Range>[]] => {
  let stepsRev = [...steps].reverse();
  let indexedMarks: [number, number, MStep<Range>][] = marks.map(idx => {
    let stepRevIdx = stepsRev.findIndex(step => {
      let frame = _.last(step.stack.frames)!;
      let markInFrame =
        frame.body_span.char_start <= idx && idx <= frame.body_span.char_end;
      let markAfterLoc = idx > frame.location.char_start;
      return markInFrame && markAfterLoc;
    });
    if (stepRevIdx == -1)
      throw new Error(
        `Could not find step for range: ${JSON.stringify(idx, undefined, 2)}`
      );
    return [steps.length - stepRevIdx, idx, stepsRev[stepRevIdx]];
  });
  let sortedMarks = _.sortBy(indexedMarks, ([idx]) => idx);
  return [
    sortedMarks.map(([_stepIdx, mark]) => mark),
    sortedMarks.map(([_stepIdx, _mark, step]) => step),
  ];
};

let StepMarkerView = ({ index, fail }: { index: number; fail: boolean }) => {
  return (
    <span className={classNames("step-marker", { fail })}>
      <span>L{index + 1}</span>
    </span>
  );
};

class StepMarkerWidget extends WidgetType {
  constructor(readonly index: number, readonly fail: boolean) {
    super();
  }

  toDOM() {
    let container = document.createElement("span");
    ReactDOM.createRoot(container).render(
      <StepMarkerView index={this.index} fail={this.fail} />
    );
    return container;
  }
}

export let markerField = makeDecorationField();

export function renderInterpreter(
  view: EditorView,
  container: HTMLDivElement,
  trace: MTrace<Range>,
  contents: string,
  config?: InterpreterConfig,
  annotations?: InterpAnnotations
) {
  let root = ReactDOM.createRoot(container);
  let marks = annotations?.state_locations || [];
  let widgetRanges;
  if (marks.length > 0) {
    let [sortedMarks, filteredSteps] = filterSteps(trace.steps, marks);
    widgetRanges = sortedMarks;
    trace.steps = filteredSteps;
  } else {
    widgetRanges = trace.steps.map(
      step => _.last(step.stack.frames)!.location.char_end
    );
  }

  let decos = widgetRanges.map((mark, i) =>
    Decoration.widget({
      widget: new StepMarkerWidget(
        i,
        i == trace.steps.length - 1 && trace.result.type == "Error"
      ),
    }).range(mark)
  );

  view.dispatch({
    effects: [markerField.setEffect.of(decos)],
  });

  root.render(
    <CodeContext.Provider value={contents}>
      <InterpreterView trace={trace} config={config} />
    </CodeContext.Provider>
  );
}
