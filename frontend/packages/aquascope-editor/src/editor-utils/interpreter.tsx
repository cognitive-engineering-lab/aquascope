import { StateEffect } from "@codemirror/state";
import {
  Decoration,
  EditorView,
  ViewPlugin,
  WidgetType,
} from "@codemirror/view";
import LeaderLine from "leader-line-new";
import _ from "lodash";
import React, { useContext, useEffect, useRef } from "react";
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

let CodeContext = React.createContext("");

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

let AbbreviatedView = ({ value }: { value: Abbreviated<MValue> }) =>
  value.type == "All" ? (
    <>
      {intersperse(
        value.value.map((el, i) => <ValueView key={i} value={el} />),
        <>,</>
      )}
    </>
  ) : (
    <>
      {intersperse(
        value.value[0].map((el, i) => <ValueView key={i} value={el} />),
        <>,</>
      )}
      ,...,
      <ValueView value={value.value[1]} />
    </>
  );

let ValueView = ({ value }: { value: MValue }) => (
  <>
    {value.type == "Bool" ||
    value.type == "Char" ||
    value.type == "Uint" ||
    value.type == "Int" ||
    value.type == "Float" ||
    value.type == "String" ? (
      value.value.toString()
    ) : value.type == "Struct" ? (
      <>
        {value.value.name}
        <table>
          <tbody>
            {value.value.fields.map(([k, v]) => (
              <tr key={k}>
                <td>{k}</td>
                <td>
                  <ValueView value={v} />
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </>
    ) : value.type == "Enum" ? (
      <>
        {value.value.name} ({value.value.variant})
        <table>
          <tbody>
            {value.value.fields.map(([k, v]) => (
              <tr key={k}>
                <td>{k}</td>
                <td>
                  <ValueView value={v} />
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </>
    ) : value.type == "Pointer" ? (
      (() => {
        let ptr = value.value;
        let pointTo =
          ptr.type == "Heap"
            ? `.heap-${ptr.value.index}`
            : `.stack-${ptr.value.frame}-${ptr.value.local}`;
        return (
          <span className="pointer" data-point-to={pointTo}>
            ●
          </span>
        );
      })()
    ) : value.type == "Array" ? (
      <>
        [<AbbreviatedView value={value.value} />]
      </>
    ) : value.type == "Unallocated" ? (
      <>X</>
    ) : (
      <>TODO</>
    )}
  </>
);

let LocalsView = ({
  index,
  locals,
}: {
  index: number;
  locals: [string, MValue][];
}) => (
  <table>
    <tbody>
      {locals.map(([key, value], i) => (
        <tr key={i}>
          <td>{key}</td>
          <td className={`stack-${index}-${key}`}>
            <ValueView value={value} />
          </td>
        </tr>
      ))}
    </tbody>
  </table>
);

let FrameView = ({ index, frame }: { index: number; frame: MFrame<Range> }) => {
  let code = useContext(CodeContext);
  let snippet = code.slice(frame.location.char_start, frame.location.char_end);
  return (
    <div className="frame">
      <div className="frame-header">{frame.name}</div>
      {DEBUG ? <pre>{snippet}</pre> : null}
      <LocalsView index={index} locals={frame.locals} />
    </div>
  );
};

let StackView = ({ stack }: { stack: MStack<Range> }) => (
  <div className="memory stack">
    <div className="memory-header">Stack</div>
    <div className="frames">
      {stack.frames.map((frame, i) => (
        <FrameView key={i} index={i} frame={frame} />
      ))}
    </div>
  </div>
);

let HeapView = ({ heap }: { heap: MHeap }) => (
  <div className="memory heap">
    <div className="memory-header">Heap</div>
    <table>
      <tbody>
        {heap.locations.map((value, i) => (
          <tr key={i}>
            <td className={`heap-${i}`}>
              <ValueView value={value} />
            </td>
          </tr>
        ))}
      </tbody>
    </table>
  </div>
);

let StepView = ({ step, index }: { step: MStep<Range>; index: number }) => {
  let ref = useRef<HTMLDivElement | null>(null);
  useEffect(() => {
    let container = ref.current!;
    let pointers = container.querySelectorAll<HTMLSpanElement>(".pointer");
    let lines = Array.from(pointers).map(src => {
      let dstSel = src.dataset.pointTo!;
      let dst = container.querySelector(dstSel);
      if (!dst)
        throw new Error(
          `Could not find endpoint for pointer selector: ${dstSel}`
        );
      let endSocket: "right" | "left" = dstSel.startsWith(".stack")
        ? "right"
        : "left";
      return new LeaderLine(src, dst, {
        color: "black",
        size: 1,
        endPlugSize: 2,
        startSocket: "right",
        endSocket,
      });
    });
    return () => lines.forEach(line => line.remove());
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
  markedRanges: Range[]
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
    <div className="interpreter">
      <CodeContext.Provider value={contents}>
        {steps.map((step, i) => (
          <StepView key={i} index={i} step={step} />
        ))}
      </CodeContext.Provider>
    </div>
  );
}
