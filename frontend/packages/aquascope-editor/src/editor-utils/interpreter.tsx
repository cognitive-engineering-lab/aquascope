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
      <span className="pointer" data-point-to={value.value}>
        *
      </span>
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

let LocalsView = ({ locals }: { locals: [string, MValue][] }) => (
  <table>
    <tbody>
      {locals.map(([key, value], i) => (
        <tr key={i}>
          <td>{key}</td>
          <td>
            <ValueView value={value} />
          </td>
        </tr>
      ))}
    </tbody>
  </table>
);

let FrameView = ({ frame }: { frame: MFrame<Range> }) => {
  let code = useContext(CodeContext);
  let snippet = code.slice(frame.location.char_start, frame.location.char_end);
  return (
    <div className="frame">
      <div>{frame.name}</div>
      {DEBUG ? <pre>{snippet}</pre> : null}
      <LocalsView locals={frame.locals} />
    </div>
  );
};

let StackView = ({ stack }: { stack: MStack<Range> }) => (
  <div className="memory stack">
    <div className="memory-header">Stack</div>
    {stack.frames.map((frame, i) => (
      <FrameView key={i} frame={frame} />
    ))}
  </div>
);

let HeapView = ({ heap }: { heap: MHeap }) => (
  <div className="memory heap">
    <div className="memory-header">Heap</div>
    <table>
      <tbody>
        {heap.locations.map(([loc, value], i) => (
          <tr key={i}>
            <td>
              <ValueView key={loc} value={value} />
            </td>
          </tr>
        ))}
      </tbody>
    </table>
  </div>
);

let StepView = ({ step, index }: { step: MStep<Range>; index: number }) => {
  let stackRef = useRef<HTMLDivElement | null>(null);
  let heapRef = useRef<HTMLDivElement | null>(null);
  useEffect(() => {
    let pointers =
      stackRef.current!.querySelectorAll<HTMLSpanElement>(".pointer");
    let lines = Array.from(pointers).map(ptr => {
      let i = parseInt(ptr.dataset.pointTo!);
      return new LeaderLine(
        ptr,
        heapRef.current!.querySelector(`tr:nth-child(${i + 1})`)!,
        {
          color: "black",
          size: 1,
          endPlugSize: 2,
        }
      );
    });
    return () => lines.forEach(line => line.remove());
  }, []);
  return (
    <div className="step">
      <div className="step-name">L{index + 1}</div>
      <div className="memory-container">
        <div ref={stackRef}>
          <StackView stack={step.stack} />
        </div>
        {step.heap.locations.length > 0 ? (
          <div ref={heapRef}>
            <HeapView heap={step.heap} />
          </div>
        ) : null}
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
