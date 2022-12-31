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
      <pre>{snippet}</pre>
      <LocalsView locals={frame.locals} />
    </div>
  );
};

let StackView = ({ stack }: { stack: MStack<Range> }) => (
  <div className="stack">
    <h2>Stack</h2>
    {stack.frames.map((frame, i) => (
      <FrameView key={i} frame={frame} />
    ))}
  </div>
);

let HeapView = ({ heap }: { heap: MHeap }) => (
  <div className="heap">
    <h2>Heap</h2>
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

let StepView = ({ step }: { step: MStep<Range> }) => {
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
      <div ref={stackRef}>
        <StackView stack={step.stack} />
      </div>
      <div ref={heapRef}>
        <HeapView heap={step.heap} />
      </div>
    </div>
  );
};

let filterSteps = (steps: MStep<Range>[], ranges: Range[]): MStep<Range>[] =>
  steps.filter(step => {
    let lastFrame = _.last(step.stack.frames)!;
    let loc = lastFrame.location;
    return _.some(
      ranges,
      other =>
        loc.char_start <= other.char_start && other.char_end <= loc.char_end
    );
  });

export function renderInterpreter(
  container: HTMLDivElement,
  steps: MStep<Range>[],
  contents: string,
  markedRanges: Range[]
) {
  let root = ReactDOM.createRoot(container);
  if (markedRanges.length > 0) {
    steps = filterSteps(steps, markedRanges);
  }

  root.render(
    <div className="interpreter">
      <CodeContext.Provider value={contents}>
        {steps.map((step, i) => (
          <StepView key={i} step={step} />
        ))}
      </CodeContext.Provider>
    </div>
  );
}
