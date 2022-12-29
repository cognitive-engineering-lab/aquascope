import LeaderLine from "leader-line-new";
import React, { useEffect, useRef } from "react";
import ReactDOM from "react-dom/client";

import { MFrame, MHeap, MStack, MStep, MValue } from "../types";

let ValueView = ({ value }: { value: MValue }) => (
  <div>
    {value.type == "Bool" ||
    value.type == "Char" ||
    value.type == "Uint" ||
    value.type == "Int" ||
    value.type == "Float" ? (
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
    ) : value.type == "Pointer" ? (
      <span className="pointer" data-point-to={value.value}>
        *
      </span>
    ) : (
      "<TBD>"
    )}
  </div>
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

let FrameView = ({ frame }: { frame: MFrame<Range> }) => (
  <div className="frame">
    <div>{frame.name}</div>
    <LocalsView locals={frame.locals} />
  </div>
);

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
        {heap.locations.map(([loc, value]) => (
          <tr>
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
    console.log(stackRef);
    let pointers =
      stackRef.current!.querySelectorAll<HTMLSpanElement>(".pointer");
    pointers.forEach(ptr => {
      let i = parseInt(ptr.dataset.pointTo!);
      new LeaderLine(
        ptr,
        heapRef.current!.querySelector(`tr:nth-child(${i+1})`)!,
        {
          color: "black",
          size: 1,
          endPlugSize: 2,
        }
      );
    });
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

export function renderInterpreter(
  container: HTMLDivElement,
  steps: MStep<Range>[]
) {
  let root = ReactDOM.createRoot(container);
  root.render(
    <div className="interpreter">
      {steps.map((step, i) => (
        <StepView key={i} step={step} />
      ))}
    </div>
  );
}
