import {
  Range,
  RangeSet,
  StateEffect,
  StateEffectType,
  StateField,
} from "@codemirror/state";
import { Decoration, DecorationSet, EditorView } from "@codemirror/view";
import _ from "lodash";

import {
  AnalysisFacts,
  AnalysisOutput,
  LoanKey,
  Range as RangeT,
  RefinementRegion,
} from "../types";

// ---------
// Constants

export const readChar = "R";
export const writeChar = "W";
export const dropChar = "O";

// ----------
// Interfaces

export interface HTMLIcon {
  readonly display: boolean;
  toDOM(): HTMLElement;
}

export interface ReactIcon {
  readonly display: boolean;
  render(): JSX.Element;
}

export interface IconField<C, T> {
  effectType: StateEffectType<Array<T>>;
  stateField: StateField<DecorationSet>;
  fromOutput(o: C, facts: AnalysisFacts): T;
}

// ------------
// Render utils

export class RGB {
  constructor(readonly r: number, readonly g: number, readonly b: number) {}
  toString(): string {
    return `rgb(${this.r},${this.g},${this.b})`;
  }
  withAlpha(a: number): RGBA {
    return new RGBA(this.r, this.g, this.b, a);
  }
}

export class RGBA {
  constructor(
    readonly r: number,
    readonly g: number,
    readonly b: number,
    readonly a: number
  ) {}
  toString(): string {
    return `rgba(${this.r},${this.g},${this.b},${this.a})`;
  }
  withAlpha(newA: number): RGBA {
    return new RGBA(this.r, this.g, this.b, newA);
  }
}

export type Color = RGB | RGBA;

export const softRed: RGB = new RGB(255, 66, 68);
export const softGreen: RGB = new RGB(93, 202, 54);
export const softBlue: RGB = new RGB(78, 190, 239);
export const softYellow: RGB = new RGB(238, 238, 155);
export const softOrange: RGB = new RGB(245, 202, 123);
export const whiteColor: RGB = new RGB(255, 255, 255);

export const dropColor = softRed;
export const readColor = softGreen;
export const writeColor = softBlue;

// ---------
// Utilities

export let makeTag = (length: number) => {
  var result = "";
  var characters =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  var charactersLength = characters.length;
  for (var i = 0; i < length; i++) {
    result += characters.charAt(Math.floor(Math.random() * charactersLength));
  }
  return "tag" + result;
};

export let genStateField = <T>(
  ty: StateEffectType<T>,
  transform: (t: T) => Array<Range<Decoration>>
): StateField<DecorationSet> =>
  StateField.define<DecorationSet>({
    create: () => Decoration.none,

    update(points, transactions) {
      for (let e of transactions.effects) {
        if (e.is(ty)) {
          // Sort the values by default          vvvv
          return RangeSet.of(transform(e.value), true);
        }
      }

      return transactions.docChanged ? RangeSet.of([]) : points;
    },

    provide: f => EditorView.decorations.from(f),
  });

export type LoanFacts = {
  refinerTag: string;
  regionTag: string;
  refinerPoint: RangeT;
  region: RefinementRegion;
};

export const loanFactsStateType = StateEffect.define<Array<LoanFacts>>();

export const loanFactsField = genStateField<Array<LoanFacts>>(
  loanFactsStateType,
  (ts: Array<LoanFacts>): Array<Range<Decoration>> =>
    ts.flatMap(loanFactsToDecoration)
);

function loanFactsToDecoration({
  refinerTag,
  regionTag,
  refinerPoint,
  region,
}: LoanFacts): Array<Range<Decoration>> {
  let loanDeco = Decoration.mark({
    class: "aquascope-loan",
    tagName: refinerTag,
  }).range(refinerPoint.char_start, refinerPoint.char_end);

  let regionDecos = region.refined_ranges
    .filter(range => range.char_start != range.char_end)
    .map(range => {
      let highlightedRange = Decoration.mark({
        class: "aquascope-live-region",
        tagName: regionTag,
      }).range(range.char_start, range.char_end);
      return highlightedRange;
    });

  return [loanDeco, ...regionDecos];
}

export function generateAnalysisDecorationFacts<T>(
  output: AnalysisOutput<T>
): [AnalysisFacts, Array<LoanFacts>] {
  let points: Record<LoanKey, string> = {};
  let regions: Record<LoanKey, string> = {};

  let stateFacts = [];

  for (const loan in output.loan_regions) {
    let loanTag = makeTag(26);
    let regionTag = makeTag(26);
    let refinedRegion = output.loan_regions[loan];

    // TODO: the refined region stores whether it is for a Loan or Move.
    // When moves get reported this needs to be updated accordingly to
    // accommodate for this scenario.

    let loanPoint = output.loan_points[loan];

    points[loan] = loanTag;
    regions[loan] = regionTag;

    let loanFacts = {
      refinerTag: loanTag,
      regionTag: regionTag,
      refinerPoint: loanPoint,
      region: refinedRegion,
    };
    stateFacts.push(loanFacts);
  }

  let facts = {
    loanPoints: points,
    loanRegions: regions,
  };

  return [facts, stateFacts];
}

export let hideLine = StateEffect.define<{ line: number }>();
let hiddenLineClass = Decoration.line({ class: "hidden-line" });
export let hiddenLines = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update(decos, tr) {
    decos = decos.map(tr.changes);

    for (let e of tr.effects)
      if (e.is(hideLine))
        decos = decos.update({
          add: [hiddenLineClass.range(tr.state.doc.line(e.value.line).from)],
        });

    return decos;
  },
  provide: f => EditorView.decorations.from(f),
});

let forCustomTag = (tag: string, callback: (e: HTMLElement) => void) => {
  Array.from(
    document.getElementsByTagName(tag) as HTMLCollectionOf<HTMLElement>
  ).forEach(callback);
};

export let showLoanRegion = (
  facts: AnalysisFacts,
  key?: LoanKey,
  names: string[] = []
) => {
  if (key !== undefined) {
    const loanTag = facts.loanPoints[key];
    const regionTag = facts.loanRegions[key];

    forCustomTag(loanTag, elem => {
      elem.classList.add("show-hidden");
      names.forEach((n: string) => elem.classList.add(n));
    });
    forCustomTag(regionTag, elem => {
      elem.classList.add("show-hidden");
      names.forEach((n: string) => elem.classList.add(n));
    });
  }
};

export let hideLoanRegion = (
  facts: AnalysisFacts,
  key?: LoanKey,
  names: string[] = []
) => {
  if (key !== undefined) {
    const loanTag = facts.loanPoints[key];
    const regionTag = facts.loanRegions[key];

    forCustomTag(loanTag, elem => {
      elem.classList.remove("show-hidden");
      names.forEach((n: string) => elem.classList.remove(n));
    });
    forCustomTag(regionTag, elem => {
      elem.classList.remove("show-hidden");
      names.forEach((n: string) => elem.classList.remove(n));
    });
  }
};
