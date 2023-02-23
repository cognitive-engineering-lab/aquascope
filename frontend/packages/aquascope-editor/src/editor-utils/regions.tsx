import { Line, Range } from "@codemirror/state";
import { Decoration, EditorView, WidgetType } from "@codemirror/view";
import classNames from "classnames";
import _ from "lodash";
import React, { useState } from "react";
import ReactDOM from "react-dom/client";

import {
    RegionViolation,
    RegionViolationKind,
    RegionFlow,
    Range as SRange,
} from "../types";
import {
  dropChar,
  hideLoanRegion,
  hideMoveRegion,
  makeDecorationField,
  readChar,
  showLoanRegion,
  showMoveRegion,
  writeChar,
} from "./misc";

export let regionField = makeDecorationField();

function makeLocalOutlives(localBindingRange: SRange, abstractRange: SRange, flowRanges: SRange[]): Range<Decoration>[] {
    let rangeDecos = flowRanges.map(range => {
        return Decoration.mark({
            class: "aquascope-region-flow",
        }).range(range.char_start, range.char_end);
    });

    let localBinder = Decoration.mark({
        class: "aquascope-escaping-local",
    }).range(localBindingRange.char_start, localBindingRange.char_end);

    let escapingRegion = Decoration.mark({
        class: "aquascope-escaping-region",
    }).range(abstractRange.char_start, abstractRange.char_end);

    return [...rangeDecos, localBinder, escapingRegion];
}

export function makeRegionDecorations(
    view: EditorView,
    regionViolation?: RegionViolation,
): Range<Decoration>[] {
    if (regionViolation === undefined) {
        return [];
    }

    if (regionViolation.kind.type === "LocalOutlivesUniversal") {
        let localBindingRange = regionViolation.kind.local_binding_range;
        let abstractRange = regionViolation.kind.abstract_range;
        return makeLocalOutlives(localBindingRange, abstractRange, regionViolation.flow_ranges);
    } else if (regionViolation.kind.type === "OutlivesConstraintMissing") {
        let fromRange = regionViolation.kind.longer;
        let toRange = regionViolation.kind.shorter;
        return makeLocalOutlives(fromRange, toRange, regionViolation.flow_ranges);
    } else {
        console.error("Unknown region violation kind: ", regionViolation.kind)
        return [];
    }
}
