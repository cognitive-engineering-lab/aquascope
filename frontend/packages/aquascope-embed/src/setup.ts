import { defaultKeymap, history, historyKeymap } from "@codemirror/commands";
import { HighlightStyle, syntaxHighlighting } from "@codemirror/language";
import type { Extension } from "@codemirror/state";
import {
  EditorView,
  drawSelection,
  highlightSpecialChars,
  keymap
} from "@codemirror/view";
import { type Tag, tags as t } from "@lezer/highlight";

let tf = (tag: Tag, hljs: string) => ({ tag, class: `hljs-${hljs}` });
let tt = (cm: keyof typeof t, hljs: string) => tf(t[cm] as any, hljs);

let highlightStyle = HighlightStyle.define([
  tt("comment", "comment"),
  tt("lineComment", "comment"),
  tt("blockComment", "comment"),
  tt("docComment", "comment"),
  // tt("variableName", "variable"),
  tt("typeName", "type"),
  tt("tagName", "type"),
  tt("literal", "literal"),
  tt("string", "string"),
  tt("docString", "string"),
  tt("number", "number"),
  tt("integer", "number"),
  tt("float", "number"),
  tt("regexp", "regexp"),
  tt("keyword", "keyword"),
  tt("self", "variable.language"),
  tt("operator", "operator"),
  tf(t.function(t.definition(t.variableName)), "title")
]);

let suppressKeyEvents = () => {
  let captureKeyboard = (e: KeyboardEvent) => e.stopPropagation();
  return EditorView.domEventHandlers({
    focusin: () => {
      document.documentElement.addEventListener(
        "keydown",
        captureKeyboard,
        false
      );
    },
    focusout: () => {
      document.documentElement.removeEventListener(
        "keydown",
        captureKeyboard,
        false
      );
    }
  });
};

export let setup: Extension = (() => [
  highlightSpecialChars(),
  history(),
  drawSelection(),
  syntaxHighlighting(highlightStyle, { fallback: true }),
  keymap.of([...defaultKeymap, ...historyKeymap]),
  suppressKeyEvents()
])();
