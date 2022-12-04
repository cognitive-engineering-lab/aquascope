import { StateEffectType, StateField } from "@codemirror/state";
import { Decoration, DecorationSet } from "@codemirror/view";

// ---------
// Constants

export const readChar = "R";
export const writeChar = "W";
export const dropChar = "O";
export const glyphWidth = 12;

// ----------
// Interfaces

export interface Icon {
  readonly display: boolean;
  toDom(): HTMLElement;
}

export interface IconField<C, Ico extends Icon, T> {
  effectType: StateEffectType<Array<T>>;
  stateField: StateField<DecorationSet>;
  makeDecoration(icos: Array<Ico>): Decoration;
  fromOutput(callTypes: C): T;
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
