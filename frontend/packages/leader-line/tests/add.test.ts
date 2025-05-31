import { expect, test } from "vitest";

import { add } from "../src/lib";

test("add", () => expect(add(2, 2)).toBe(4));
