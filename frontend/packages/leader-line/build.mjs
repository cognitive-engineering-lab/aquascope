import fs from "node:fs";

fs.mkdirSync("dist/bindings", {recursive: true});
fs.copyFileSync("src/bindings/leader-line.d.ts", "dist/bindings/leader-line.d.ts");
fs.copyFileSync("src/leader-line.js", "dist/leader-line.js");