/// <reference types="vitest" />
import fs from "node:fs";
import { defineConfig } from "vite";

let manifest = JSON.parse(fs.readFileSync("package.json", "utf-8"));
export default defineConfig(({ mode }) => ({
  define: {
    "process.env.NODE_ENV": JSON.stringify(mode)
  },
  test: {
    environment: "jsdom",
    deps: {
      inline: [/^(?!.*vitest).*$/]
    }
  }
}));
