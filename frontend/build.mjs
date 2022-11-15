#!/usr/bin/env node

import esbuild from "esbuild";

esbuild.build({
  logLevel: "info",
  entryPoints: ["src/index.ts"],
  bundle: true,
  watch: process.argv.includes("-w"),
  outfile: "static/index.js",
});
