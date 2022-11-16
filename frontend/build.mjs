#!/usr/bin/env node
import esbuild from "esbuild";

esbuild.build({
  logLevel: "info",
  entryPoints: ["src/index.ts", "src/embed.ts"],
  bundle: true,
  watch: process.argv.includes("-w"),
  outdir: "dist",
});
