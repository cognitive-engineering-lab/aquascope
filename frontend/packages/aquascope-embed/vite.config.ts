import fs from "fs";
import react from "@vitejs/plugin-react";
import { defineConfig } from "vite";
import { resolve } from "path";

let manifest = JSON.parse(fs.readFileSync("package.json", "utf-8"));
export default defineConfig(({mode}) => ({
  build: {
    lib: {
      entry: resolve(__dirname, "src/main.tsx"),
      name: "AquascopeEmbed",
      formats: ["iife"],
    },
    rollupOptions: {
      external: Object.keys(manifest.dependencies || {})
    }
  },
  define: {
    "process.env.NODE_ENV": JSON.stringify(mode),
  },
  plugins: [react()],
}));
