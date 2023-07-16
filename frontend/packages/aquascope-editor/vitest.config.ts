/// <reference types="vitest" />
import react from "@vitejs/plugin-react";
import { defineConfig } from "vite";

export default defineConfig(({mode}) => ({
  define: {
    "process.env.NODE_ENV": JSON.stringify(mode),
  },
  plugins: [react()],
  test: {
    environment: "jsdom",
    setupFiles: "tests/setup.ts",
    deps: {
      inline: [/^(?!.*vitest).*$/],
    },
  },
}));
