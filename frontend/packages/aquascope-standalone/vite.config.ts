import react from "@vitejs/plugin-react";
import * as cp from "child_process";
import { defineConfig } from "vite";

let serverUrl = process.env["AQUASCOPE_SERVER"] ?? "http://127.0.0.1:8008";

// https://vitejs.dev/config/
export default defineConfig({
  base: "./",
  define: {
    SERVER_URL: JSON.stringify(serverUrl),
  },
  plugins: [react()],
  build: { emptyOutDir: false },
});
