#!/usr/bin/env node

require('esbuild')
  .build({
    logLevel: 'info',
    entryPoints: ['src/index.ts'],
    bundle: true,
    outfile: 'static/index.js',
    loader: {
      ".ts": "tsx",
      ".html": "text",
    },
  })
  .catch(() => process.exit(1))
