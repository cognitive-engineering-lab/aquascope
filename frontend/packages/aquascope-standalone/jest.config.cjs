/** @type {import('ts-jest').JestConfigWithTsJest} */

const merge = require('merge')
const { jsWithTsESM: tsjPreset } = require('ts-jest/presets')
const puppeteerPreset = require('jest-puppeteer/jest-preset')

module.exports = merge.recursive(tsjPreset, puppeteerPreset, {
  // preset: "ts-jest/presets/js-with-ts-esm",
  // preset: "jest-puppeteer",
  globals: {
    URL: "<http://localhost:8000>"
  },
  testEnvironment: "node",
  verbose: true,
  maxWorkers: 2,
  roots: ['<rootDir>/tests']
});
