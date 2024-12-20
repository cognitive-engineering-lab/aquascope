import {
  type Browser,
  type BrowserContext,
  type Page,
  chromium
} from "playwright";
import { type PreviewServer, preview } from "vite";
import { afterAll, beforeAll, beforeEach, describe, it } from "vitest";

const permStackSelector = ".permission-stack";
const permStepSelector = ".step-widget-container";
const _interpSelector = ".interpreter";

describe("Aquascope Standalone", () => {
  let browser: Browser;
  let page: Page;
  let server: PreviewServer;
  let context: BrowserContext;

  beforeAll(async () => {
    server = await preview({ preview: { port: 8000 } });
    // NOTE: uncomment lines to debug playrwright tests
    browser = await chromium.launch({
      // headless: false,
      // args: ["--start-maximized"],
      // slowMo: 250
    });
    context = await browser.newContext({
      userAgent:
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/66.0.3359.181 Safari/537.36"
    });

    page = await context.newPage();
    await page.setExtraHTTPHeaders({
      "Accept-Language": "en-GB,en-US;q=0.9,en;q=0.8"
    });
  });

  afterAll(async () => {
    await browser.close();
    await new Promise<void>((resolve, reject) => {
      server.httpServer.close(error => (error ? reject(error) : resolve()));
    });
  });

  beforeEach(async () => {});

  it("does nothing", async () => {
    // TODO: uncomment tests when the standalone is stable (post-WASM port)
  });

  // it("runs permissions", async () => {
  //   let permBtn = await page.$("#showPermissions");
  //   expect(permBtn).toBeDefined();
  //   await permBtn!.click();

  //   await page.waitForSelector(permStackSelector);
  //   let stackWidgets = await page.$$(permStackSelector);
  //   expect(stackWidgets.length).toBeGreaterThan(0);

  //   await page.waitForSelector(permStepSelector);
  //   let stepWidgets = await page.$$(permStepSelector);
  //   expect(stepWidgets.length).toBeGreaterThan(0);

  //   let crashedElement = await page.$(".aquascope-crash");
  //   // No crashed elements
  //   expect(crashedElement).toBeNull();
  // }, 60_000);

  // it("runs the interpreter", async () => {
  //   await page.click("#showInterpret");
  //   let crashedElement = await page.$(".aquascope-crash");
  //   // No crashed elements
  //   expect(crashedElement).toBeNull();
  //   await page.waitForSelector(interpSelector);
  //   let widgets = await page.$$(interpSelector);
  //   expect(widgets.length).toBeGreaterThan(0);
  // });
});
