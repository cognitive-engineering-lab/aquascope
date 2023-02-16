import puppeteer, { Browser, Page } from "puppeteer";

const permStackSelector = ".permission-stack";
const permStepSelector = ".step-widget-container";
const interpSelector = ".interpreter";

jest.setTimeout(50000);

describe("Aquascope Standalone", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    // NOTE: here's a good way to debug a puppeteer test.
    // browser = await puppeteer.launch({
    //   headless: false,
    //   product: "chrome",
    //   args: ["--start-maximized"],
    //   defaultViewport: { width: 1700, height: 800 },
    //   slowMo: 250,
    // });
    browser = await puppeteer.launch();
    // there seems to be a discrepency between headless and headed modes.
    // See: https://github.com/puppeteer/puppeteer/issues/665
    //
    // In a headless mode the browser seems to skip rendering visual
    // elements (just a gut feeling). Setting the language and
    // user agent seem to help keep things consistent.
    page = await browser.newPage();
    await page.setExtraHTTPHeaders({
      "Accept-Language": "en-GB,en-US;q=0.9,en;q=0.8",
    });
    await page.setUserAgent(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/66.0.3359.181 Safari/537.36"
    );
  });

  afterAll(async () => {
    await browser.close();
  });

  beforeEach(async () => {
    await page.goto(`http://localhost:8000`, {
      waitUntil: "networkidle0",
    });
  });

  it("runs permissions", async () => {
    await page.waitForSelector("#showPermissions");
    await page.click("#showPermissions");

    await page.waitForSelector(permStackSelector);
    let stackWidgets = await page.$$(permStackSelector);
    expect(stackWidgets.length).toBeGreaterThan(0);

    await page.waitForSelector(permStepSelector);
    let stepWidgets = await page.$$(permStepSelector);
    expect(stepWidgets.length).toBeGreaterThan(0);

    let crashedElement = await page.$(".aquascope-crash");
    // No crashed elements
    expect(crashedElement).toBeNull();
  });

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
