import puppeteer, { Browser, Page } from "puppeteer";

const permStackSelector = "svg.permission";
const permStepSelector = ".step-widget-container";
const interpSelector = ".interpreter";

describe("Aquascope Standalone", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    browser = await puppeteer.launch();
    page = await browser.newPage();
  });

  afterAll(async () => {
    await page.close();
    await browser.close();
  });

  beforeEach(async () => {
    await page.goto(`http://localhost:8000`, {
      waitUntil: "domcontentloaded",
    });
  });

  it("runs permission boundaries", async () => {
    await page.click("#showBoundaries");
    let crashedElement = await page.$(".aquascope-crash");
    // No crashed elements
    expect(crashedElement).toBeNull();
    await page.waitForSelector(permStackSelector);
    let widgets = await page.$$(permStackSelector);
    expect(widgets.length).toBeGreaterThan(0);
  });

  it("runs permissions steps", async () => {
    await page.click("#showPermSteps");
    let crashedElement = await page.$(".aquascope-crash");
    // No crashed elements
    expect(crashedElement).toBeNull();
    await page.waitForSelector(permStepSelector);
    let widgets = await page.$$(permStepSelector);
    expect(widgets.length).toBeGreaterThan(0);
  });

  it("runs the interpreter", async () => {
    await page.click("#showInterpret");
    let crashedElement = await page.$(".aquascope-crash");
    // No crashed elements
    expect(crashedElement).toBeNull();
    await page.waitForSelector(interpSelector);
    let widgets = await page.$$(interpSelector);
    expect(widgets.length).toBeGreaterThan(0);
  });
});
