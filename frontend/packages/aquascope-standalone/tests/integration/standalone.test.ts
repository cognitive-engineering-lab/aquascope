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
    await browser.close();
  });

  beforeEach(async () => {
    await page.goto(`http://localhost:8000`, {
      waitUntil: "domcontentloaded",
    });
  });

  it("runs permissions", async () => {
    await page.waitForSelector("#showPermissions");
    await page.click("#showPermissions");
    await page.waitForNetworkIdle();

    let crashedElement = await page.$(".aquascope-crash");
    // No crashed elements
    expect(crashedElement).toBeNull();

    await page.waitForSelector(permStackSelector);
    let stackWidgets = await page.$$(permStackSelector);
    expect(stackWidgets.length).toBeGreaterThan(0);

    await page.waitForSelector(permStepSelector);
    let stepWidgets = await page.$$(permStepSelector);
    expect(stepWidgets.length).toBeGreaterThan(0);
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
