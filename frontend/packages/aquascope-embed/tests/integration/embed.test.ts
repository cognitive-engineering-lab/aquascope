import puppeteer, { Browser, Page } from "puppeteer";

describe("Aquascope Embed", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    browser = await puppeteer.launch();
    page = await browser.newPage();
  });

  afterAll(async () => {
    await browser.close();
  });

  it("launches crates/mdbook-aquascope/test-book/", async () => {
    await page.goto(
      `file://${__dirname}../../../../../../crates/mdbook-aquascope/test-book/book/index.html`,
      {
        waitUntil: "domcontentloaded",
      }
    );
    let editors = await page.$$(".aquascope");
    let crashedElements = await page.$$(".aquascope-crash");
    let embedElements = await page.$$(".aquascope-embed");

    // No embed elements that didn't get rendered
    expect(embedElements.length).toBe(0);

    // No crashed elements
    expect(crashedElements.length).toBe(0);

    // There must have been an editor on the screen.
    expect(editors).not.toBeNull();
    expect(editors.length).toBeGreaterThan(0);
  });
});
