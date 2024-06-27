import puppeteer, { type Browser, type Page } from "puppeteer";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

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
      `file://${__dirname}../../../../../crates/mdbook-aquascope/test-book/book/index.html`,
      {
        waitUntil: "domcontentloaded"
      }
    );

    await page.waitForSelector(".aquascope");
    // The embedded editors don't have to call out to aquascope,
    // they just need to render their contents. After the first editor has been
    // rendered they should all follow shortly.
    await page.waitForTimeout(5000);

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
  }, 30_000);
});
