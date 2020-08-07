const puppeteer = require('puppeteer-core');
const fs = require('fs').promises;

(async () => {
  const browser = await puppeteer.launch({
    product: 'chrome',
    executablePath: '../../../../../usr/bin/google-chrome-stable',
    defaultViewport: {
      width: 1920,
      height: 1080,
    },
  });
  const page = await browser.newPage();
  await page.goto('http://localhost:8000/index.html');
  await page.keyboard.press('F2');
  let frame = 0;
  const date = +new Date();
  await fs.mkdir(`render${date}`);
  while (true) {
    try {
      console.log(`Rendering frame ${frame}`);
      await page.waitForSelector('#fullscreen-scene', {timeout: 500});
      await page.screenshot({path: `render${date}/frame${frame.toString().padStart(3,'0')}.png`});
      await page.keyboard.press('F1');
      frame++;
    } catch (e) {
      console.log('error');
      console.log(e);
      await page.screenshot({path: 'error.png'});
      break;
    }
  }
  await browser.close();
})();
