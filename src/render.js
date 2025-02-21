const puppeteer = require('puppeteer');
const fs = require('fs').promises;

(async () => {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();
  await page.goto('http://localhost:8000/index.html');
  await page.setViewport({width: 1920,height:1080});
  await page.type('textarea', `##### TypeTextSpeed 30c/s
// counter.f80
main(a, b) {
  ROM.clearScreen()
  let counter = 0
  let name = "Hello!"

##### Wait 40f
##### TypeTextSpeed 30c/s
  renderStaticText()

##### Wait 40f
##### TypeTextSpeed 30c/s
  loop {
    renderCounter(counter)

##### Wait 40f
##### TypeTextSpeed 30c/s
    wait for keypress {
      Key.J -> { counter -= 1 }

##### Wait 40f
##### TypeTextSpeed 30c/s
      Key.K -> { counter += 1 }
    }
  }
}
##### END`);
  await page.keyboard.press('F2');
  let frame = 0;
  const date = +new Date();
  await fs.mkdir(`render${date}`);
  while (true) {
    try {
      console.log(`Rendering frame ${frame}`);
      await page.waitForSelector('#fullscreen-scene', {timeout: 500});
      await page.screenshot({path: `render${date}/frame${frame.toString().padStart(5,'0')}.png`});
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
