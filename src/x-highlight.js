xtag.create('x-highlight', class extends XTagElement {
  constructor () {
    super();
    this.code = '';
  }

  '::template(true)' () {
    const html = Prism.highlight(this.code, Prism.languages['json'], 'json');
    return `<pre class="language-json" style="overflow: hidden;"><code class="language-json">${html}</code></pre>`;
  }

  set 'data-code::attr' (code) {
    this.code = code;
    this.render();
  }

});
