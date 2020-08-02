xtag.create('x-highlight', class extends XTagElement {
  constructor () {
    super();
    this.code = '';
  }

  '::template(true)' () {
    const html = Prism.highlight(this.code, Prism.languages['elm'], 'elm');
    return html;
  }

  set 'data-code::attr' (code) {
    this.code = code;
    this.render();
  }

});
