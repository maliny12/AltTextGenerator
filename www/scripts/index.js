

$(document).on('click', function(event) {
  const panel = $('#settings_panel');
  const button = $('#show_setting');

  // Hide panel when clicked outside of panel
  if (
    !panel.is(event.target) && panel.has(event.target).length === 0 &&
    !button.is(event.target) && button.has(event.target).length === 0
  ) {
    panel.hide();
  }
});



$(() => {
  $(document).on("shiny:connected", () => {
    import('./webr.js').then(
      async ({ WebR }) => {
        globalThis.webR = new WebR({channelType: 0});
        await globalThis.webR.init();
        let result = await (await globalThis.webR.evalR("rnorm(100)")).toJs()
        console.log(result);
      })
  });
