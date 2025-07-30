

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
