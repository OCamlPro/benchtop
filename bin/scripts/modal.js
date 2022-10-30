window.onload = function () {
  let modal_element = document.getElementById('error-modal');
  if (modal_element) {
    let error_modal = new bootstrap.Modal(modal_element, {});
    error_modal.show();
  }
}
