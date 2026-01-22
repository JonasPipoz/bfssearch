#' JavaScript pour copier dans le presse-papiers et gérer le modal
#' 
#' @noRd
get_js_scripts <- function() {
  tags$script("
    Shiny.addCustomMessageHandler('copyToClipboard', function(message) {
      var textarea = document.createElement('textarea');
      textarea.value = message;
      textarea.style.position = 'fixed';
      textarea.style.opacity = '0';
      document.body.appendChild(textarea);
      textarea.select();
      try {
        document.execCommand('copy');
      } catch (err) {
        console.error('Erreur lors de la copie:', err);
      }
      document.body.removeChild(textarea);
    });
    
    // Gérer le modal SSE
    Shiny.addCustomMessageHandler('showSSEModal', function(message) {
      $('#sse_modal').modal('show');
    });
    
    // Empêcher le changement de radio button quand on clique sur le lien info
    $(document).on('click', '#sse_info_btn', function(e) {
      e.preventDefault();
      e.stopPropagation();
      $('#sse_modal').modal('show');
      return false;
    });
  ")
}
