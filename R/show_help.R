#' @export
show_help <- function(){
  rintrojs::introjs(
    session,
    events = list(
      "onchange" = I("if (this._currentStep==0) {
                       $('a[data-value=\"Portfolio\"]').removeClass('active');
                       $('a[data-value=\"Table\"]').removeClass('active');
                       $('a[data-value=\"Trade Results\"]').addClass('active');
                       $('a[data-value=\"Trade Results\"]').trigger('click');
                       }
                       if (this._currentStep==1) {
                       $('a[data-value=\"Trade Results\"]').removeClass('active');
                       $('a[data-value=\"Table\"]').removeClass('active');
                       $('a[data-value=\"Portfolio\"]').addClass('active');
                       $('a[data-value=\"Portfolio\"]').trigger('click');
                       }
                       if (this._currentStep==2) {
                       $('a[data-value=\"Trade Results\"]').removeClass('active');
                       $('a[data-value=\"Portfolio\"]').removeClass('active');
                       $('a[data-value=\"Table\"]').addClass('active');
                       $('a[data-value=\"Table\"]').trigger('click');
                       }")
    )
  )
}
