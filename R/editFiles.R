#' Edit multiple files side by side
#' @importFrom shiny runApp
#' @importFrom rio import
#' @export
editFiles=function(){
     shiny::runApp(system.file('multipleFiles2',package='editData'))
}
