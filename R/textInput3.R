#' Sample Data for testing 'editData' addin
#'
#' A sample dataset containing data for 4 people
#'
#' @format A data.frame with 4 rows and 6 variables:
#' \describe{
#' \item{name}{Last name}
#' \item{age}{age in years}
#' \item{country}{Country Name}
#' \item{sex}{sex, A factor with two levels.}
#' \item{bloodType}{Blood Type. A factor with four levels}
#' \item{date}{Date}
#' }
"sampleData"


#' Create a side-by-side textInput control for entry of unstructured text values
#'
#'@param inputId The input slot that will be used to access the value.
#'@param label Display label for the control, or NULL for no label.
#'@param value Initial value.
#'@param width The width of the input in pixel
#'@param bg The color of text
#'@param ... arguments to be passed to textInput
#'@export
#'@examples
#'library(shiny)
#'# Only run examples in interactive R sessions
#'if (interactive()) {
#'   ui <- fluidPage(
#'          textInput3("id", "id", ""),
#'          textInput3("name","name","")
#'     )
#'     server <- function(input, output) {
#'
#'     }
#'     shinyApp(ui, server)
#'}
textInput3<-function (inputId, label, value = "",width=100,bg=NULL,...)
{
        style=paste0("width: ",width,"px;")
        if(!is.null(bg)) style=paste0(style,"background-color:",bg,";")
  div(style="display:inline-block;",
      if(label!="") tags$label(label, `for` = inputId),
      tags$input(id = inputId, type = "text", class="form-control",value = value,
                 style=style,...))
}

#'Create a side-by-side selectInput
#'@param ... arguments to be passed to selectInput
#'@param width The width of the input in pixel
#'@export
#'@examples
#'library(shiny)
#'# Only run examples in interactive R sessions
#'if (interactive()) {
#'   ui <- fluidPage(
#'          selectInput3("sex", "sex", choices=c("Male","Female")),
#'          selectInput3("smoking", "smokingStatus", choices=c("Never","Ex-smoker","Smoker"))
#'     )
#'     server <- function(input, output) {
#'
#'     }
#'     shinyApp(ui, server)
#'}
selectInput3<-function(...,width=100){
  mywidth=paste(width,"px",sep="")
  div(style="display:inline-block;",selectInput(...,width=mywidth))
}


#'Create a side-by-side label
#'@param label A text to display
#'@param width The width of the input in pixel
#'@param bg The color of text
#'@param ... arguments to be passed to label
#'@export
#'@examples
#'library(shiny)
#'# Only run examples in interactive R sessions
#'if (interactive()) {
#'   ui <- fluidPage(
#'          label3("Welcome"),
#'          checkboxInput3("somevalue", "Some value", FALSE),
#'          verbatimTextOutput("value")
#'   )
#'   server <- function(input, output) {
#'         output$value <- renderText({ input$somevalue })
#'   }
#'   shinyApp(ui, server)
#'}
label3<-function(label,width=100,bg=NULL,...){
    style=paste0("width: ",width,"px;")
    if(!is.null(bg)) style=paste0(style,"background-color:",bg,";")
    div(style="display:inline-block;",
        tags$label(label, style=style,...))
}

#'Create a side-by-side numericInput
#'@param inputId The input slot that will be used to access the value.
#'@param label Display label for the control, or NULL for no label.
#'@param value Initial value.
#'@param min Minimum allowed value
#'@param max Maximum allowed value
#'@param step Interval to use when stepping between min and max
#'@param width The width of the input in pixel
#'@param ... arguments to be passed to numericInput
#'@export
#'@examples
#'library(shiny)
#'# Only run examples in interactive R sessions
#'if (interactive()) {
#'   ui <- fluidPage(
#'          textInput3("id", "id", ""),
#'          numericInput3("score","score",value=1)
#'     )
#'     server <- function(input, output) {
#'
#'     }
#'     shinyApp(ui, server)
#'}
numericInput3<-function (inputId, label, value, min=NA,max=NA,step=NA,width=100,...)
{
    div(style="display:inline-block;",
        tags$label(label, `for` = inputId,class="control-label"),
        tags$input(id = inputId, type = "number", class="form-control",
                   value = value, min=min,max=max,step=step,style=paste("width: ",width,"px;",sep=""),...)
    )
}

#'Create a side-by-side checkboxInput
#'@param inputId The input slot that will be used to access the value.
#'@param label Display label for the control, or NULL for no label.
#'@param value Initial value.
#'@param width The width of the input in pixel
#'@export
#'@examples
#'library(shiny)
#'# Only run examples in interactive R sessions
#'if (interactive()) {
#'   ui <- fluidPage(
#'          label3("Welcome"),
#'          checkboxInput3("somevalue", "Some value", FALSE),
#'          verbatimTextOutput("value")
#'   )
#'   server <- function(input, output) {
#'         output$value <- renderText({ input$somevalue })
#'   }
#'   shinyApp(ui, server)
#'}
checkboxInput3<-function(inputId,label,value=FALSE,width=100){
  if(value)
    div(style="display:inline-block;",

        tags$input(id = inputId, type = "checkbox",checked = "checked"),
        tags$label(label, `for` = inputId,
                   style=paste("width: ",width-15,"px;",sep=""))
    )
  else
    div(style="display:inline-block;",
        tags$input(id = inputId, type = "checkbox"),
        tags$label(label, `for` = inputId, style=paste("width: ",width-15,"px;",sep=""))
    )
}
