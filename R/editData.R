#' A shiny app for editing a 'data.frame'
#' @param data A tibble or a tbl_df or a data.frame to manipulate
#' @param viewer Specify where the gadget should be displayed. Possible choices are c("dialog","browser","pane")
#' @param mode An integer
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom utils read.csv str write.csv
#' @importFrom shiny stopApp callModule runGadget column fileInput downloadButton renderPrint
#'  observeEvent tagList uiOutput browserViewer dialogViewer downloadHandler h4 hr paneViewer
#'  checkboxInput need validate reactiveValues
#' @return A manipulated 'data.frame' or NULL
#' @export
#' @examples
#' library(shiny)
#' library(editData)
#'# Only run examples in interactive R sessions
#' if (interactive()) {
#'     result<-editData(mtcars)
#'     result
#' }
editData=function(data=NULL,viewer="dialog",mode=2){

    # data("sampleData",package="editData",envir=environment())
    data("sampleData",package="editData",envir=environment())
    context <- rstudioapi::getActiveDocumentContext()

    # Set the default data to use based on the selection.
    text <- context$selection[[1]]$text
    defaultData <- text

    if(is.null(data)) {
        if(nzchar(defaultData)) {
            mydata=defaultData
        } else {
            mydata="sampleData"
        }
    }

    if(any(class(data) %in% c("data.frame","tibble","tbl_df"))) {
        mydata=deparse(substitute(data))
    } else if(class(data) =="character") {
        mydata=data
    }


ui<-miniPage(
    gadgetTitleBar("editable DataTable"),
    miniContentPanel(
    fluidRow(

        column(6,
    fileInput("file1","Upload File")),

    column(6,
    textInput3("mydata","Or Enter data name",value=mydata,width=150,bg="lightcyan"))),
    editableDTUI("table1")

))

server=function(input,output,session){

     if(!isNamespaceLoaded("tidyverse")){
          attachNamespace("tidyverse")
     }

    RV=reactiveValues()

    observeEvent(input$mydata,
                 RV$df<-myget(input$mydata))

    df=callModule(editableDT,"table1",data=reactive(RV$df))

    observeEvent(input$file1,{
        if(!is.null(input$file1)) {
             dataname=ifelse(input$mydata=="uploaded","uploaded1","uploaded")
             updateTextInput(session,"mydata",value=dataname)
             assign(dataname,myimport(input$file1$datapath))
            # if(input$mydata!="uploaded"){
            #     uploaded<<-myimport(input$file1$datapath)
            #     updateTextInput(session,"mydata",value="uploaded")
            #     # RV$df<-myimport(input$file1$datapath)
            #
            # } else{
            #     uploaded1<<-myimport(input$file1$datapath)
            #     updateTextInput(session,"mydata",value="uploaded1")
            # }

        }
    })

    # output$text1=renderPrint({
    #     str(RV$df)
    #
    # })


    observeEvent(input$done, {


        result=df()

        stopApp(result)
    })

    observeEvent(input$cancel, {

        stopApp()
    })
}

if(viewer=="dialog") myviewer <- dialogViewer("editData", width = 1000, height = 800)
else if(viewer=="browser") myviewer <- browserViewer()
else myviewer <- paneViewer()
runGadget(ui, server, viewer = myviewer)
}


