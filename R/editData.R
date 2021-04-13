#' A shiny app for editing a 'data.frame'
#' @param data A tibble or a tbl_df or a data.frame to manipulate
#' @param viewer Specify where the gadget should be displayed. Possible choices are c("dialog","browser","pane")
#' @param length Numeric desired maximum length of string
#' @param cols numeric
#' @param status character. dropdownButton status. One of c("default","info","primary","danger","warning","success")
#' @param showButtons logical
#' @param enableSave logical
#' @param editable logical
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
editData=function(data=NULL,viewer="dialog",length=50,cols=1:7,status="default",showButtons=TRUE,enableSave=TRUE,editable=NULL){

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
    # ,verbatimTextOutput("text1")

))

server=function(input,output,session){

     # if(!isNamespaceLoaded("tidyverse")){
     #      attachNamespace("tidyverse")
     # }

    RV=reactiveValues(df=c(),mode=1)
    uploaded<-uploaded1<-c()

    observeEvent(input$mydata,{
        x=input$mydata
        if(!is.null(x) && nzchar(x) &&
            exists(x) && is.data.frame(get(x))) {
            RV$df<-get(x)

        } else {
            RV$df<-NULL
        }


     })


    df=callModule(editableDT,"table1",data=reactive(RV$df),
                  length=length,cols=cols,status=status,showButtons=showButtons,enableSave=enableSave,editable=editable)


    observeEvent(input$file1,{
        if(!is.null(input$file1)) {

            if(input$mydata!="uploaded"){
                uploaded<<-myimport(input$file1$datapath)
                updateTextInput(session,"mydata",value="uploaded")
                # RV$df<-myimport(input$file1$datapath)

            } else{
                uploaded1<<-myimport(input$file1$datapath)
                updateTextInput(session,"mydata",value="uploaded1")
            }

        }
    })

    # output$text1=renderPrint({
    #     str(RV$df)
    #     str(df())
    #
    # })


    observeEvent(input$done, {


        result=df()

        stopApp(invisible(result))
    })

    observeEvent(input$cancel, {

        stopApp()
    })
}

if(viewer=="dialog") myviewer <- dialogViewer("editData", width = 1000, height = 1000)
else if(viewer=="browser") myviewer <- browserViewer()
else myviewer <- paneViewer()
runGadget(ui, server, viewer = myviewer)
}


