#' A shiny app for editing a 'data.frame'
#' @param data A tibble or a tbl_df or a data.frame to manipulate
#' @param viewer Specify where the gadget should be displayed. Possible choices are c("dialog","browser","pane")
#' @param mode An integer
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom utils read.csv str write.csv
#' @importFrom shiny stopApp callModule runGadget column fileInput downloadButton renderPrint
#'  observeEvent tagList uiOutput browserViewer dialogViewer downloadHandler h4 hr paneViewer
#'  checkboxInput need validate
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

    sampleData<-editData::sampleData
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

        result<-tryCatch(eval(parse(text=data)),error=function(e) "error")
        if(any(class(result) %in% c("data.frame","tibble","tbl_df"))) mydata=data
        else  return(NULL)
    }


ui<-miniPage(
    gadgetTitleBar("editable DataTable"),
    miniContentPanel(
    fluidRow(

        column(6,
    fileInput("file1","Upload CSV file"),
    checkboxInput("strAsFactor","strings As Factor",value=FALSE)),
    column(6,
    textInput3("mydata","Or Enter data name",value=mydata,width=150,bg="lightcyan"))),
    editableDTUI("table1")
))

server=function(input,output,session){

     if(!isNamespaceLoaded("tidyverse")){
          attachNamespace("tidyverse")
     }

    uploaded <-c()

    mydf=reactive({
        validate(
            need(any(class(try(eval(parse(text=input$mydata)))) %in% c("tbl_df","tibble","data.frame")),"Enter Valid Data Name"))
        mydf=eval(parse(text=input$mydata))
        mydf
    })
    df=callModule(editableDT,"table1",data=reactive(mydf()))

    # output$test=renderPrint({
    #     str(df())
    # })

    observeEvent(input$file1,{
        if(!is.null(input$file1)) {
            uploaded<<-read.csv(input$file1$datapath,stringsAsFactors = input$strAsFactor)
            updateTextInput(session,"mydata",value="uploaded")
        }
    })


    # mydf<-editData::sampleData
    #
    # df2=callModule(editableDT,"table2",data=reactive(mydf))
    #
    # output$test2=renderPrint({
    #     str(df2())
    # })
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("edited-",Sys.Date(),".csv", sep = "")
        },
        content = function(file) {
            write.csv(df(), file, row.names = FALSE)
        }
    )

    observeEvent(input$done, {

        # if(nzchar(defaultData)) {
        #     insertText(text=input$code)
        #     stopApp()
        # } else{
        #     result <- eval(parse(text=input$code))
        #     attr(result,"code") <- input$code
        #     stopApp(result)
        # }
        result=df()
        # if(input$resultAs=="tibble"){
        #     result<-as_tibble(result)
        # } else{
        #     result<-as.data.frame(result)
        # }
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


