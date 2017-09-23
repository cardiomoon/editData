#' A shiny app for editing a 'data.frame'

#' @param data A tibble or a tbl_df or a data.frame to manipulate
#' @param viewer Specify where the gadget should be displayed. Possible choices are c("dialog","browser","pane")
#'
#' @return A manipulated 'data.frame' or NULL
#' @importFrom shiny selectInput runGadget hr dateInput h4 modalButton modalDialog showModal updateDateInput
#' @importFrom shiny textInput checkboxInput numericInput conditionalPanel verbatimTextOutput uiOutput h3 actionButton
#' @importFrom shiny validate need updateTextInput updateCheckboxInput reactive
#' @importFrom shiny updateSelectInput renderUI tagList updateNumericInput updateSelectInput fluidRow column br radioButtons
#' @importFrom shiny observeEvent stopApp dialogViewer paneViewer browserViewer div tags icon
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom tibble add_row as_tibble
#' @importFrom DT datatable
#' @export
#' @examples
#' library(shiny)
#' library(editData)
#'# Only run examples in interactive R sessions
#' if (interactive()) {
#'     result<-editData(mtcars)
#'     result
#' }
editData=function(data=NULL,viewer="dialog"){

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


ui <- miniPage(
     gadgetTitleBar("editable DataTable"),
     miniContentPanel(

     fluidRow(
          column(6,
                 textInput("mydata","Enter data name",value=mydata),
     actionButton("delRow","Delete",icon=icon("remove",lib="glyphicon")),
     actionButton("addRow","Add New",icon=icon("plus",lib="glyphicon")),
     actionButton("editData","Edit Data",icon=icon("wrench",lib="glyphicon"))),
     column(6,
            br(),
     radioButtons("resultAs","Resultant Data as",choices=c("tibble","data.frame"),inline=TRUE))),
     hr(),
     DT::dataTableOutput("origTable"),
     conditionalPanel(condition="true==false",
     numericInput("no","no",value=1))
     )
)

server <- function(input, output, session) {

     deleted<-deleted1<-edited<-edited1<-added<-added1<-updated1<-updated<-c()

     df=reactive({
          input$reload

          eval(parse(text=input$mydata))
     })

     output$origTable <- DT::renderDataTable({
         validate(
             need(any(class(try(eval(parse(text=input$mydata)))) %in% c("tbl_df","tibble","data.frame")),
                  "Please enter the valid data name")
         )
          datatable(
               df(),
               selection = "single",
               caption = input$mydata
          )
     })

     observeEvent(input$delRow,{
          ids <- input$origTable_rows_selected
          if(length(ids)>0){
               x<-df()
               x <- x[-ids,]

               if(input$mydata=="deleted"){
                    deleted1<<-x
                    updateTextInput(session,"mydata",value="deleted1")
               } else{
                    deleted<<-x
                    updateTextInput(session,"mydata",value="deleted")
               }


          } else {
              showModal(modalDialog(
                  title = "Delete Row",
                  "Please Select Row To Delete. Press 'Esc' or Press 'OK' button",
                  easyClose = TRUE,
                  footer=modalButton("OK")
              ))
          }
     })


     observeEvent(input$remove,{


             x<-df()
             x <- x[-input$no,]

             if(input$mydata=="deleted"){
                 deleted1<<-x
                 updateTextInput(session,"mydata",value="deleted1")
             } else{
                 deleted<<-x
                 updateTextInput(session,"mydata",value="deleted")
             }
             if(input$no>nrow(x)) updateNumericInput(session,"no",value=nrow(x))

     })

     observeEvent(input$addRow,{

               x<-df()
               x1 <- tibble::add_row(x)
               x1 <- as.data.frame(x1)
               rownames(x1)=c(rownames(x),nrow(x1))

               if(input$mydata=="added"){
                    added1<<-x1
                    updateTextInput(session,"mydata",value="added1")
               } else{
                    added<<-x1
                    updateTextInput(session,"mydata",value="added")
               }
               updateNumericInput(session,"no",value=nrow(x1))
               editData2()

     })

     observeEvent(input$new,{

         x<-df()
         x1 <- tibble::add_row(x)
         x1 <-as.data.frame(x1)
         rownames(x1)=c(rownames(x),nrow(x1))

         if(input$mydata=="added"){
             added1<<-x1
             updateTextInput(session,"mydata",value="added1")
         } else{
             added<<-x1
             updateTextInput(session,"mydata",value="added")
         }
         updateNumericInput(session,"no",value=nrow(x1))

     })

     observeEvent(input$update,{
          ids <- input$no
          x<-df()

          myname=colnames(x)
          status=ifelse(tibble::has_rownames(x),1,0)
          x<-as.data.frame(x)
          rownames(x)[ids]=input$rowname

          for(i in 1:ncol(x)){
              try(x[ids,i]<-input[[myname[i]]])
              if("POSIXct" %in% class(x[ids,i])){
                   tz=""
                   if(!is.null(attr(x[ids,i],"tzone"))) tz=attr(x[ids,i],"tzone")
                   x[ids,i]=as.POSIXct(input[[myname[i]]],tz=tz,origin="1970-01-01")
              }
          }
          if(input$mydata=="updated"){
               updated1<<-x
               updateTextInput(session,"mydata",value="updated1")
          } else{
               updated<<-x
               updateTextInput(session,"mydata",value="updated")
          }
          updateCheckboxInput(session,"showEdit",value=FALSE)
          #removeModal()
     })

     observeEvent(input$no,{
         mydf=df()[input$no,]
         mydf=as.data.frame(mydf)
         myclass=lapply(mydf,class)

         updateTextInput(session,"rowname",value=rownames(mydf)[1])
         for(i in 1:ncol(mydf)){
             myname=colnames(mydf)[i]

             if("factor" %in% myclass[[i]]){
                 updateSelectInput(session,myname,
                                            choices=levels(mydf[[i]]),selected=mydf[1,i])
             } else if("Date" %in% myclass[[i]]){
                 updateDateInput(session,myname,value=mydf[1,i])
             } else if("logical" %in% myclass[[i]]){
                 if(is.na(mydf[1,i])) myvalue=FALSE
                 else myvalue=mydf[1,i]
                 updateCheckboxInput(session,myname,value=myvalue)
             } else { # c("numeric","integer","charater")
                 updateTextInput(session,myname,value=mydf[1,i])
             }
         }

     })

     observeEvent(input$home,{
         updateNumericInput(session,"no",value=1)
     })

     observeEvent(input$end,{
         updateNumericInput(session,"no",value=nrow(df()))
     })

     observeEvent(input$left,{
         value=ifelse(input$no>1,input$no-1,1)
         updateNumericInput(session,"no",value=value)
     })

     observeEvent(input$right,{

         value=ifelse(input$no<nrow(df()),input$no+1,nrow(df()))
         updateNumericInput(session,"no",value=value)
     })

     observeEvent(input$rowno,{
         maxno=nrow(df())
         if(input$rowno>maxno) {
             updateNumericInput(session,"rowno",value=maxno)
             updateNumericInput(session,"no",value=maxno)
         } else{
             updateNumericInput(session,"no",value=input$rowno)
         }

     })

     output$test2=renderUI({

          ids <- input$no
          if(length(ids)==1){

              mydf=df()
              mylist=list()
              myclass=lapply(mydf,class)
              mylist[[1]]=actionButton("home","",icon=icon("backward",lib="glyphicon"))
              mylist[[2]]=actionButton("left","",icon=icon("chevron-left",lib="glyphicon"))
              mylist[[3]]=numericInput3("rowno","rowno",value=input$no,min=1,
                                        max=nrow(mydf),step=1,width=50+10*log10(nrow(mydf)))
              mylist[[4]]=actionButton("right","",icon=icon("chevron-right",lib="glyphicon"))
              mylist[[5]]=actionButton("end","",icon=icon("forward",lib="glyphicon"))
              mylist[[6]]=actionButton("new","",icon=icon("plus",lib="glyphicon"))
              mylist[[7]]=textInput3("rowname","rowname",value=rownames(mydf)[input$no],width=200)
              mylist[[8]]=hr()
              addno=8
              mydf=as.data.frame(mydf[input$no,])
              for(i in 1:ncol(mydf)){
                   myname=colnames(mydf)[i]
                   if("factor" %in% myclass[[i]]){
                        mylist[[i+addno]]=selectInput3(myname,myname,
                                                        choices=levels(mydf[[i]]),selected=mydf[1,i])
                   } else if("Date" %in% myclass[[i]]){
                        mylist[[i+addno]]=dateInput(myname,myname,value=mydf[1,i])
                   } else if("logical" %in% myclass[[i]]){
                        if(is.na(mydf[1,i])) myvalue=FALSE
                        else myvalue=mydf[1,i]
                        mylist[[i+addno]]=checkboxInput3(myname,myname,value=myvalue)
                   } else { # c("numeric","integer","charater")
                        mylist[[i+addno]]=textInput3(myname,myname,value=mydf[1,i])
                   }
              }
              do.call(tagList,mylist)
          } else{

                    h4("You can edit data after select one row in datatable.")

          }


     })

     observeEvent(input$editData,{
          ids <- input$origTable_rows_selected
          if(length(ids)==1) updateNumericInput(session,"no",value=ids)
          else if(input$no>nrow(df())) updateNumericInput(session,"no",value=1)
          editData2()
     })

     editData2=function(){
         showModal(modalDialog(
             title="Edit Data",
             footer=tagList(
                 actionButton("remove","Delete",icon=icon("remove",lib="glyphicon")),
                 actionButton("update","Update",icon=icon("ok",lib="glyphicon")),
                 modalButton("Cancel",icon=icon("eject",lib="glyphicon"))),
             easyClose=TRUE,
             uiOutput("test2")
         ))
     }

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
         if(input$resultAs=="tibble"){
              result<-as_tibble(result)
         } else{
              result<-as.data.frame(result)
         }
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



