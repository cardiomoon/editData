library(shiny)
library(editData)
library(rio)
library(openxlsx)
library(miniUI)
library(rstudioapi)
library(DT)
library(tibble)
library(dplyr)
library(shinyWidgets)

options(shiny.sanitize.errors = FALSE)

ui<-miniPage(
     gadgetTitleBar("editable DataTable"),
     miniContentPanel(
          fluidRow(

               column(6,
                      fileInput("file1","Upload File")),

               column(6,
                      textInput3("mydata","Or Enter data name",value="mtcars",width=150,bg="lightcyan"))),
          editableDT2UI("table1")

     ))

server=function(input,output,session){

     # if(!isNamespaceLoaded("tidyverse")){
     #      attachNamespace("tidyverse")
     # }

     RV=reactiveValues()

     observeEvent(input$mydata,
                  RV$df<-myget(input$mydata))

     df=callModule(editableDT2,"table1",data=reactive(RV$df))

     observeEvent(input$file1,{
          if(!is.null(input$file1)) {
               # dataname=ifelse(input$mydata=="uploaded","uploaded1","uploaded")
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

# myviewer <- dialogViewer("editData", width = 1000, height = 800)
shinyApp(ui, server)

