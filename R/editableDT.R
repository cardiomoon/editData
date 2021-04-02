#' Side by side pickerInput
#' @param ... Further arguments to be passed to pickerInput
#' @importFrom shinyWidgets pickerInput
#' @export
pickerInput3=function (...)
{
     div(style = "display:inline-block;", pickerInput(...))
}

#' UI of editableDT Shiny module
#' @param id A string
#' @importFrom shiny NS
#' @export
#' @examples
#'# Only run examples in interactive R sessions
#'if (interactive()) {
#' library(shiny)
#' ui=fluidPage(
#'   selectInput("select","select",choices=c("mtcars","iris","sampleData")),
#'   textInput("mydata","mydata",value="mtcars"),
#'   hr(),
#'   editableDTUI("editableDT"),
#'   hr(),
#'   verbatimTextOutput("test")
#' )
#' server=function(input,output,session){
#'   data=reactive({
#'             myget(input$mydata)
#'   })
#'   observeEvent(input$select,{
#'        updateTextInput(session,"mydata",value=input$select)
#'   })
#'   result=callModule(editableDT,"editableDT",data=data)
#'   output$test=renderPrint({
#'   str(result())
#'   })
#' }
#' shinyApp(ui=ui,server=server)
#' }
editableDTUI=function(id){
     ns <-NS(id)

     uiOutput(ns("editableDTModule"))
}


#' Server function of editableDT Shiny module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param data A reactive data object
#' @importFrom DT DTOutput renderDT dataTableProxy datatable replaceData
#' @importFrom openxlsx write.xlsx
#' @importFrom shiny br span reactiveVal actionButton fluidRow icon modalButton modalDialog
#' reactive removeModal renderUI showModal textAreaInput updateTextInput
#' @export
editableDT=function(input,output,session,data){

     ns <- session$ns

     df1<-reactiveVal()

     observeEvent(data(),{
          df1(data())
     })

     output$editableDTModule=renderUI({
          tagList(
               actionButton(ns("delete"),"Delete",icon=icon("trash-alt")),
               actionButton(ns("reset"),"Restore",icon=icon("trash-restore")),
               actionButton(ns("edit"),"Edit",icon=icon("pen-fancy")),
               actionButton(ns("add"),"Add",icon=icon("plus-square")),
               actionButton(ns("insert"),"Insert",icon=icon("search-plus")),
               actionButton(ns("deleteAll"),"Delete All",icon=icon("minus-square")),
               br(),
               checkboxInput(ns("confirm"),"Confirm delete or restore",value=TRUE),
               DTOutput(ns("table")),
               downloadButton(ns("downloadData"), "download as CSV",icon=icon("file-csv")),
               downloadButton(ns("downloadExcel"), "download as Excel",icon=icon("file-excel")),
               downloadButton(ns("downloadRDS"), "download as RDS")
               # ,verbatimTextOutput(ns("test1"))
          )
     })

     output$test1=renderPrint({
          cat("This is in module\n")
          str(data())
          str(df1())
     })

     observeEvent(input$resetOk,{
          restoreData()
          removeModal()
     })


     output$table <- renderDT({
          datatable(data(), editable = "cell",
                    options=list(
                         pageLength=10
                    ))
     })

     proxy = dataTableProxy('table')

     observeEvent(input$table_cell_edit, {
          info=input$table_cell_edit

          i=info$row
          j=info$col
          v=info$value

          newdf <- df1()
          newdf[i,j]<-DT::coerceValue(v, newdf[i, j])

          df1(newdf)
     })
     observeEvent(input$delete,{
          if(!input$confirm){
               deleteSelected()
          } else{
               i= input$table_rows_selected

               if(length(i)>0){
                    showModal(modalDialog( span('Do you really want to delete data selected ?'),
                                           footer = tagList(
                                                modalButton("Cancel"),
                                                actionButton(ns("deleteOk"), "Delete",icon=icon("trash-alt"))
                                           ),
                                           easyClose=TRUE
                    ))
               }
          }
     })

     observeEvent(input$deleteAll,{
                    showModal(modalDialog( span('Do you really want to delete ALL data ?'),
                                           footer = tagList(
                                                modalButton("Cancel"),
                                                actionButton(ns("deleteAllOk"), "Delete",icon=icon("trash-alt"))
                                           ),
                                           easyClose=TRUE
                    ))


     })
     observeEvent(input$reset,{
          if(!input$confirm){
               restoreData()
          } else{
               showModal(modalDialog( span('Do you really restore original data ?\n You will lost all the changes'),
                                      footer = tagList(
                                           modalButton("Cancel"),
                                           actionButton(ns("resetOk"), "Restore",icon=icon("trash-restore"))
                                      ),
                                      easyClose=TRUE
               ))
          }
     })

     deleteSelected=function(){
          i= input$table_rows_selected

          if(length(i)>0){
               newdf <- df1()
               newdf <- newdf[-i,]
               df1(newdf)
               replaceData(proxy,df1(),resetPaging=FALSE)
          }
     }
     restoreData=function(){
          df1(data())
          replaceData(proxy,df1(),resetPaging=FALSE)
     }

     observeEvent(input$deleteOk,{
          deleteSelected()
          removeModal()
     })
     observeEvent(input$deleteAllOk,{
          newdf=df1()
          newdf=newdf[0,]
          df1(newdf)
          replaceData(proxy,df1(),resetPaging=FALSE)
          removeModal()
     })

     observeEvent(input$edit,{
          i= input$table_rows_selected
          if(length(i)>0){
               i= input$table_rows_selected[1]
               result=data2input(df1(),i)
               showModal(modalDialog(
                    do.call(tagList,result),
                    title=paste0(i,"/",nrow(df1())),
                    footer = tagList(
                         modalButton("Cancel"),
                         actionButton(ns("update"), "update",icon=icon("save"))
                    ),
                    easyClose = TRUE
               ))
          }
     })

     observeEvent(input$add,{

               i= nrow(df1())+1
               result=data2input(df1(),i)
               showModal(modalDialog(
                    do.call(tagList,result),
                    title=paste0(i,"/",nrow(df1())),
                    footer = tagList(
                         modalButton("Cancel"),
                         actionButton(ns("addOk"), "Add",icon=icon("save"))
                    ),
                    easyClose = TRUE
               ))

     })

     observeEvent(input$insert,{

          i= input$table_rows_selected
          if(length(i)==1){
          result=data2input(df1(),i)
          showModal(modalDialog(
               do.call(tagList,result),
               title=paste0(i,"/",nrow(df1())),
               footer = tagList(
                    modalButton("Cancel"),
                    actionButton(ns("insertUp"), "InsertUp",icon=icon("thumbs-up")),
                    actionButton(ns("insertDown"), "InsertDown",icon=icon("thumbs-down"))
               ),
               easyClose = TRUE
          ))
          }

     })

     observeEvent(input$update,{
          i= input$table_rows_selected[1]
          for(j in 1:ncol(df1())){

               newdf <- df1()
               newdf[i,j]<-DT::coerceValue(input[[paste0("colid",j)]], newdf[i, j])

               df1(newdf)
               replaceData(proxy,df1(),resetPaging=FALSE)
          }
          removeModal()
     })

     observeEvent(input$addOk,{
          newdf=df1()
          newdf<-rbind(newdf,newdf[nrow(newdf),])
          i=nrow(newdf)
          for(j in 1:ncol(df1())){

               newdf <- df1()
               newdf[i,j]<-DT::coerceValue(input[[paste0("colid",j)]], newdf[i, j])

               df1(newdf)
               replaceData(proxy,df1(),resetPaging=FALSE)
          }
          removeModal()
     })

     observeEvent(input$insertUp,{
          i= input$table_rows_selected
          newdf=df1()
          newdf<-rbind(newdf[1:i,],newdf[i:nrow(newdf),])

          for(j in 1:ncol(df1())){

               newdf[i,j]<-DT::coerceValue(input[[paste0("colid",j)]], newdf[i, j])

               df1(newdf)
               replaceData(proxy,df1(),resetPaging=FALSE)
          }
          removeModal()
     })
     observeEvent(input$insertDown,{
          i= input$table_rows_selected
          newdf=df1()
          newdf<-rbind(newdf[1:i,],newdf[i:nrow(newdf),])
          i=i+1
          for(j in 1:ncol(df1())){

               newdf[i,j]<-DT::coerceValue(input[[paste0("colid",j)]], newdf[i, j])

               df1(newdf)
               replaceData(proxy,df1(),resetPaging=FALSE)
          }
          removeModal()
     })

     data2input=function(data,row){

          lapply(1:ncol(data),vector2input,data,row)

     }


     vector2input=function(x,data,row){
          kind=class(data[[x]])
          id=paste0("colid",x)
          label=names(data)[x]
          if(row>nrow(data)){
               value=data[[x]][length(data[[x]])]
          } else{
               value=data[[x]][row]
          }
          if(is.character(data[[x]])){
               width=max(c(max(nchar(data[[x]])),nchar(label),150))
          } else if(is.factor(data[[x]])){
               width=max(c(max(nchar(levels(data[[x]]))),nchar(label),150))
          } else{
               width=max(nchar(label),150)
          }

          if(kind %in% c("numeric","integer","double")) {
               numericInput3(ns(id),label,value=value,width=width)
          } else if(kind =="factor"){
               pickerInput3(ns(id),label,choices=levels(data[[x]]),
                            selected=value,width=paste0(width,"px"))
          } else if(kind =="Date"){
               dateInput3(ns(id),label,value=value,width=width)
          } else if(kind =="logical"){
               checkboxInput3(ns(id),label,value=value,width=width)
          } else {
               if(max(nchar(data[[x]]))<20){
                textInput3(ns(id),label,value=value,width=width)
               } else{
               textAreaInput(ns(id),label,value=value)
               }
          }


     }

     output$downloadData <- downloadHandler(
          filename = function() {
               "mydata.csv"
          },
          content = function(file) {
               write.csv(df1(), file, row.names = FALSE)
          }
     )

     output$downloadExcel <- downloadHandler(
          filename = function() {
               "mydata.xlsx"
          },
          content = function(file) {
               write.xlsx(df1(), file,asTable=TRUE)
          }
     )
     output$downloadRDS <- downloadHandler(
          filename = function() {
               "mydata.RDS"
          },
          content = function(file) {
               saveRDS(df1(), file)
          }
     )

    return(reactive(df1()))

}
