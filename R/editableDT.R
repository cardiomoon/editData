#' Side by side pickerInput
#' @param ... Further arguments to be passed to pickerInput
#' @importFrom shinyWidgets pickerInput
#' @export
pickerInput3=function (...)
{
    div(style = "display:inline-block;", pickerInput(...))
}


#' Calculate maximal length
#' @param x A vector
#' @export
#' @examples
#' maxLength(month.name)
maxLength=function(x){
    if(is.character(x)){
        max(nchar(x),na.rm=TRUE)
    } else{
        1
    }
}

#' Truncate string to desired length
#' @param x A vector
#' @param length numeric desired string length
#' @export
makeShort=function(x,length=50){
    if(is.character(x)){
        if(length(grep("<a href",x))==0){
        select<-nchar(x,keepNA=FALSE)>length
        select
        x[select]<-paste0(substr(x[select],1,length),"...")
        }
    }
    x
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
#' @param length numeric desired length of string
#' @param cols numeric Initial columns to display
#' @param status character. dropdownButton status. One of c("default","info","primary","danger","warning","success")
#' @param showButtons logical
#' @param enableSave logical
#' @param editable logical
#' @param formatList Null or list. Format list to be passed to formatStyle
#' @param ... Further arguments to be passed to datatable()
#' @importFrom DT DTOutput renderDT dataTableProxy datatable replaceData coerceValue formatStyle
#' @importFrom openxlsx write.xlsx
#' @importFrom shiny br span reactiveVal actionButton fluidRow icon modalButton modalDialog
#' reactive removeModal renderUI showModal textAreaInput updateTextInput isolate conditionalPanel
#' numericInput verbatimTextOutput showNotification textOutput renderText
#' @importFrom shinyWidgets dropdownButton tooltipOptions checkboxGroupButtons awesomeCheckbox
#' updateCheckboxGroupButtons
#' @importFrom lubridate as_datetime
#' @export
editableDT=function(input,output,session,data,length=50,cols=1:7,status="default",showButtons=TRUE,enableSave=TRUE, editable=NULL,formatList=NULL,...){

     ns <- session$ns

     df1<-reactiveVal()

     finalDf<-reactiveVal()

     RV=reactiveValues(cols=cols,editable=FALSE)

     if(!is.null(editable)){
         if(editable==FALSE){
             showButtons=FALSE
             enableSave=FALSE
         }
     }

     observeEvent(data(),{
         finalDf(data())
         RV$cols=intersect(cols,1:ncol(finalDf()))
         df1(shortdata())

     })


     shortdata=reactive({
          input$Refresh
         # data1<-finalDf()
         data1<-data()

         if(is.null(data1)){
             result=NULL
         } else if(ncol(data1)==0){
             result=NULL
         } else{
          RV$cols<-intersect(RV$cols,1:ncol(data1))
          result=as.data.frame(lapply(data1[RV$cols],makeShort,isolate(input$length)))
          rownames(result)=rownames(data1)
          if(is.null(editable)){

                if(identical(RV$cols,1:ncol(data1))&(max(sapply(data1,maxLength),na.rm=TRUE)<length)) {
                      RV$editable=TRUE
                } else{
                    RV$editable=FALSE
                }

          } else{

              RV$editable=editable

          }
         }

          result
     })

     output$dropUI=renderUI({
         if(!is.null(data())){
         no=ncol(data())
         selected=intersect(1:no,RV$cols)
         temp=makeShort(names(data()),length=length)

         dropdownButton(

             checkboxGroupButtons(ns("checkgroup"),"Select Columns to be displayed",
                                  #status = "primary",
                                  selected=selected,
                                  checkIcon = list(
                                      yes = icon("ok",
                                                 lib = "glyphicon"),
                                      no = icon("remove",
                                                lib = "glyphicon")),
                                  choiceNames=temp,
                                  choiceValues=1:no
             ),
             actionButton(ns("selectAll"),"Select ALL columns",icon("ok",lib = "glyphicon")),
             actionButton(ns("unselectAll"),"Unselect ALL columns",icon("remove",lib = "glyphicon")),
             numericInput(ns("length"),"Desired maximum length of cells",value=length),
             actionButton(ns("Refresh"),"Apply Selected Columns and Length",class = "btn-info"),

             circle=TRUE,status=status,icon = icon("gear"),width="600px",
             tooltip=tooltipOptions(title="Customize Table")
         )
         }
     })


     output$editableDTModule=renderUI({

          tagList(
              tags$style(type='text/css', sprintf('#%s {color: green;}',ns("dataCheck"))),
               textOutput(ns("dataCheck")),
               uiOutput(ns("dropUI")),


               conditionalPanel("true==false",
                                checkboxInput(ns("showButtons"),"showButtons",value=showButtons),
                                checkboxInput(ns("enableSave"),"enableSave",value=enableSave)
                                ),
               div(style="display:inline-block;",
               conditionalPanel(sprintf("input[['%s']]==true",ns("showButtons")),
                    br(),
                    actionButton(ns("delete"),"Delete",icon=icon("trash-alt")),
                    actionButton(ns("reset"),"Restore",icon=icon("trash-restore")),
                    actionButton(ns("edit"),"Edit",icon=icon("pen-fancy")),
                    actionButton(ns("add"),"Add",icon=icon("plus-square")),
                    actionButton(ns("insert"),"Insert",icon=icon("search-plus")),
                    actionButton(ns("deleteAll"),"Delete All",icon=icon("minus-square")
               ),
               br(),
               br(),
               awesomeCheckbox(ns("confirm"),"Confirm delete or restore",value=TRUE))),
               awesomeCheckbox(ns("simpleColNames"),"Use simple column names",value=FALSE),
               DTOutput(ns("table")),
               conditionalPanel(sprintf("input[['%s']]==true",ns("enableSave")),
               downloadButton(ns("downloadData"), "download as CSV",icon=icon("file-csv")),
               downloadButton(ns("downloadExcel"), "download as Excel",icon=icon("file-excel")),
               downloadButton(ns("downloadRDS"), "download as RDS")),
               hr()
                #,verbatimTextOutput(ns("test1"))
          )
     })

     output$dataCheck=renderText({
          if(is.null(data())) {
              "Please enter valid data name !"
          }
     })

     observeEvent(input$selectAll,{
         updateCheckboxGroupButtons(session,"checkgroup",selected=1:ncol(data()))
     })
     observeEvent(input$unselectAll,{
         updateCheckboxGroupButtons(session,"checkgroup",selected=NA)
     })


     observeEvent(input$Refresh,{
         RV$cols<-as.integer(input$checkgroup)
         df1(shortdata())
     })

     # output$test1=renderPrint({
     #     cat("finalDf()\n")
     #     str(finalDf())
     #     cat("df1()\n")
     #     str(df1())
     #     cat("shortdata()\n")
     #     str(shortdata())
     #
     # })

     observeEvent(input$resetOk,{
          restoreData()
          removeModal()
     })


     output$table <- renderDT({
          if(!is.null(shortdata())){


          if(input$simpleColNames){
              table=datatable(shortdata(),
                        editable=RV$editable,
                        colnames=paste0("col",1:ncol(shortdata())),
                        options=list(pageLength=10),...)
          } else{
              table=datatable(shortdata(),
                    editable=RV$editable, options=list(pageLength=10),...)
          }
              if(is.null(formatList)){
                  table
              } else{
                  formatList$table=table
                  formatList$columns=1:ncol(shortdata())
                  do.call(formatStyle,formatList)
              }
          }
     })

     proxy = dataTableProxy('table')


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

     observeEvent(input$table_cell_edit, {
          info=input$table_cell_edit

          i=info$row
          j=info$col
          v=info$value

          newdf <- df1()

          if(j==0){
              if(v %in% rownames(newdf)[-i]) {
                  showNotification("Duplicated rownames is not allowed",duration=3,type="message")

              } else{
                  rownames(newdf)[i]=v
              }
          } else{
             newdf[i,j]<-DT::coerceValue(v, newdf[i, j])
          }
          df1(newdf)
          replaceData(proxy,df1(),resetPaging=FALSE)
          newdf2<-finalDf()
          if(j==0){
              if(v %in% rownames(newdf)[-i]){
              } else{
             rownames(newdf2)[i]=v
              }
          } else{
             newdf2[i,j]<-DT::coerceValue(v, newdf2[i, j])

          }
          finalDf(newdf2)


     })

     deleteSelected=function(){
          i= input$table_rows_selected

          if(length(i)>0){
               newdf <- df1()
               newdf <- newdf[-i,]
               df1(newdf)
               replaceData(proxy,df1(),resetPaging=FALSE)
               newdf2<-finalDf()
               newdf2 <- newdf2[-i,]
               finalDf(newdf2)
          }
     }
     restoreData=function(){
          df1(shortdata())
          replaceData(proxy,df1(),resetPaging=FALSE)
          finalDf(data())
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
          newdf2=finalDf()
          newdf2=newdf2[0,]
          finalDf(newdf2)
          removeModal()
     })

     observeEvent(input$edit,{
          i= input$table_rows_selected
          if(length(i)>0){
               i= input$table_rows_selected[1]
               result=data2input(finalDf(),i)
               showModal(modalDialog(
                    do.call(tagList,result),
                    title=paste0(i,"/",nrow(finalDf())),
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
               result=data2input(finalDf(),i)
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
          result=data2input(finalDf(),i)
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
          updateRowname=TRUE
          if(length(grep("[^0-9]",rownames(finalDf())))==0) {
              updateRowname=FALSE
          } else if(input$rowname %in% rownames(finalDf())[-i]){
              updateRowname=FALSE
              showNotification("Duplicated rownames is not allowed",duration=3,type="message")
          }
              newdf2 <- finalDf()
              for(j in 1:ncol(newdf2)){
                  if("POSIXct" %in% class(newdf2[i,j])){
                      newdf2[i,j]<-lubridate::as_datetime(input[[paste0("colid",j)]])
                  } else{
              newdf2[i,j]<-DT::coerceValue(input[[paste0("colid",j)]], newdf2[i, j])
                  }
              }
              if(updateRowname) rownames(newdf2)[i]=input$rowname
              finalDf(newdf2)
              # result=as.data.frame(lapply(finalDf(),makeShort,length))
              # rownames(result)=rownames(finalDf())
              # newdf=result[RV$cols]
              # df1(newdf)


              newdf=df1()
              temp=as.data.frame(lapply(finalDf()[i,],makeShort,length))
              newdf[i,]=temp[RV$cols]
              if(updateRowname) rownames(newdf)[i]=input$rowname
              df1(newdf)
              replaceData(proxy,df1(),resetPaging=FALSE)


          removeModal()
     })

     observeEvent(input$addOk,{
          # newdf2 <- finalDf()
          # newdf2[i,j]<-DT::coerceValue(input[[paste0("colid",j)]], newdf2[i, j])
          # finalDf(newdf2)
         updateRowname=TRUE
         if(length(grep("[^0-9]",rownames(finalDf())))==0) {
             updateRowname=FALSE
         } else if(input$rowname %in% rownames(finalDf())[-i]){
             updateRowname=FALSE
             showNotification("Duplicated rownames is not allowed",duration=3,type="message")
         }

          newdf2=finalDf()
          newdf2<-rbind(newdf2,newdf2[nrow(newdf2),])
          i=nrow(newdf2)

          newdf2 <- finalDf()
          for(j in 1:ncol(finalDf())){
                  if("POSIXct" %in% class(newdf2[i,j])){
                      newdf2[i,j]<-lubridate::as_datetime(input[[paste0("colid",j)]])
                  } else{
                      newdf2[i,j]<-DT::coerceValue(input[[paste0("colid",j)]], newdf2[i, j])
                  }

          }
          if(updateRowname) rownames(newdf2)[i]=input$rowname
          finalDf(newdf2)

          result=as.data.frame(lapply(finalDf(),makeShort,length))
          rownames(result)=rownames(finalDf())
          newdf=result[RV$cols]
          if(updateRowname) rownames(newdf)[i]=input$rowname
          df1(newdf)
          replaceData(proxy,df1(),resetPaging=FALSE)
          removeModal()
     })

     observeEvent(input$insertUp,{
          i= input$table_rows_selected
          updateRowname=TRUE
          if(length(grep("[^0-9]",rownames(finalDf())))==0) {
              updateRowname=FALSE
          } else if(input$rowname %in% rownames(finalDf())[-i]){
              updateRowname=FALSE
              showNotification("Duplicated rownames is not allowed",duration=3,type="message")
          }
          newdf2=finalDf()
          newdf2<-rbind(newdf2[1:i,],newdf2[i:nrow(newdf2),])

          for(j in 1:ncol(finalDf())){
              if("POSIXct" %in% class(newdf2[i,j])){
                  newdf2[i,j]<-lubridate::as_datetime(input[[paste0("colid",j)]])
              } else{
                  newdf2[i,j]<-DT::coerceValue(input[[paste0("colid",j)]], newdf2[i, j])
              }

          }
          if(updateRowname) rownames(newdf2)[i]=input$rowname
          finalDf(newdf2)
          result=as.data.frame(lapply(finalDf(),makeShort,length))
          rownames(result)=rownames(finalDf())
          newdf=result[RV$cols]
          if(updateRowname) rownames(newdf)[i]=input$rowname
          df1(newdf)
          replaceData(proxy,df1(),resetPaging=FALSE)
          removeModal()
     })
     observeEvent(input$insertDown,{
         i= input$table_rows_selected
         updateRowname=TRUE
         if(length(grep("[^0-9]",rownames(finalDf())))==0) {
             updateRowname=FALSE
         } else if(input$rowname %in% rownames(finalDf())[-i]){
             updateRowname=FALSE
             showNotification("Duplicated rownames is not allowed",duration=3,type="message")
         }

         newdf2=finalDf()
         newdf2<-rbind(newdf2[1:i,],newdf2[i:nrow(newdf2),])
         i=i+1
         for(j in 1:ncol(finalDf())){
             if("POSIXct" %in% class(newdf2[i,j])){
                 newdf2[i,j]<-lubridate::as_datetime(input[[paste0("colid",j)]])
             } else{
                 newdf2[i,j]<-DT::coerceValue(input[[paste0("colid",j)]], newdf2[i, j])
             }

         }
         if(updateRowname) rownames(newdf2)[i]=input$rowname
         finalDf(newdf2)
         result=as.data.frame(lapply(finalDf(),makeShort,length))
         rownames(result)=rownames(finalDf())
         newdf=result[RV$cols]
         if(updateRowname) rownames(newdf)[i]=input$rowname
         df1(newdf)
         replaceData(proxy,df1(),resetPaging=FALSE)
         removeModal()

     })

     data2input=function(data,row){

          lapply(0:ncol(data),vector2input,data,row)

     }


     vector2input=function(x,data,row){
         if(x==0) {
             if(length(grep("[^0-9]",rownames(data)))==0){
                 return(NULL)
             } else{
                 value=rownames(data)[row]
                 length1=nchar(value,keepNA=FALSE)
                 label="rowname"
                 id="rowname"
                 width=max(nchar(rownames(data)[x]),nchar(label),150)
                 if(is.na(length1)){
                     textInput3(ns(id),label,value=value,width=width)
                 } else if(length1<20){
                     textInput3(ns(id),label,value=value,width=width)
                 } else{
                     textAreaInput(ns(id),label,value=value,width="460px",rows=1+length1/60)
                 }
             }
         } else{

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
              if(is.na(value)) value=FALSE
               checkboxInput3(ns(id),label,value=value,width=width)
          } else {
              if(is.na(value)) value=""
              length1=nchar(data[[x]][row],keepNA=FALSE)
              width=max(length1*8,150)
              if(length1<70){
                  textInput3(ns(id),label,value=value,width=width)
              } else{
               textAreaInput(ns(id),label,value=value,width="460px",rows=1+length1/60)
               }
          }
       }


     }

     output$downloadData <- downloadHandler(
          filename = function() {
               "mydata.csv"
          },
          content = function(file) {
               updateRowname=TRUE
               if(length(grep("[^0-9]",rownames(finalDf())))==0) updateRowname=FALSE
               write.csv(finalDf(), file, row.names = updateRowname)
          }
     )

     output$downloadExcel <- downloadHandler(
          filename = function() {
               "mydata.xlsx"
          },
          content = function(file) {
              updateRowname=TRUE
              if(length(grep("[^0-9]",rownames(finalDf())))==0) updateRowname=FALSE
               write.xlsx(finalDf(), file,asTable=TRUE,row.names=updateRowname)
          }
     )
     output$downloadRDS <- downloadHandler(
          filename = function() {
               "mydata.RDS"
          },
          content = function(file) {
               saveRDS(finalDf(), file)
          }
     )

    return(reactive(finalDf()))

}
