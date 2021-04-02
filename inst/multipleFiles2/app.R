library(editData)
library(tidyverse)
library(shiny)

ui <- fluidPage(
    h2("Edit Multiple Files"),
    p("You can edit upto four files side by side."),
    checkboxInput("sidebyside","Side by side",value=TRUE),
    fileInput("file","Select File(s) to edit",multiple=TRUE),
    uiOutput("editUI")

)
server <- function(input, output) {


    output$editUI=renderUI({
        data<-uiname<-result<-mylist<-textname<-downloadname<-downloadname2<-list()

        count=length(input$file$datapath)

        if(count>0) {
            for(i in  1:count){
                # data[[i]]<-readr::read_csv(input$file$datapath[i],comment="#")
                data[[i]]<-myimport(input$file$datapath[i])
                uiname[[i]]<-paste0("table",i)
                title=paste0("File No ",i," : ",input$file$name[i])
                mylist[[3*i-2]]<-h2(title)
                mylist[[3*i-1]]<-editableDTUI(uiname[[i]])
                textname[[i]]=paste0("text",i)

                mylist[[3*i]]<-verbatimTextOutput(textname[[i]])


                local({
                    j<-i
                    result[[j]]=callModule(editableDT,uiname[[j]],data=reactive(data[[j]]))

                    output[[textname[[j]]]]=renderPrint({
                     head(result[[j]]())
                    })


                })
            }
            colwidth=12/count

            if(input$sidebyside){
            tagList(
                 fluidRow(
                      column(colwidth,
                              mylist[1:3]
                             ),
                      if(count>1) column(colwidth,
                             mylist[4:6]
                             ),
                      if(count>2) column(colwidth,
                                        mylist[7:9]
                      ),
                      if(count>3) column(colwidth,
                                         mylist[10:12]
                      )
                 )
            )
            } else{
               do.call(tagList,mylist)
            }
        }

    })

}
shinyApp(ui, server)
