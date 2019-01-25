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
                mylist[[5*i-4]]<-h2(title)
                mylist[[5*i-3]]<-editableDTUI(uiname[[i]])
                textname[[i]]=paste0("text",i)
                downloadname[[i]]=paste0("download",i)
                downloadname2[[i]]=paste0("downloadRDS",i)
                mylist[[5*i-2]]<-verbatimTextOutput(textname[[i]])
                mylist[[5*i-1]]<-downloadButton(downloadname[[i]],"download as csv")
                mylist[[5*i]]<-downloadButton(downloadname2[[i]],"download as RDS")

                local({
                    j<-i
                    result[[j]]=callModule(editableDT,uiname[[j]],data=reactive(data[[j]]),
                                           mode=reactive(2))

                    output[[textname[[j]]]]=renderPrint({
                     head(result[[j]]())
                    })

                    output[[downloadname[[j]]]]=downloadHandler(
                         filename=paste0(downloadname[[j]],".csv"),
                         content=function(file){
                              readr::write_csv(result[[j]](),file)
                         },
                         contentType="text/csv"
                    )
                    output[[downloadname2[[j]]]]=downloadHandler(
                         filename=paste0(downloadname[[j]],".RDS"),
                         content=function(file){
                              saveRDS(result[[j]](),file)
                         },
                         contentType="text/RDS"
                    )
                })
            }
            colwidth=12/count

            if(input$sidebyside){
            tagList(
                 fluidRow(
                      column(colwidth,
                              mylist[1:5]
                             ),
                      if(count>1) column(colwidth,
                             mylist[6:10]
                             ),
                      if(count>2) column(colwidth,
                                        mylist[11:15]
                      ),
                      if(count>3) column(colwidth,
                                         mylist[16:20]
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
