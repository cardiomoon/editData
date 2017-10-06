library(editData)
library(tidyverse)
library(shiny)


ui <- fluidPage(
    fileInput("file","Upload File(s) to edit",multiple=TRUE),
    uiOutput("editUI")
    
)
server <- function(input, output) {
    
    
    output$editUI=renderUI({
        data<-uiname<-result<-mylist<-textname<-list()
        
        count=length(input$file$datapath)
        
        if(count>0) {
            for(i in  1:count){
               
                
                   
                data[[i]]<-read.csv(input$file$datapath[i],stringsAsFactors = FALSE)
                uiname[[i]]<-paste0("table",i)
                title=paste0("File No:",i)
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
            do.call(tagList,mylist)
        }
       
    })
    
}
shinyApp(ui, server)
