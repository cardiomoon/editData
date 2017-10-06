library(shiny)
library(editData)


ui <- fluidPage(
    textInput("mydata","Enter data name",value="mtcars"),
    editableDTUI("table1"),
    verbatimTextOutput("test"),
    editableDTUI("table2"),
    verbatimTextOutput("test2")
)
server <- function(input, output) {
    df=callModule(editableDT,"table1",dataname=reactive(input$mydata),inputwidth=reactive(170))

    output$test=renderPrint({
         str(df())
    })
    mydf<-editData::sampleData
    df2=callModule(editableDT,"table2",data=reactive(mydf))
    output$test2=renderPrint({
         str(df2())
    })
}
shinyApp(ui, server)
