library(shiny)
library(editData)

ui <- fluidPage(
    h2("Data 1"),
    textInput("mydata","Enter data name",value="mtcars"),
    editableDTUI("table1"),
    verbatimTextOutput("test"),
    h2("Data 2"),
    textInput("mydata2","Enter data name",value="sampleData"),
    editableDTUI("table2"),
    verbatimTextOutput("test2")
)
server <- function(input, output) {

    data=reactive({
            myget(input$mydata)
    })

    data2=reactive({
            myget(input$mydata2)
    })

    df=callModule(editableDT,"table1",data=reactive(data()))

    output$test=renderPrint({
         str(df())
    })
    #mydf<-editData::sampleData
    df2=callModule(editableDT,"table2",data=data2)
    output$test2=renderPrint({
         str(df2())
    })
}
shinyApp(ui, server)
