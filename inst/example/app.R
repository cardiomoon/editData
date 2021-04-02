library(shiny)
library(editData)


ui <- fluidPage(
    textInput("mydata","Enter data name",value="mtcars"),
    editableDTUI("table1"),
    verbatimTextOutput("test"),
    textInput("mydata2","Enter data name",value="sampleData"),
    editableDTUI("table2"),
    verbatimTextOutput("test2")
)
server <- function(input, output) {

    data=reactive({
        if (!is.null(input$mydata) && nzchar(input$mydata) &&
            exists(input$mydata) && is.data.frame(get(input$mydata)))
            # eval(parse(text=input$mydata))
            get(input$mydata)
    })

    data2=reactive({
        if (!is.null(input$mydata2) && nzchar(input$mydata2) &&
            exists(input$mydata2) && is.data.frame(get(input$mydata2)))
            # eval(parse(text=input$mydata))
            get(input$mydata2)
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
