library(shiny)
library(shinyBound)

# Define UI for application
ui <- fluidPage(
  h4("Custom component"),
  uiOutput("out"),
  h4("Update attributes"),
  actionButton("update", "Action button"),
  h4("Custom component Input values"),
  verbatimTextOutput("console_text")
)

# Define server logic
server <- function(input, output, session) {
  output$out <- renderUI({
    component("testID",
      tags$div(
        fsProperty="innerHTML:label",
        fsStyle="color:bling|font-size|border-color:bling",

        tsProperty = "innerHTML:content",
        tsStyle="color:bling|font-size",

        style = "
          border: 1px solid;
          width: 100px; height: 100px;
          display: flex; justify-content: center; align-items: center;
          text-align: center;
        "
      ) |> toString(),
      list(label = "Button to update")
    )
  })

  observeEvent(input$update, {
    updateComponent(session, "testID",
      label = input$update,
      bling = sample(c("red", "green", "blue", "purple"), 1),
      `font-size` = paste0(sample(1:80, 1), "px")
    )
  })

  observeEvent(input$testID, {
    print(input$testID)
    output$console_text <- renderPrint(input$testID)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
