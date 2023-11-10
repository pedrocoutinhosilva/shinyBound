library(shiny)
library(shinyBound)

# Define UI for application
ui <- fluidPage(
  useShinyBound(),
  actionButton("update", "Action button"),
  actionButton("updatein", "Update IN"),
  h4("Custom component"),
  uiOutput("out"),
  h4("Update attributes"),
  h4("Custom component Input values"),
  verbatimTextOutput("console_text")
)

# Define server logic
server <- function(input, output, session) {
  output$out <- renderUI({
    tagList(
      component("sneaky", h1(fsProperty = "innerHTML:label", "big test"), list(label = "starter")),
      component("testID",
        tags$div(
          fsProperty="innerHTML:label",
          fsStyle="color:bling|font-size|border-color:bling",

          style = "
            border: 1px solid;
            width: 100px; height: 100px;
            display: flex; justify-content: center; align-items: center;
            text-align: center;
          ",

          tags$style("p {background: red;}"),
          tags$script("console.log(document.querySelector('div'))"),
          tags$script("console.log(document.querySelector('div'))")
        ),
        list(label = component("test", h1(fsProperty = "innerHTML:label", "big test"), list(label = component("inner2", h1(fsProperty = "innerHTML:label", "bigger test"), list(label = "starter")))))
      )
    )
  })

  observeEvent(input$update, {
    updateComponent(session, "testID",
      label = input$update,
      bling = sample(c("red", "green", "blue", "purple"), 1),
      `font-size` = paste0(sample(1:80, 1), "px")
    )
  })

  observeEvent(input$updatein, {
    print(input$updatein)
    updateComponent(session, "test",
      label = input$updatein
    )
  })


  observeEvent(input$testID, {
    print(input$testID)
    output$console_text <- renderPrint(input$testID)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
