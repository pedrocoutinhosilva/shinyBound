library(shiny)
library(imola)
library(shinyBound)

# https://nostalgic-css.github.io/NES.css/

registerComponent("fancyButton",
  HTML('
    <head>
      <link href="https://fonts.googleapis.com/css?family=Press+Start+2P" rel="stylesheet">
    </head>
    <link href="https://unpkg.com/nes.css/css/nes.css" rel="stylesheet" />

    <style>
      html, body, pre, code, kbd, samp, div {
          font-family: "Press Start 2P";
      }
    </style>

    <button
      fsProperty="innerHTML:content"
      tsProperty="innerHTML:content"
      fsClass="type"
      tsEvent="click:clicks"
      type="button" class="nes-btn is-primary">Primary</button>
  ')
)

# Define UI for application
ui <- fixedPage(
  actionButton("shinyButton1", "Chance style"),
  useComponent("boundButton1", "fancyButton"),
  useComponent("boundButton2", "fancyButton", list(type = "is-error", content = "Error")),
  actionButton("shinyButton2", "Action Button"),
  actionButton("shinyButton3", "Action Button"),
  actionButton("shinyButton4", "Action Button"),
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$boundButton1, {
    print(input$boundButton1)
  })
  observeEvent(input$boundButton2, {
    print(input$boundButton2)
  })
  observeEvent(input$shinyButton1, {
    updateComponent(session, "boundButton1", type = "is-success", content = "Success")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
