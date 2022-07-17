library(shiny)
library(imola)
library(shinyBound)

# https://github.com/thecreazy/framework-webcomponents/

registerComponent("fancyDropdown",
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

    <button type="button" class="nes-btn is-primary">Primary</button>
  ')
)

# Define UI for application
ui <- fixedPage(
  actionButton("shinyButton1", "Action Button"),
  useComponent("testID", "fancyDropdown"),
  actionButton("shinyButton2", "Action Button"),
  actionButton("shinyButton3", "Action Button"),
  actionButton("shinyButton4", "Action Button"),
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$testID, {
    print(input$testID)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
