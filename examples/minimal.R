library(shiny)
library(imola)
library(shinyBound)

# https://nostalgic-css.github.io/NES.css/

component_html <- HTML('
  <head>
    <link href="https://fonts.googleapis.com/css?family=Press+Start+2P" rel="stylesheet">
  </head>
  <link href="https://unpkg.com/nes.css/css/nes.css" rel="stylesheet" />
  <script> console.log("tag") </script>

  <style>
    html, body, pre, code, kbd, samp, div {
        font-family: "Press Start 2P";
    }
    .nes-btn {
        border-image-repeat: repeat;
    }
  </style>

  <button
    fsProperty="innerHTML:content"
    tsProperty="innerHTML:content"
    fsClass="is-{type}:type"
    tsEvent="click:clicks"
    type="button" class="nes-btn is-primary">Primary</button>
')

# Define UI for application
ui <- fixedPage(
  actionButton("shinyButton1", "Chance style"),
  component("boundButton1", component_html),
  component("boundButton2", component_html, list(type = "error", content = "Error")),
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
    updateComponent(session, "boundButton1", type = "success", content = "Success")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
