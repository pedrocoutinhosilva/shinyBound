library(shiny)
library(imola)
library(shinyBound)

# https://github.com/thecreazy/framework-webcomponents/

# Define UI for application
ui <- flexPage(
  component(
    "testID", HTML('
      <h1>Updating a react app embeded in a webcomponent</h1>
      <button id="reset">set value to random</button>
      <react-counter startvalue="10" id="react-counter">
      <script src="bundle.js"></script>
    '),
    list(content = "init label")
  )
)

# Define server logic
server <- function(input, output, session) {
  componentScript(session, "testID", HTML('function() {
    this.querySelector("#reset")
      .addEventListener("click", () => {
          this.querySelector("#react-counter")
            .setAttribute("startvalue", Math.floor(Math.random() * 100000));
      });
  }'))
}

# Run the application
shinyApp(ui = ui, server = server)
