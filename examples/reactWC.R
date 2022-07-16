library(shiny)
library(imola)
library(shinyBound)

# https://github.com/thecreazy/framework-webcomponents/

registerComponent("fancyDropdown",
    HTML('
    <h1>Exmple of webcomponent in react</h1>
    <button id="reset">set value to random</button>
    <react-counter startvalue="10" id="react-counter">
    <script src="bundle.js"></script>
    ')
)

# Define UI for application
ui <- flexPage(
    HTML('
    '),
  useComponent("testID", "fancyDropdown",
    list(content = "init label")
  )
)

# Define server logic
server <- function(input, output, session) {
  componentScript(session, "testID", HTML('function() {

    console.log(this);

    this.querySelector("#reset").addEventListener("click", () => {
        this.querySelector("#react-counter").setAttribute("startvalue", Math.floor(Math.random() * 100000));
    });
  }'))


  observeEvent(input$testID, {

    print(input$testID)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
