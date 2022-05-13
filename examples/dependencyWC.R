library(shiny)
library(shinybound)

# https://www.webcomponents.org/element/@honatas/multi-select-webcomponent

registerComponent("fancyDropdown",
    HTML('

    <script>console.log("shiny")</script>
    <script type="module" src="multi-select-webcomponent.min.js"></script>
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.0/dist/css/bootstrap.min.css">

    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.5.0/font/bootstrap-icons.css">
    <label fsProperty="innerHTML:content" for="planetId" class="form-label p-0">Planets</label>
    <span><slot name = "time"></span>
    <span><slot name = "time2"></span>
    <multi-select fsProperty="disabled" id="planetIds" class="border"
      tsProperty="value"
      tsEvent="change"
      selecteditem="badge bg-primary pt-1 m-1"
      dropdown="border"
      dropdownitem="p-1"
      selectallbutton="btn btn-sm btn-light"
      selectallbuttonspan="bi-check2-all text-success"
      clearbutton="btn btn-sm btn-light"
      clearbuttonspan="bi-x-circle text-danger"
      >
      <option selected value="1">Mercury</option>
      <option value="2">Venus</option>
      <option value="3">Earth</option>
      <option value="4">Mars</option>
      <option value="5">Jupiter</option>
      <option value="6">Saturn</option>
      <option value="7">Uranus</option>
      <option value="8">Neptune</option>
    </multi-select>
    ')
)

# Define UI for application
ui <- fluidPage(
    HTML('
    '),
  useComponent("testID", "fancyDropdown",
    list(content = "init label"),
    myAttribute = "test",
    time = Sys.Date(),
    time2 = Sys.Date()
  ),
  actionButton("update", "Action button")
)

# Define server logic
server <- function(input, output, session) {
  componentScript(session, "testID", HTML("function() {
    const pluto = document.createElement('option');
    pluto.value = 9;
    pluto.innerHTML = 'Pluto';

    const msw = this.getElementById('planetIds');

    msw.options.push(pluto);

    msw.build();
  }"))

  observeEvent(input$testID_value, {
    print(input$testID_value)
  })

  observeEvent(input$update, {
    updateComponent(session, "testID", content = "updated", disabled = TRUE)

    componentScript(session, "testID", HTML("function() {
      const sun = document.createElement('option');
      sun.value = 10;
      sun.innerHTML = 'Sun';

      const msw = this.getElementById('planetIds');

      msw.options.push(sun);

      msw.build();
    }"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
