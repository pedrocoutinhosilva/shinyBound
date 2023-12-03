library(shiny)
library(imola)
library(shinyBound)

# https://www.fast.design/docs/components/getting-started/

component_html <- HTML('
    <script type="module" src="https://cdn.jsdelivr.net/npm/@microsoft/fast-components@2.16.0/dist/fast-components.min.js"></script>
    <fast-card>
      <div style="padding: 0 10px 10px; color: var(--neutral-foreground-rest);">
        <h3 fsProperty="innerHTML:content">Card title</h3>

        <fast-select tsEvent="change" tsProperty="value" id="shirt-size">
            <fast-option value="s">Small</fast-option>
            <fast-option value="m">Medium</fast-option>
            <fast-option value="l">Large</fast-option>
            <fast-option value="xl">Extra Large</fast-option>
        </fast-select>

        <p>
          At purus lectus quis habitant commodo, cras. Aliquam malesuada velit
          a tortor. Felis orci tellus netus risus et ultricies augue aliquet.
        </p>
        <fast-button appearance="primary" tsEvent="click">Learn more</fast-button>
      </div>
    </fast-card>
    ')

# Define UI for application
ui <- flexPage(
  h2("Wrapping a fast UI component into a widget"),
  component(
    "testID", component_html,
    list(content = "init label")
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$testID, {
    print(input$testID)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
