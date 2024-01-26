library(shiny)
library(shinyBound)
library(imola)
library(stringi)

# Define UI for application
ui <- fillPage(
  HTML('
    <!-- Load dependencies -->
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/codemirror.min.css">
    <script src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/codemirror.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/mode/xml/xml.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/mode/javascript/javascript.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/mode/css/css.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/mode/htmlmixed/htmlmixed.js"></script>
    <script src="https://codemirror.net/2/lib/util/formatting.js"></script>
    <style>
      .CodeMirror-scroll {
        min-height: 350px;
        max-height: 80vh;
      }

      .code-editor button {
        width: 100%;
        height: 50px;
        background: #f5f5f5;
        font-weight: bold;
        border: 1px solid #cccccc;
      }

    </style>
  '),
  gridPanel(
    columns = "50px 1fr 1fr 50px",
    rows = "50px 50px 1fr 1fr 50px",
    areas = c(
      "... ... ... ...",
      "... examples export ...",
      "... code preview ...",
      "... state outputs ...",
      "... ... ... ..."
    ),
    examples = flexPanel(
      actionButton("help", "Help"),
      div(style = "min-width: 3vw"),
      div(style = "display:flex; justify-content: center; align-items: center;", "Load Examples:"),
      actionButton("exampleButton", "Button"),
      actionButton("exampleDropdown", "Dropdown"),
      actionButton("exampleKPI", "KPI")
    ),
    code = div(class = "code-editor"),
    preview = uiOutput("out") |> tagAppendAttributes(style = "display: flex; justify-content: center; align-items: center;"),
    state = uiOutput("state"),
    outputs = verbatimTextOutput("values"),
    export = div(
      style = "display: flex; justify-content: center;",
      actionButton("exportCode", "Generate Code") |> tagAppendAttributes(style = "height: 100%; width: 50%;")
    )
  ),
  HTML('<script src="code-editor.js"></script>')
)

# Define server logic
server <- function(input, output, session) {
  state <- reactiveValues(
    id = "",
    args = c(),
    inputs = c(),
    cache = list(
      label = "A label"
    )
  )

  session$sendCustomMessage("code", readLines("www/button.html") |> paste(collapse = "\n"))

  observeEvent(input$exampleButton, {
    session$sendCustomMessage("code", readLines("www/button.html") |> paste(collapse = "\n"))
  })
  observeEvent(input$exampleDropdown, {
    session$sendCustomMessage("code", readLines("www/dropdown.html") |> paste(collapse = "\n"))
  })
  observeEvent(input$exampleKPI, {
    session$sendCustomMessage("code", readLines("www/kpi.html") |> paste(collapse = "\n"))
  })

  observeEvent(input$code, {
    newID <- Sys.time() |>
      as.integer() |>
      paste0(stri_rand_strings(1, 12))

    newID <- paste0("sb", newID)
    state$id <- newID

    newUI <- input$code
    options <- getComponentBinds(newUI)
    state_options <- options$from_shiny |>
      lapply(function(single) {
        single$state
      }) |>
      unlist() |>
      unique()

    output$out <- renderUI({
      print(isolate({
        state$cache
      }))

      do.call(
        component,
        modifyList(
          list(newID, newUI),
          isolate({
            state$cache
          })
        )
      )
    })

    state$args <- state_options
    state$inputs <- c()

    state_options |> lapply(function(option) {
      state$inputs <- c(state$inputs, paste0(newID, option))

      observeEvent(input[[paste0(newID, option)]],
        {
          update_arg <- option
          update_value <- input[[paste0(newID, option)]]

          state$cached[[update_arg]] <- update_value

          args <- list()
          args[[update_arg]] <- update_value

          try({
            print(update_arg, update_value)
          })

          do.call(
            updateComponent,
            modifyList(
              list(session, newID),
              args
            )
          )
        },
        ignoreInit = TRUE
      )
    })

    output$state <- renderUI({
      div(
        h3("Server Side State Updates"),
        state_options |> lapply(function(option) {
          print(isolate({
            state$cached[[option]]
          }))

          textInput(paste0(newID, option), option, value = isolate({
            state$cached[[option]]
          }))
        })
      )
    })

    observeEvent(input[[newID]], {
      print(input[[newID]])
      output$values <- renderPrint({
        input[[newID]]
      })
    })
  })

  observeEvent(input$exportCode, {
    prettyContent <- strsplit(input$code, "\n") |>
      unlist() |>
      lapply(function(line) {
        paste0("    ", line)
      }) |>
      unlist()


    argsLine <- c(
      "# install package from github",
      "# pak::pak(\"pedrocoutinhosilva/shinyBound\")",
      "library(shinyBound)",
      "",
      "component(\"myID\", '",
      prettyContent,
      "  '"
    )



    for (index in seq_len(length(state$args))) {
      argsLine <- c(
        head(argsLine, -1),
        paste0(tail(argsLine, 1), ","),
        paste0("  ", state$args[index], " = ", '"', input[[state$inputs[index]]], '"')
      )
    }

    codeLines <- c(
      argsLine,
      ")"
    )


    modal_content <- tagList(
      h3("UI Initialization"),
      codeLines |> paste0(collapse = "\n") |> tags$pre(),
      if (length(state$args) > 0) {
        tagList(
          h3("Update values in server"),
          paste("updateComponent(\"myID\", ", lapply(seq_len(length(state$args)), function(index) {
            paste0("", state$args[index], " = ", '"', input[[state$inputs[index]]], '"')
          }) |> unlist() |> paste0(collapse = ", "), ")") |> tags$pre()
        )
      }
    )

    showModal(modalDialog(
      title = "Generated Code",
      size = "l",
      modal_content,
      easyClose = TRUE,
      footer = NULL
    ))
  })

  observeEvent(input$help, {
    modal_content <- includeMarkdown("www/help.md")


    showModal(modalDialog(
      title = "Creating Components",
      size = "l",
      modal_content,
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
