#' @importFrom rvest read_html
#' @importFrom rvest html_elements
#' @importFrom rvest html_attr
#' @export
getComponentBinds <- function(template) {
  template <- template |>
    read_html()

  types <- c("property", "attribute", "style", "class", "event")

  to_shiny <- list()
  from_shiny <- list()

  for (type in types) {
    ts_type <- template |>
      html_elements(css = paste0("[ts", type, "]"))

    if (length(ts_type) > 0) {
      total <- ts_type |>
        html_attr(paste0("ts", type))

      breakdown <- total |> lapply(function(single) {
        strsplit(single, "|", fixed = TRUE)[[1]]
      })

      breakdown <- breakdown |>
        unlist() |>
        lapply(function(single) {
          strsplit(single, ":", fixed = TRUE)[[1]]
        })

      for (single in breakdown) {
        if (length(single) == 1) {
          single <- c(single, single)
        }

        to_shiny <- append(to_shiny, list(list(
          state = single[2],
          value = single[1],
          type = type
        )))
      }
    }

    fs_type <- template |>
      html_elements(css = paste0("[fs", type, "]"))

    if (length(fs_type) > 0) {
      total <- fs_type |>
        html_attr(paste0("fs", type))

      breakdown <- total |> lapply(function(single) {
        strsplit(single, "|", fixed = TRUE)[[1]]
      })

      breakdown <- breakdown |>
        unlist() |>
        lapply(function(single) {
          strsplit(single, ":", fixed = TRUE)[[1]]
        })

      for (single in breakdown) {
        if (length(single) == 1) {
          single <- c(single, single)
        }

        from_shiny <- append(from_shiny, list(list(
          state = single[2],
          value = single[1],
          type = type
        )))
      }
    }
  }

  list(
    to_shiny = to_shiny,
    from_shiny = from_shiny
  )
}

#' @importFrom rvest read_html
#' @importFrom rvest html_elements
#' @importFrom rvest html_attr
#' @export
getComponentHelp <- function(template) {
  template <- template |>
    read_html()

  types <- c("property", "attribute", "style", "class", "event")

  to_shiny <- list()
  from_shiny <- list()

  for (type in types) {
    ts_type <- template |>
      html_elements(css = paste0("[ts", type, "]"))


    if (length(ts_type) > 0) {
      total <- ts_type |>
        html_attr(paste0("ts", type))

      breakdown <- total |> lapply(function(single) {
        strsplit(single, "|", fixed = TRUE)[[1]]
      })

      breakdown <- breakdown |>
        unlist() |>
        lapply(function(single) {
          strsplit(single, ":", fixed = TRUE)[[1]]
        })

      to_shiny[[type]][["breakdown"]] <- list()
      to_shiny[[type]][["raw"]] <- list()

      for (single in breakdown) {
        if (length(single) == 1) {
          single <- c(single, single)
        }
        to_shiny[[type]][["breakdown"]][[single[1]]] <- single[2]
      }


      to_shiny[[type]][["raw"]] <- ts_type |>
        html_attr(paste0("ts", type))
    }

    fs_type <- template |>
      html_elements(css = paste0("[fs", type, "]"))

    if (length(fs_type) > 0) {
      total <- fs_type |>
        html_attr(paste0("fs", type))

      breakdown <- total |> lapply(function(single) {
        strsplit(single, "|", fixed = TRUE)[[1]]
      })

      breakdown <- breakdown |>
        unlist() |>
        lapply(function(single) {
          strsplit(single, ":", fixed = TRUE)[[1]]
        })

      from_shiny[[type]][["breakdown"]] <- list()
      from_shiny[[type]][["raw"]] <- list()

      for (single in breakdown) {
        if (length(single) == 1) {
          single <- c(single, single)
        }

        from_shiny[[type]][["breakdown"]][[single[1]]] <- single[2]
      }


      from_shiny[[type]][["raw"]] <- fs_type |>
        html_attr(paste0("fs", type))
    }
  }

  list(
    to_shiny = to_shiny,
    from_shiny = from_shiny
  )
}
