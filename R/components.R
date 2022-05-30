comparitors <- list(
    GREATER_THAN = '>',
    LESS_THAN = '<',
    EQUAL_TO = '==',
    NOT_EQUAL_TO = '!='
    # IN = `%in%`,
    # NOT_IN = Negate(`%in%`)
)

operators <- list(
    AND = '&',
    OR = '|'
)

#' qsubset
#' 
#' @param data the data to subset
#' @param the column on which to subset data
#' @param the comparitor to use
#' @param the value to compare against
#' @example inst/examples/qsubset.R
#' @export
qexpression <- function(data, column = NULL, comparitor = '==', value = NULL, operator = '&') {

    # validate and update column
    if (is.null(column)) {
        column <- colnames(data)[1]
    }
    if (!(column %in% colnames(data))) {
        stop(glue::glue("Column '{column}' not found"))
    }
    col <- data[[column]]


    # validate and update value
    if (is.null(value)) {
        if (is.numeric(value)) {
            value <- 0
        } else {
            value <- first(col)
        }
    } else {
        if(!is.numeric(value) & (! value %in% col)) {
            stop(glue::glue("Value '{value}' not found in column '{column}'"))
        }
    }

    if(!(operator %in% operators)) {
        stop(glue::glue("Operator '{operator}' not found"))
    }
    
    # validate comparitor
    if (!(comparitor %in% comparitors)) {
        stop(glue::glue("Comparitor '{comparitor}' not found"))
    }
    if (is.factor(col) &
        comparitor %in% c(
         comparitors$GREATER_THAN,
         comparitors$LESS_THAN)) {
        stop(glue::glue("Cannot compare factors with '{comparitor}'"))
    }

    if (!(operator %in% c("&", "|"))) {
        stop(glue::glue("Operator '{operator}' not found"))
    }

    structure(
        list(
            data = data,
            column = column,
            comparitor = comparitor,
            operator = operator,
            value = value),
        class = "qexpression")
}

is.qexpression <- function(x) {
    inherits(x,  "qexpression")
}


#' qsubset
#' @param data the data to subset
#' @param qexpression single qepression or list of qexpressions to subset with
#' @example inst/examples/qsubset.R
#' @export
qsubset <- function(data, qexpressions) {
    get_filter <- \(sub) get(sub$comparitor)(data[[sub$column]], sub$value)


    if (is.qexpression(qexpressions)) {
        index <- get_filter(qexpressions)
    } else if (length(qexpressions) == 1) {
        index <- get_filter(qexpressions[[1]])
    } else {
        index <- qexpressions %>%
            Reduce(\(prev, curr) {
                get(curr$operator)(prev, get_filter(curr))
            }, ., TRUE)
    }

    subset(data, index)
}


add_qexpression <- function(input_id) {
    shiny.mui::Divider(
        shiny.mui::Button.shinyInput(
            glue::glue("{input_id}_add"),
            "( + ) Add Filter"
        )
    )
}

#' @examples
#' data(mtcars)
#' component_example(subset_step("test", mtcars))
subset_step <- function(input_id, data, expression = NULL) {
    if(is.null(expression)) {
        expression <- qexpression(data)
    }

    value_input <- shiny.mui::TextField.shinyInput(
        glue::glue("{input_id}_value"),
        label = "Value",
        value = expression$value,
        sx = list(flexGrow = 1)
    )

    tagList(
        shiny.quartz::HStack(shiny.quartz::QSelect.shinyInput(
                glue::glue("{input_id}_column"),
                options = shiny.quartz::make_options(colnames(data)),
                label = "Column",
                value = expression$column
            ),
            shiny.quartz::QSelect.shinyInput(
                glue::glue("{input_id}_comparitor"),
                options = comparitors,
                value = expression$comparitor,
                label = ""
            ),
            value_input,
            shiny.mui::Button.shinyInput(glue::glue("{input_id}_delete"), "X", color = "error")
        ),
        add_qexpression(paste0(input_id, "_add") %>% print())
    )
}



#' Subset UI
#' @param input_id shiny input id
#' @param steps dictionary of steps
#' @param columns available columns
#' @examples 
#' data(mtcars)
#' component_example(subset_ui("test", mtcars, list(qexpression(mtcars), qexpression(mtcars))))
subset_ui <- function(input_id, data, steps) {
        lapply(seq_along(steps), \(step) {
                subset_step(
                    glue::glue("{input_id}_step_{step}"),
                    data,
                    steps[[step]]
                )
            }) %>%
            shiny.quartz::VStack(add_qexpression(input_id), ., shiny.mui::Button.shinyInput("subset", "Subset"))
    
}
        options(shiny.launch.browser = FALSE)

shinyApp(
    ui = tagList(shiny.react::reactOutput("filters"),
        shiny.react::reactOutput("filtered")),
    server = function(input, output) {
        data <- mtcars

        expressions <- reactiveVal(list())

        output$filters <- shiny.react::renderReact({
            subset_ui(
                "test",
                mtcars,
                expressions()
            )
        })

        firstLoad <- 2
        observe_sub <- function(sub) names(input) %>% .[grep(sub, .)] %>% lapply(\(x) input[[x]])
        observeEvent(observe_sub("_add"), {
            if(firstLoad) {
                firstLoad <<- firstLoad - 1
                return()
            }
            else {
                firstLoad <<- 1
            }
            
            expressions() %>%
                append(list(qexpression(data))) %>%
                expressions()
   
        })

        observeEvent(input$test_step_1_delete, {
            expressions(expressions()[-1])
        })

        filteredData <- reactiveVal()

        output$filtered <- shiny.react::renderReact({
            req(!is.null(filteredData))
            shiny.quartz::QDataGrid(filteredData())
        })
        observeEvent(observe_sub("value|label|comparitor"), {
            expressions() %>%
                lapply(\(x) {
                    x$value <- input[[paste0("test_step_", x$step, "_value")]]
                    x$column <- input[[paste0("test_step_", x$step, "_column")]]
                    x$comparitor <- input[[paste0("test_step_", x$step, "_comparitor")]]
                    x
                }) %>%
                expressions()

        })
        observeEvent(input$subset, {
            exp <<- expressions()

            data %>%
                qsubset(expressions()) %>%
                filteredData()
        })

    }
)
