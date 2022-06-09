options(shiny.launch.browser = FALSE)



#' tableone UI Function
#'
#' @noRd
#' @importFrom shiny NS tagList
#' @importFrom shiny.quartz QCard
mod_tableone_ui <- function(id) {
    ns <- NS(id)
    QCard(
        title = "Table One",
        shiny.quartz::VStack(
        shiny.react::reactOutput(ns("controls")),
        shiny.mui::Box(
            overflow = "scroll",
            tableOutput(ns("tableone"))
        )
        )
    )
}


#' tableone Server Funciton
#'
#' @noRd
mod_tableone_server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output$controls <- shiny.react::renderReact({
            shiny.quartz::QThemeProvider(
                shiny.quartz::HStack(
                    shiny.mui::Autocomplete.shinyInput(
                        ns("columns"),
                        multiple = T,
                        fullWidth = T,
                        value = list(),
                        options = colnames(data()) %>% .[sapply(data(), is.factor)],
                        inputProps = list(label = "Columns")
                    ),
                    shiny.mui::Autocomplete.shinyInput(
                        ns("rows"),
                        multiple = T,
                        fullWidth = T,
                        value = list(),
                        options = colnames(data()),
                        inputProps = list(label = "Rows")
                    )
                )
            )
        })

        output$tableone <- renderTable({
            req(input$columns)
            req(input$rows)
            table1::table1(        
                paste0(
                    "~",
                    paste0(input$rows, collapse = "+"),
                    "|",
                    paste0(input$columns, collapse = "+")
                ) %>%
                    as.formula(),
                data = data())
        })
    })
}

