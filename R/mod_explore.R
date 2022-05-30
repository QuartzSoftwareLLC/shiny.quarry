subset_step <- function(input_id) {
    shiny.quartz::HStack(
        shiny.mui::TextField.shinyInput(
            glue::glue("{input_id}_column")
        ),

    )
}

subset_ui <- function(input_id, steps) {
    sapply(seq_along(steps), \(step) {
                subset_step(glue::glue("{input_id}_step_{step}"))
            }) %>%
        do.call(shiny.quartz::VStack, .)
}


#' explore UI Function
#'
#' @noRd
#' @importFrom shiny NS tagList
#' @importFrom shiny.quartz QCard
mod_explore_ui <- function(id) {
    ns <- NS(id)
    QCard(
        title = "explore",
        fileInput(ns("inputData"), "Choose CSV File",
        multiple = TRUE,
        accept = c("text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
        subset_ui(ns("subset"), 1:5),
        shiny.react::reactOutput(ns("head"))
    )
}


#' explore Server Funciton
#'
#' @noRd
mod_explore_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output$head <- shiny.react::renderReact({
            input_data <<- input$inputData
            input_data %>%
                req() %>%
                .$datapath %>%
                read.csv() %>%
                head() %>%
                shiny.quartz::QDataGrid()
        })



    })

}

