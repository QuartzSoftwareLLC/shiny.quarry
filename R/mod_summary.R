#' summary UI Function
#'
#' @noRd
#' @importFrom shiny NS tagList
#' @importFrom shiny.quartz QCard
mod_summary_ui <- function(id) {
    ns <- NS(id)
    QCard(
        title = "Introduction",
        shiny.quartz::VStack(
            shiny.mui::Typography(
                variant = "h6",
                children = "Summary"
            ),
            shiny.react::reactOutput(ns("introduce")),
            shiny.mui::Typography(
                variant = "h6",
                children = "Data Viewer"
            ),
            shiny.react::reactOutput(ns("preview"))
        )
    )
}


#' summary Server Funciton
#'
#' @noRd
mod_summary_server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output$introduce <- renderDataGrid({
            data() %>%
                DataExplorer::introduce() %>%
                tidyr::pivot_longer(everything())
        })
        output$preview <- renderDataGrid(
            data()
        )
    })
}
