#' summary UI Function
#'
#' @noRd
#' @importFrom shiny NS tagList
#' @importFrom shiny.quartz QCard
mod_summary_ui <- function(id) {

Subhead <- function(text) {
    shiny.mui::Typography(variant = "h5", text)
}
    ns <- NS(id)
    tagList(
        QCard(
            title = "Introduction",
            shiny.quartz::VStack(
                shiny.mui::Typography(
                    variant = "h6",
                    children = "Data Viewer"
                ),
                shiny.react::reactOutput(ns("preview")),
                shiny.mui::Typography(
                    variant = "h6",
                    children = "Summary"
                ),
                shiny.react::reactOutput(ns("introduce"))
            )
        ),
        QCard(
            title = "Summary Plots",
            shiny.quartz::VStack(
                Subhead("Frequency Bar Plots"),
                plotOutput(ns("bar")),
                Subhead("Histogram Plots"),
                plotOutput(ns("histogram")),
                Subhead("Density Plots"),
                plotOutput(ns("density")),
                Subhead("Missing Values"),
                plotOutput(ns("missing")),
                Subhead("Variable Correlation"),
                plotOutput(ns("correlation"))
            )

        )
    )
}


#' summary Server Funciton
#'
#' @noRd
mod_summary_server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        output$introduce <- renderDataGrid(
            reactive({data() %>%
                DataExplorer::introduce() %>%
                tidyr::pivot_longer(everything())})
        )

output$bar <- renderPlot({
    DataExplorer::plot_bar(data())
})

output$histogram <- renderPlot({
    DataExplorer::plot_histogram(data())
})
output$density <- renderPlot({
    DataExplorer::plot_density(data())
})
output$correlation <- renderPlot({
    DataExplorer::plot_correlation(data())
})
output$missing <- renderPlot({
    DataExplorer::plot_missing(data())
})
        output$preview <- renderDataGrid(
            data
        )
    })
}
