#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    # Your application server logic
    mod_explore_server("explore")
}

#' @importFrom shiny.quartz QCard Container Item QSelect.shinyInput make_options AcknowledgementCard
app_ui <- function(request) {
    shiny.quartz::Page(
        "Explore",
        mod_explore_ui("explore")
    )
}


#' @export
run_app <- function() {
    shiny::shinyApp(
        server = app_server,
        ui = app_ui,
    )
}