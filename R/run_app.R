options(shiny.maxRequestSize = 30 * 1024^2)

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
    # Your application server logic
    global_state <- reactiveValues()
    global_state$data <- mod_data_server("data")

    data <- reactiveVal()
    observeEvent(input$process, {
        data(global_state$data())
    })

mod_summary_server("summary", global_state$data)
mod_tableone_server("tableone", global_state$data)

}

#' @importFrom shiny.quartz QCard Container Item QSelect.shinyInput make_options AcknowledgementCard
app_ui <- function(request) {
    shiny.quartz::Page(
        "Data",
        shiny.quartz::VStack(
            mod_data_ui("data"),
            mod_summary_ui("summary"),
            mod_tableone_ui("tableone")
        )
    )
}


#' @export
run_app <- function() {
    shiny::shinyApp(
        server = app_server,
        ui = app_ui,
    )
}