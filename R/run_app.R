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
    # global_state$subset <- mod_subset_server("subset", global_state$data)

    data <- reactiveVal()
    observeEvent(input$process, {
        data(global_state$data())
    })
    output$preview <- shiny.react::renderReact({
        if(!is.null(data())) {
            data() %>%
                head(100) %>%
                shiny.quartz::QDataGrid()
        }
        else {
            div()
        }
    })
}

#' @importFrom shiny.quartz QCard Container Item QSelect.shinyInput make_options AcknowledgementCard
app_ui <- function(request) {
    shiny.quartz::Page(
        "Data",
        mod_data_ui("data"),
        # mod_subset_ui("subset"),
        shiny.mui::Button.shinyInput("process", "Process", color = "primary", variant="contained", fullWidth = T),
        shiny.react::reactOutput("preview")
    )
}


#' @export
run_app <- function() {
    shiny::shinyApp(
        server = app_server,
        ui = app_ui,
    )
}