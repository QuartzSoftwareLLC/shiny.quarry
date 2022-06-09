data <- jsonlite::fromJSON("https://epi.quartzsoftware.com/api/data")
options <- setNames(data$title, data$link)

factor_convert <- function(x, len) {
    val <- length(unique(x))

    if (val < len) {
        x <- factor(x)
    }
    return(x)
}


#' data UI Function
#'
#' @noRd
#' @importFrom shiny NS tagList
#' @import shiny.quartz
mod_data_ui <- function(id) {

    ns <- NS(id)
    QCard(
        title = "Choose Dataset",
        VStack(
            QSelect.shinyInput(
                ns("source"),
                label = "Data Source",
                options = make_options("Upload", "IDDR"),
                value = "IDDR"
            ),
            conditionalPanel(condition = "input.source == 'Upload'", ns = ns,
                fileInput(ns("inputData"), "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv"))
            ),
            conditionalPanel(condition = "input.source == 'IDDR'", ns = ns,
                QSelect.shinyInput(
                    ns("iddrSource"),
                    label = "IDDR Source",
                    options = options,
                    value = "https://s3.amazonaws.com/quartzdata/datasets/influenza-burden.csv"
                )
            ),        
        # subset_ui(ns("subset"), 1:5),
            shiny.react::reactOutput(ns("preview"))
        )
    )
}


#' data Server Funciton
#'
#' @noRd
mod_data_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        data <- reactive({
            if (input$source == "Upload") {
                if(is.null(input$inputData)) {
                    return(NULL)
                }
                else {
                    input$inputData %>%
                        .$datapath %>%
                        read.csv() %>%
                        return()
                }
            }
            else {
                return(tidyiddr::cache_download(input$iddrSource))
            }
        })

        factored_data <- reactive({
            data() %>%
                dplyr::mutate_all(factor_convert, len = 10)
        })

        return(factored_data)
    })


}

