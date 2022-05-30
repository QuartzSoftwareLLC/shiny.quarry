select_ui <- function(input_id, data) {
    shiny.quartz::VStack(
        shiny.mui::Autocomplete.shinyInput(
            glue::glue("{input_id}_select"),
            multiple = T,
            options = colnames(data),
            value = colnames(data)
        ),
        shiny.mui::Button.shinyInput("select", "Select")
    )
}


options(shiny.launch.browser = FALSE)

shinyApp(
    ui = tagList(shiny.react::reactOutput("selectors"),
        shiny.react::reactOutput("selected")),
    server = function(input, output) {
        data <- mtcars

        output$selectors <- shiny.react::renderReact({
            shiny.quartz::QThemeProvider(select_ui(
                "test",
                mtcars
            ))
        })

        selectedData <- reactiveVal()

        output$selected <- shiny.react::renderReact({
            req(!is.null(selectedData()))
            shiny.quartz::QDataGrid(selectedData())
        })

        observeEvent(input$select, {
            data %>%
              select(input$test_select) %>%
              selectedData()
        })
    }

)
