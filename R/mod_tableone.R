options(shiny.launch.browser = FALSE)

factor_convert <- function(x, len){
  
  val <- length(unique(x))
  
  if (val < len) {
    x <- factor(x)
  }
  return(x)
}

shinyApp(
    ui = shiny.quartz::VStack(
        shiny.react::reactOutput("controls"),
        tableOutput("tableone")
        ),
    server = function(input, output) {
        data <- mtcars %>%
                    dplyr::mutate_all(factor_convert, len = 5) %>%
                    tibble()

        output$controls <- shiny.react::renderReact({
            shiny.quartz::QThemeProvider(
                shiny.quartz::HStack(
                    shiny.mui::Autocomplete.shinyInput(
                        "columns",
                        multiple = T,
                        fullWidth = T,
                        value = list(),
                        inputProps = list(label = "Columns"),
                        options = colnames(data) %>% .[sapply(data, is.factor)]
                    ),
                    shiny.mui::Autocomplete.shinyInput(
                        "rows",
                        multiple = T,
                        fullWidth = T,
                        value = list(),
                        inputProps = list(label = "Rows"),
                        options = colnames(data)
                    )
                )
            )
        })

        

        output$tableone <- renderTable({
            table1::table1( ~ mpg + cyl | am, data = data)
        })
    }
)
