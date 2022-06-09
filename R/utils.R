data(mtcars)
renderDataGrid <- function(data) {
    shiny.react::renderReact({
        if (!is.null(data())) {
            shiny.quartz::QDataGrid(data())
        } else {
            div()
        }
    })
}
