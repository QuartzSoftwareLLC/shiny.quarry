renderDataGrid <- function(data) {
    shiny.react::renderReact({
        if (!is.null(data()) & nrow(data()) > 0) {
            shiny.quartz::QDataGrid(data())
        } else {
            div("No Data")
        }
    })
}
