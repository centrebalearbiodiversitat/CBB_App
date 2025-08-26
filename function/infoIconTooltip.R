# Function to create an info icon with a justified tooltip

infoIconTooltip <- function(tooltipText, color = "#be4358", width = 300) {
  tagList(
    tags$head(tags$style(HTML(paste0(
      ".tooltip-inner { 
         text-align: justify; 
         max-width: ", width, "px; 
         white-space: normal; 
       }"
    )))),
    tags$sup(
      icon(
        "info-circle",
        style = paste0("color: ", color, ";"),
        `data-toggle` = "tooltip",
        `data-html` = "true",      
        title = tooltipText
      )
    ),
    tags$script(HTML("
      $(function () {
        $('[data-toggle=\"tooltip\"]').tooltip({html: true});
      });
    "))
  )
}
