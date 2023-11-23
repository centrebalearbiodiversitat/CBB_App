#----------------#
# Maps mainPanel #
#----------------#

dashboardBody(
  
  fluidRow(
    
  h2("CBB Maps"),
  leaflet::leafletOutput("myMap"),
  br(),
  verbatimTextOutput(outputId = "mapStatistics"),
  br(),
  verbatimTextOutput(outputId = "mapAOO"),
  br(),
  verbatimTextOutput(outputId = "mapEOO")
  
  )

)
