#-----------------------#
# Taxonomy sidebarPanel #
#-----------------------#

fluidRow(
  
  # Title and description ----
  column(12, 
         HTML("<h3> Taxonomy </h3>
        <br> This panel includes .... <br>")
        ),
  
  
  div(class="container-fluid", 
  
      # Import file    
      div(class = "row",
          column(12,
                 fileInput("file1", h5("Chose CSV File"),
                           accept = c(".csv"))
                 )
          ),
      
      # Taxa select column and chose DB
      div(class="row",
          
          column(width = 6, 
                 selectInput("text.db", h5("Name of taxa column"),
                             choices = NULL)
                 ),
          
          column(width = 6, 
                 selectInput("taxon.an",
                             h5("Choose taxonomy style"),
                             choices = c("Specify_COL", "CBB_DB_COL",
                                         "Specify_WORMS"))
                 )
        ),
      
      # Run button
      div(class="row",
          column(6, actionButton("taxa.run.button", HTML("<b>Run</b>"))),
          column(6, uiOutput("downloadButton"))
          )
      
      )
)