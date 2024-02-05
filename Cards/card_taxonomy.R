#---------------#
# Card Taxonomy #
#---------------#

cards.taxonomy <- list(
    
  # Cards for taxonomy
  card(full_screen = FALSE,
       card_header("Taxonomy"),
       
       # Sidebar ----
       layout_sidebar(
         open = TRUE, # Open sidebar
         fillable = TRUE,
         
         # Side bar
         sidebar = sidebar(width = 400, # sidebar width
           
           # Input file 
           fileInput("file1", 
                     markdown("##### Chose CSV File"),
                     accept = c(".csv")),
           
           # Select taxa column where we want to perform the taxonomic check
           selectInput("text.db", 
                      markdown("##### Name of taxa column"),
                       choices = NULL),
           
           # Select database to perform the taxonomy check
           selectInput("taxon.an",
                       markdown("##### Choose taxonomy style"),
                       choices = c("CBB DB COL" = "CBB_DB_COL",
                                   "Specify COL" = "Specify_COL",
                                   "Specify WORMS" = "Specify_WORMS")),
           
           # Buttons
           layout_columns(col_widths = c(4, 4),
                           
                           # Button to run the taxonomic check
                           actionButton("taxa.run.button", markdown("Run")),
                           
                           # Download button
                           uiOutput("downloadButton")
                           )
           ),
         
         
       # Main content ----
         
       # Table contains input taxonomy
       uiOutput("uiTaxonomy"),  
       
       # Table contains reviewed taxonomy
       uiOutput("uiRevTaxonomy"),
       
       # Table contains IUCN category
       uiOutput("uiIucnCat")
       
       )
       )
)
  


# cards.taxonomy[[1]]