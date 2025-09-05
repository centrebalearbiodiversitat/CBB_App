#---------------#
# Card Taxonomy #
#---------------#


cards.taxonomy <- list(# Cards for taxonomy
  card(
    full_screen = FALSE,

    
    # Sidebar ----
    layout_sidebar(
      open = TRUE,
      # Open sidebar
      fillable = TRUE,
      
      # Side bar
      sidebar = sidebar(
        width = 400,
        # sidebar width
        
        # Input file
        fileInput("file1",
                  label = tags$span(
                    style = "font-size: 1.25em;font-weight: bold;",
                    "Chose CSV File"),
                  accept = c(".csv")),
        
        # Select taxa column where we want to perform the taxonomic check 
        selectInput("text.db",
                    label = tags$span(
                      style = "font-size: 1.25em;font-weight: bold;",
                      "Taxa column",
                      infoIconTooltip(
                        tooltipText = "Select the column name from your csv that contain the list of taxonomic levels (species name, genus name, family name...)."
                        )
                      ),
                    choices = NULL
                    ),
        
        # Select database to perform the taxonomy check
        selectInput(
          "taxon.an",
          label = tags$span(
            style = "font-size: 1.25em;font-weight: bold;",
            "Choose taxonomy style ",
            infoIconTooltip( 
              tooltipText = "<b>Taxonomy COL</b>: Download extenden information of a given taxonomic level, including authors, source, origin, taxonomic level status, and environmental infromation from Catalogue of Life.<br><br>
                     <b>Specify COL</b>: Download simplified taxonomic information of a given taxonomic level from Catalogue of Life.<br><br>
                     <b>Specify WoRMS</b>: Download taxonomic and environmental information of a given taxonomic level from World Register of Marine Species."
            )
          ),
          choices = c(
            "Taxonomy COL" = "CBB_DB_COL",
            "Specify COL" = "Specify_COL",
            "Specify WoRMS" = "Specify_WORMS"
          )
        ),
        
        numericInput(
          "dataset_number",
          label = tags$span(
            style = "font-size: 1.25em;font-weight: bold;",
            "COL ",
            tags$a(
              href = "https://www.checklistbank.org/",
              target = "_blank",
              style = "color: #00acba;",
              "ChecklistBank"
            ),
            "dataset key",
            infoIconTooltip(
              tooltipText = "Use only when <b>Taxonomy COL</b> or <b>Specify COL</b> download is performed.<br><br>
                     The dataset key is a unique identifier assigned to a COL checklist or dataset within ChecklistBank.
                     It is used to reference, access, and download data <b>from the specified version of a COL dataset</b>."
            )
          ),
          value = 312092, #dataset number
          min = 1
        ),

tags$head(tags$style(
  HTML("#dataset_number
                                     {color: grey;  /* default grey */}")
),
# JavaScript to change color when user types
tags$script(
  HTML(
    "$(document).on('input', '#dataset_number', function() {
      if ($(this).val() == '312092' || $(this).val() == '') {
        $(this).css('color', 'grey');
      } else {
        $(this).css('color', 'black');
      }});"
  )
)),


# Buttons
layout_columns(
  col_widths = c(4, 4),
  
  # Button to run the taxonomic check
  actionButton("taxa.run.button", "Run",
               width = "100%", 
               style = "
               display: flex;
               align-items: center;     /* vertical centering */
               justify-content: center; /* horizontal centering */
               font-size: 100%;
               font-weight: bold;
               height: 63px;
               "
               ),
  
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
uiOutput("uiIucnCat"),

# Add ambiguous taxa selection UI
uiOutput("choose_ids_ui"),

  )))


# cards.taxonomy[[1]]