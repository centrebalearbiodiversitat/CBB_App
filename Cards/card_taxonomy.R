#---------------#
# Card Taxonomy #
#---------------#

cards.taxonomy <- list(
  # Cards for taxonomy
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
        fileInput(
          "file1",
          label = tags$span(
            style = "font-size: 1.25em;font-weight: bold;",
            "Chose CSV File"
          ), accept = c(".csv")
        ),

        # Select taxa column where we want to perform the taxonomic check
        selectInput(
          "text.db",
          label = tags$span(
            style = "font-size: 1.25em;font-weight: bold;",
            "Taxa column",
            infoIconTooltip(tooltipText = "Select the column name from your csv that contain the list of taxonomic levels (species name, genus name, family name...).")
          ),
          choices = NULL
        ),

        # Select database to perform the taxonomy check
        # selectInput(
        #   "taxon.an",
        #   label = tags$span(
        #     style = "font-size: 1.25em;font-weight: bold;",
        #     "Choose taxonomy style ",
        #     infoIconTooltip(
        #       tooltipText = "<b>Specify COL</b>: Download simplified taxonomic information of a given taxonomic level from Catalogue of Life.<br><br>"
        #     )
        #   ),
        #   choices = c(
        #     "Specify COL" = "Specify_COL"
        #   )
        # ),
        numericInput(
          "dataset_number",
          label = tags$span(
            style = "font-size: 1.25em;font-weight: bold;",
            "COL ",
            tags$a(
              href = "https://www.catalogueoflife.org/",
              target = "_blank",
              style = "color: #00acba;",
              "ChecklistBank"
            ),
            "dataset key",
            infoIconTooltip(
              tooltipText = "The dataset key is a unique identifier assigned to a COL checklist or dataset in ChecklistBank. 
                             You can find the ChecklistBank number at the end of the COL webpage."
            )
          ),
          value = NULL
        ),

        # Buttons
        layout_columns(
          col_widths = c(4, 4),

          # Button to run the taxonomic check
          actionButton(
            "taxa.run.button",
            "Run",
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

      # Add ambiguous taxa selection UI
      uiOutput("choose_ids_ui")
    )
  )
)

# cards.taxonomy[[1]]