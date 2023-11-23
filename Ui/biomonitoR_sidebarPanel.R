#-------------------------#
# biomonitoR sidebarPanel #
#-------------------------#
# Add api to download IUCN Information

fluidRow(
  
  column(12, 
         # Title and description
         HTML("<h3> biomonitoR </h3>
              <br> biomonitoR .... <br>")
         ),
  
  
  # Import file    
  div(class="container-fluid", 
      div(class = "row",
          column(12,
                 fileInput("fileBr_taxa", h5("Chose TAXA .csv"),
                           accept = c(".csv"))
                 )
          )
      ),

# BiomonitoR ----
box(title = "biomonitoR",  width = NULL, solidHeader = TRUE, collapsible = TRUE, 
    collapsed = TRUE,
  
    div(class="container-fluid", 
        
        div(class = "row",
           
            # Description of the sub panel
             column(12,
                   HTML(
                     "<h4>This panel allows to visualize the relationship between the index 
                     results at different taxonomic levels (users need to select one index and 
                     two taxonomic levels). This can be useful when deciding which taxonomic 
                     resolution to use.</h4>")
                   ),
            
            # Reference dataset
            column(6, 
                   selectInput("refdb.group", h5("Taxonomy ref. group"), 
                               choices = c("Macroinvertebrate" = "mi", 
                                           "Macrophytes" = "mf",
                                           "Fish" = "fi",
                                           "Diatoms." = "di"),
                               selected = "mi")),
            
            # Load custom dataset
            column(6, 
                   fileInput("fileBr_refdb", h5("Chose Reference DB .csv file"),
                             accept = c(".csv"))),
            
            # Calculate diversity indices
            column(3,
                   actionButton("div.run.button.bior", HTML("<b>Div. index</b>"))
                   ),
            
            # Calculate community composition
            column(3,
                   actionButton("com.run.button.bior", HTML("<b>Com. compo</b>"))
                   ),
            
            # Download diversity index results
            column(3,
                   conditionalPanel(
                     condition = "input['div.run.button.bior'] % 2 == 1",
                     uiOutput("dowButtonBiorIndex"))
                   ),
            
            # Download community composition results
            column(3,
                   conditionalPanel(
                     condition = "input['com.run.button.bior'] % 2 == 1",
                     uiOutput("dowButtonBiorComp"))
                   )
            )
        )
    ),


# NMDS ----
box(title = "NMDS",  width = NULL, solidHeader = FALSE, collapsible = TRUE, 
    collapsed = TRUE,
    
    div(class="container-fluid", 
        
        div(class = "row",
            
            # Description of the sub panel
            column(12,
                   HTML(
                     "<h4>This panel allows to visualize the relationship between the index 
                     results at different taxonomic levels (users need to select one index and 
                     two taxonomic levels). This can be useful when deciding which taxonomic 
                     resolution to use.</h4>")
                   )
            
            ),
        
        # Load environmental file is needed
        div(class = "row",
            column(12,
                   fileInput("envNMDS", h5("Chose environmental .csv"),
                             accept = c(".csv"))
                   )
            ),
        
        div(class = "row",
            
            # Number of NMDS dimension
            column(4,
                   numericInput("nmdsk", "N. dimension (k)", value = 2)
                   ),
            
            # Minimum number of random starts in search of stable solution. 
            column(4,
                   numericInput("nmdsTry", "N. random (min)", value = 20)
                   ),
            
            # Maximum number of random starts in search of stable solution. 
            column(4,
                   numericInput("nmdsTrymax", "N. random (max)", value = 50)
                   )
            ),
        
        div(class = "row",
            #?
            column(12,
                   selectInput("nmdsGroup", h5("Environmental group"), 
                               choices = NULL)
                   ),
            
            # Type of plot for NMDS 
            column(6,
                   selectInput("nmdsPlotType", h5("NMDS Plot type"), 
                               choices = c("General",
                                           "Ellipse"),
                               selected = "General")
                   )
            ),
        
        div(class = "row",
            
            # Run NMDS analysis
            column(3,
                   actionButton("nmds.run.button.bior", HTML("<b>NMDS</b>"))
                   ),
            
            # Download NMDS results
            column(3,
                   condition = "input['nmds.run.button.bior'] % 2 == 1",
                   uiOutput("dowButtonNMDS")
                   )
            )
        )
    ),

# Accumulation curves ----
box(title = "Accumulation curves",  width = NULL, solidHeader = TRUE, collapsible = TRUE, 
    collapsed = TRUE,
    
    div(class="container-fluid", 
        
        div(class = "row",
            
            # Description of the sub panel
            column(12,
                   HTML(
                     "<h4>This panel allows to visualize the relationship between the index 
                     results at different taxonomic levels (users need to select one index and 
                     two taxonomic levels). This can be useful when deciding which taxonomic 
                     resolution to use.</h4>")
                   )
            ),
        
        div(class = "row", 
            
            column(3,
                   # Run button for Accumulation curves
                   actionButton("acc.run.button.bior", HTML("<b>Acc. Curves</b>"))
                   )
            )
        )
    ),

# Rarefaction curves ----
box(title = "Rarefaction curves",  width = NULL, solidHeader = TRUE, collapsible = TRUE, 
    collapsed = TRUE,

    div(class="container-fluid", 
        
        div(class = "row",
            
            # Description of the sub panel
            column(12,
                   HTML(
                     "<h4>This panel allows to visualize the relationship between the index 
                     results at different taxonomic levels (users need to select one index and 
                     two taxonomic levels). This can be useful when deciding which taxonomic 
                     resolution to use.</h4>")
            )
        ),
        
        div(class = "row", 
            # Run button for Rarefaction curves
            column(3,
                   actionButton("rar.run.button.bior", HTML("<b>Rar. Curves</b>"))
                   )
            )
        )
    )
)