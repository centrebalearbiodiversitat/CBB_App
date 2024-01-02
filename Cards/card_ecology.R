#--------------#
# Card Ecology #
#--------------#

# OTUs data: https://www.frontiersin.org/articles/10.3389/fmicb.2020.592189/full


# Drop-down menu
accordion_ecology <- accordion(open = FALSE,
  
  # Biomonitor ----                         
  accordion_panel("Diveristy indices", # icon = bsicons::bs_icon("menu-app"), 
                  
                  layout_columns(col_widths = c(12, 12, 6, 6, 4, 4),
                  # 1. Description of the sub panel
                         markdown("A diversity index is a quantitative measure that 
                                  reflects how many different types (e.g., species) 
                                  there are in a dataset (e.g., community).
                                  Diversity indices are statistical representations of 
                                  biodiversity describing different aspects of a particular 
                                  community (e.g., richness, evenness, and dominance). 
                                  Diversity indices can be used with different taxonomic 
                                  levels or categories such as genera, families, functional 
                                  types, or haplotypes."),
                  
                  # 2. Reference dataset
                         selectInput("refdb.group", markdown("##### Taxonomy ref. group"),
                                     choices = c("Macroinvertebrate" = "mi",
                                                 "Macrophytes" = "mf",
                                                 "Fish" = "fi",
                                                 "Diatoms." = "di"),
                                     selected = "mi"),
                  
                  # 4. Calculate diversity indices
                  actionButton("div.run.button.bior", markdown("Div. Index")),
                  
                  # 5. Calculate community composition
                  actionButton("com.run.button.bior", markdown("Com. Compo")),               
                  
                  # 6. Download diversity index results
                  conditionalPanel(condition = "input['div.run.button.bior'] % 2 == 1",
                                   uiOutput("dowButtonBiorIndex")),
                  
                  # 7. Download community composition results
                  conditionalPanel(condition = "input['com.run.button.bior'] % 2 == 1",
                                   uiOutput("dowButtonBiorComp"))
                  
                  ),
                          
  ),
  
  # NMDS ----
  accordion_panel("NMDS", #icon = bsicons::bs_icon("sliders"),
                  layout_columns(col_widths = c(12, 12, 4, 4, 4, 12, 12, 4, 6),
                                 
                                 # 1. Description of the sub panel
                                 markdown("Non-metric multidimensional scaling (NMDS) 
                                          is a statistical ordination technique useful 
                                          to represent the position of objects (e.g., communities) 
                                          in multidimensional space as accurately as
                                          possible using a reduced number of dimensions 
                                          that can be easily visualized. NMDS is iterative 
                                          and non-parametric, and it can use any 
                                          distance measure suitable for the data, avoiding 
                                          assumptions about the data linear relationship."),
                                 
                                 # 2. Load environmental file is needed
                                 fileInput("envNMDS", markdown("#### Chose environmental .csv"),
                                           accept = c(".csv")), 
                                 
                                 # 3. Number of NMDS dimension
                                 numericInput("nmdsk", "N. dimension (k)", value = 2),
                                 
                                 # 4. Minimum number of random starts in search of stable solution.
                                 numericInput("nmdsTry", "N. random (min)", value = 20),
                                 
                                 # 5. Maximum number of random starts in search of stable solution. 
                                 numericInput("nmdsTrymax", "N. random (max)", value = 50),
                                 
                                 # 6. ?
                                 selectInput("nmdsGroup", markdown("#### Environmental group"),
                                             choices = NULL), 
                                 
                                 # 7. Type of plot for NMDS
                                 selectInput("nmdsPlotType", markdown("#### NMDS Plot type"),
                                             choices = c("General",
                                                         "Ellipse"),
                                             selected = "General"),
                                 
                                 # 8. Run NMDS analysis                                                     
                                 actionButton("nmds.run.button.bior", markdown("NMDS"),
                                              style = "font-size:100%"),
                                 
                                 # 9. Download NMDS results
                                 conditionalPanel(condition = "input['nmds.run.button.bior'] % 2 == 1",
                                 uiOutput("dowButtonNMDS"))
                                 )
                  ),
  
  # Accumulation curves ----
  accordion_panel("Accumulation curves", #icon = bsicons::bs_icon("sliders"),
                  layout_columns(col_widths = c(12, 8),
                                 
                                 # 1. Description of the sub panel
                                 markdown("Species accumulation curves represent the 
                                          rate at which new species can be found within a community. 
                                          These curves can be extrapolated to provide an estimation of 
                                          species richness. The order in which samples are included in
                                          a species accumulation curve will influence the overall shape."),
                                 
                                 # 2. method
                                 selectInput("accMethod", markdown("#### Method"),
                                             choices = c("Collector" = "collector",
                                                         "Random" = "random",
                                                         "Exact" = "exact",
                                                         "Coleman" = "coleman",
                                                         "Rarefaction" = "rarefaction"),
                                             selected = "random"),
                                 
                                 # 2. Run button for Accumulation curves
                                 actionButton("acc.run.button.bior", markdown("Acc. Curves"))
                                 )
                  ),
  
  # Rarefaction curves ----
  accordion_panel("Rarefaction curves", #icon = bsicons::bs_icon("sliders"),
                  layout_columns(col_widths = c(12, 8),
                                 
                                 # 1. Description of the sub panel
                                 markdown("Rarefaction is a technique to assess species richness 
                                          from the results of sampling. 
                                          Rarefaction is displayed through the construction 
                                          of the rarefaction curves. These curves generally 
                                          grow rapidly at first, as the most common species 
                                          are found, until a plateau indicating that only the 
                                          rarest species remain to be sampled."),
                                 
                                 # 2. Run button for Rarefaction curves
                                 actionButton("rar.run.button.bior", markdown("Rar. Curves"))
                                 )
                  )
  )




cards.ecology <- list(
  
  # Cards for taxonomy
  card(full_screen = FALSE,
       card_header("Ecology"),
       
       # Sidebar ----
       layout_sidebar(
         open = TRUE, # Open sidebar
         fillable = TRUE,
         
         # Side bar
         sidebar = sidebar(width = 400, # sidebar width
           
           # Import file    
           layout_columns(col_widths = c(12, 12, 12),
          
                          # Load biomonitoR file
                          fileInput("fileBr_taxa", markdown("##### Chose TAXA .csv"),
                                    accept = c(".csv")),
                          
                          # Load custom dataset
                          fileInput("fileBr_refdb", markdown("##### Chose Reference DB .csv file"),
                                    accept = c(".csv")),
                          
                          actionButton("div.run.button.biorPrep", markdown("Prepare to biomonitoR"))
                          
                          ),
           
           # Drop-down menu
           accordion_ecology,
         
           ),
         
         # Main content ----
         
         uiOutput("uiBiorTable"),
         
         uiOutput("uiBiorInfo"),
         
         conditionalPanel(
           condition = "input['div.run.button.bior'] % 2 == 1",
           uiOutput("uiBiorIndex")),
         
         conditionalPanel(
           condition = "input['com.run.button.bior'] % 2 == 1",
           uiOutput("uiBiorRich")),
         
         conditionalPanel(
           condition = "input['nmds.run.button.bior'] % 2 == 1",
           uiOutput("uiBiorNmds")),
         
         conditionalPanel(
           condition = "input['acc.run.button.bior'] % 2 == 1",
           uiOutput("uiBiorAcc")),
         
         conditionalPanel(
           condition = "input['rar.run.button.bior'] % 2 == 1",
           uiOutput("uiBiorRar"))
         )
       )
  )

# cards.ecology[[1]]