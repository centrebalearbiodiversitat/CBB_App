#-------------------------------#
# Shiny App CBB Taxonomy v. 0.1 #
#-------------------------------#

server <- function(input, output, session) {
  # Taxonomy -------------------------------------------------------------------
  source("./Server/01_Taxonomy_v2.R", local = T)
  
  # Maps -----------------------------------------------------------------------
  source("./Server/02_Maps_v2.R", local = T)
  
  # biomonitoR -----------------------------------------------------------------
  source("./Server/03_biomonitoR_v2.R", local = T)
}




# server <- function(input, output, session) {
#   
#   # Taxonomy -------------------------------------------------------------------
#   source("./Server/01_Taxonomy.R", local = T)
#   
#   # Maps -----------------------------------------------------------------------
#   source("./Server/02_Maps.R", local = T)
#   
#   # biomonitoR -----------------------------------------------------------------
#   source("./Server/03_biomonitoR.R", local = T)
#   
#   
#   }