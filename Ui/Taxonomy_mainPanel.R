#--------------------#
# Taxonomy mainPanel #
#--------------------#

fluidRow(
  DT::dataTableOutput("inputDataframe"),
  br(),
  dataTableOutput("dataTaxonomy") %>% withSpinner()
  )