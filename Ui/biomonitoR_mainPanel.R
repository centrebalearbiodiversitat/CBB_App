#----------------------#
# biomonitoR mainPanel #
#----------------------#

fluidRow(
  
  div(class="container-fluid", 
      div(class = "row",
          # Show input table
          DT::dataTableOutput("inputDataframe_bioR")
          ),
      
      div(class = "row",
          # Show general info of table
          DT::dataTableOutput("generalInfo_bioR")
          ),
      
      div(class = "row",
          # Show table with biological indices
          conditionalPanel(
            condition = "input['div.run.button.bior'] % 2 == 1",
            DT::dataTableOutput("modify_index_bioR"))
          ),
      
      div(class = "row",
          # Show table with community composition
          conditionalPanel(
            condition = "input['com.run.button.bior'] % 2 == 1",
            DT::dataTableOutput("modify_rich_bioR"))
          ),
      
      div(class = "row",
          # Show plot with NMDS results
          conditionalPanel(
            condition = "input['nmds.run.button.bior'] % 2 == 1",
            plotOutput("nmdsPlot"))
          ),
      
      div(class = "row",
          # Show plot with accumulation curves 
          conditionalPanel(
            condition = "input['acc.run.button.bior'] % 2 == 1",
            plotOutput("accPlot"))
          ),
      
      div(class = "row",
          
          # Show plot with rarefaction curves
          conditionalPanel(
            condition = "input['rar.run.button.bior'] % 2 == 1",
            plotOutput("rarPlot"))
          )
      )
  )