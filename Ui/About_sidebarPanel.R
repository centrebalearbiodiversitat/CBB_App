#--------------------#
# About sidebarPanel #
#--------------------#

fluidRow(

  div(class="container-fluid", 
      
      # Import file    
      div(class = "row",
          
       column(12,
              HTML(
                "<h1>CBB Taxonomy App</h1> 
                <br>
                
                <h3>Contact</h3>
                <br> <i><b>CBB taxonomy</b></i> 
                The <i>CBB Taxonomy App</i> is created and maintained by Tommaso Cancellario and Tomás Golomb. <br>
                <br> To comunicate bug reports and feature requests please contact: 
                <a href=\"mailto:t.cancellario@uib.eu\">T. Cancellario</a>
                or <a href=\"mailto:t.cancellario@uib.eu\"> Tomás Golomb</a>.
                
                <br> 
                <br> Or pull request the project on GitHub: 
                <a href=\"https://github.com/centrebalearbiodiversitat/CBB_dataAnalysis/tree/main/CBB_Shiny\">GitHub</a>
                <br> 
                <br>")
              )
       )
      ),
      
      # Taxa select column and chose DB
      div(class="row",
          column(12,
                 img(src = "img/logo-cbb.png", heigth = "100%", width = "80%")       
                 )
          )
  )