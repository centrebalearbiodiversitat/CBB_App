#---------------------#
# START Shiny CBB App #
#---------------------#

pacman::p_load(shiny) 
runGitHub(repo = "CBB_dataAnalysis", username = "centrebalearbiodiversitat", 
          ref = "main", subdir = "CBB_Shiny")