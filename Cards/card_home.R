#-----------#
# Card Home #
#-----------#

cards.home <- list(
  
  card(full_screen = FALSE, fill = TRUE,
  
  layout_sidebar(open = TRUE, fillable = TRUE, 
    
    # Side bar ----
    sidebar = sidebar(open =  "always",
      value_box(title = "N. of species in our DB",
                value = 420,
                showcase = icon("bug", lib = "font-awesome"),
                theme = "text-black", # Remember: We can use the colour of the bs_theme writing for example text-fg or text-success
                style="text-align:center"),
      
      value_box(title = "N. of sequences in our DB",
                value = 11111,
                showcase = icon("dna", lib = "font-awesome"),
                theme = "text-black",
                style="text-align:center"),
      
      value_box(title = "N. of XXX in our DB",
                value = 11111,
                showcase = icon("chart-pie", lib = "font-awesome"),
                theme = "text-black",
                style="text-align:center")
    ),
    
    
    # Main content ----
    card(full_screen = FALSE, fill = FALSE,
         
         layout_columns(col_widths = c(6, 6),
                        card(full_screen = FALSE, fill = FALSE, height = "100%",
                             markdown(
                               "## CBB Taxonomy App

        The CBB Taxonomy App is a versatile web application designed to streamline
        a variety of tasks commonly associated with taxonomy and ecology.
        Thanks to this web App, you can easily compare your own taxonomy with
        those stored in several public databases (i.e., Catalogue of Life, WORMS, etc.).
        Alternatively, you can perform different explorative analyses standard in
        ecology to estimate foe example community diversity and/or composition."
                             )),
                        
                        
                        card(full_screen = FALSE, fill = FALSE, height = "100%",
                             markdown(
                               "## The Balearic Biodiversity Centre

        The Balearic Biodiversity Centre (*CBB*) is a scientific initiative that aims to generate knowledge
      about the biological diversity and the natural environment of the Balearic archipelago.
      Such information is crucial to establish management and conservation policies and to
      maintain the ecosystem services that biodiversity provides to society.
      The Balearic Biodiversity Centre is composed of a team of specialized researchers
      and technicians that can offer scientific - technical support for research groups,
      government managers, and the private sector, as well as to promote strategic collaborations,
      advice, and training on issues related to biodiversity.

      To know more about the CBB, please visit our web site [CBB](https://centrebaleardebiodiversitat.uib.eu)"
                             ))
                        ),
      
      layout_columns(col_widths = c(-4, 4, -4),
                     card_image(file = "./www/img/logo-cbb.png", fill = FALSE, width = "80%"))
      
    ),
    
    layout_columns(col_widths = c(4), 
                   card(full_screen = FALSE, fill = FALSE,
                        markdown(
                          "#### Contact
                              The ***CBB Taxonomy App*** has been created by the CBB Data Management Unit.
                              For any bug reports or feature requests, please don't hesitate to reach out to
                              [T. C.](mailto:t.cancellario@uib.eu) or [T. G.](mailto:t.cancellario@uib.eu)
                              Alternatively, you can submit a pull request directly on the project's
                              [GitHub](https://github.com/centrebalearbiodiversitat/CBB_dataAnalysis/tree/main/CBB_Shiny)"
                        )
                   ))
    
  ),
  
  card_footer(layout_columns(col_widths = c(-4, 5, 3), 
                             markdown("The creation of this application was 
                                      supported by MCIN with funding from the 
                                      European Union—NextGenerationEU (PRTR-C17.I1) 
                                      and the Government of the Balearic Islands."),
                             card_image(file = "./www/img/Logos Financiacion horizontal_V2Jul2023.png", fill = FALSE, width = "100%"))
              )
  )
  )

# cards.home
