#----------------------------------#
# WORMS taxonomic function (WORMS) #
#----------------------------------#


# Function to extract taxonomy from WORMS
extractTaxonWorms <- function(x) {
  if (is.null(x$child)) {
    return(data.frame(rank = x$rank, scientificname = x$scientificname))
  } else {
    child_df <- extractTaxonWorms(x$child)
    current_df <- data.frame(rank = x$rank, scientificname = x$scientificname)
    return(rbind(current_df, child_df))
  }
}

# x: vector of taxa

#########################################
#########################################
#########################################
#  -999 when multiple matches are found #
#########################################
#########################################
#########################################

specifyWorms <- function(x) {
  
  colNames = data.frame()
  
  withProgress(message = "Downloading taxonomy", value = 0,
               for (i in 1:length(x)) {
                 sp.1 <- x[i]
                 
                 # Json query
                 json.sp <- gsub(" ", "%20", sp.1)
                 # Search taxon Aphia
                 try(json <- fromJSON(paste0("https://www.marinespecies.org/rest/AphiaIDByName/", json.sp, "?marine_only=false")), silent = TRUE)
                 
                 if (!exists("json")) {
                   # If the taxon is not present in COL
                   colNames.1 <- data.frame(
                     originalName = sp.1,
                     colNamesAccepted = "Not found",
                     Life = "Not found",
                     Kingdom = "Not found",
                     Phylum = "Not found",
                     Class = "Not found",
                     Order = "Not found",
                     Family = "Not found",
                     Genus = "Not found",
                     Species = "Not found",
                     Subspecies = "Not found",
                     originalStatus = "Not found",
                     taxonRank = "Not Found",
                     brackish = "Not Found",
                     freshwater = "Not Found",
                     marine = "Not Found",
                     terrestrial = "Not Found"
                   )
                 } else {
                   if (json == -999) {
                     # If the taxon is not present in COL
                     colNames.1 <- data.frame(
                       originalName = sp.1,
                       colNamesAccepted = "Multiple matches",
                       Life = "Life",
                       Kingdom = "Multiple matches",
                       Phylum = "Multiple matches",
                       Class = "Multiple matches",
                       Order = "Multiple matches",
                       Family = "Multiple matches",
                       Genus = "Multiple matches",
                       Species = "Multiple matches",
                       Subspecies = "Multiple matches",
                       originalStatus = "Multiple matches",
                       taxonRank = "Multiple matches",
                       brackish = "Multiple matches",
                       freshwater = "Multiple matches",
                       marine = "Multiple matches",
                       terrestrial = "Multiple matches")
                   } else {
                     
                     classification <- extractTaxonWorms(fromJSON(paste0("https://www.marinespecies.org/rest/AphiaClassificationByAphiaID/", json)))
                     
                     sp.statusAndHabitat <- gsub(" ", "%20", classification$scientificname[nrow(classification)])
                     statusAndHabitat <- fromJSON(paste0("http://www.marinespecies.org/rest/AphiaRecordsByName/", sp.statusAndHabitat, "?like=true&marine_only=false&offset=1"))
                     
                     statusAndHabitat <- statusAndHabitat[statusAndHabitat$AphiaID == json, ]
                     
                     
                     colNames.1 <- data.frame(
                       originalName = sp.1,
                       colNamesAccepted = statusAndHabitat$scientificname[statusAndHabitat$AphiaID == json],
                       Life = "Life",
                       Kingdom = ch0_to_Na(classification$scientificname[classification$rank == "Kingdom"]),
                       Phylum = ch0_to_Na(classification$scientificname[classification$rank == "Phylum"]),
                       Class = ch0_to_Na(classification$scientificname[classification$rank == "Class"]),
                       Order = ch0_to_Na(classification$scientificname[classification$rank == "Order"]),
                       Family = ch0_to_Na(classification$scientificname[classification$rank == "Family"]),
                       Genus = ch0_to_Na(classification$scientificname[classification$rank == "Genus"]),
                       Species = ch0_to_Na(word(classification$scientificname[classification$rank == "Species"],-1)),
                       Subspecies = ch0_to_Na(word(classification$scientificname[classification$rank == "Subspecies"],-1)),
                       originalStatus = statusAndHabitat$status[statusAndHabitat$AphiaID == json],
                       taxonRank = tolower(classification$rank[nrow(classification)]),
                       brackish = statusAndHabitat$isBrackish != 0,
                       freshwater = statusAndHabitat$isFreshwater != 0,
                       marine = statusAndHabitat$isMarine != 0,
                       terrestrial = statusAndHabitat$isTerrestrial != 0) %>%
                       unique()
                     
                   }
                   
                 }
                 
                 colNames <- rbind(colNames, colNames.1)
                 
                 if (exists("json")) {rm(json)}
                 
                 # print(paste(i, "---- of ----", length(x)))
                 
                 # Increment the progress bar, and update the detail text.
                  incProgress(1/length(x), detail = paste("Doing:", i))
               }
  )
  
  return(colNames)
  
  
}