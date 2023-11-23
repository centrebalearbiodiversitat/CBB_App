# X is the vector containing the species to search.

# x <- fread("~/OneDrive - Universitat de les Illes Balears/CBB objectives/CBB_DB/Taxonomy/ToCheck/thTaxa_2023_11_16.csv")
# x <- x$originalName
# 
# i=93


cbbdbCol <- function(x){
  
  colNames <- data.frame()
  
  withProgress(message = "Downloading taxonomy", value = 0,
  for(i in 1:length(x)){
    
    # Taxonomy source
    taxonSource <- "Catalogue of Life"
    
    # Taxon origin
    taxonOrigin <- "database"
    
    # Species i of the list
    sp.1 <- x[i]
    
    # Json query
    json.sp <- gsub(" ", "%20", sp.1)
    json <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/nameusage/search?content=SCIENTIFIC_NAME&q=", json.sp, "&type=EXACT&offset=0&limit=50"))
    
    
    # Species not found into COL database
    if(isTRUE(json$empty)){
      
      colNames.1 <- data.frame(originalName = sp.1,
                               colNamesAccepted = "Not found",
                               colID = "Not found",
                               Kingdom = "Not found",
                               kingdomAuthor = "Not found",
                               kingdomSource = "Not found",
                               kingdomOrigin = "Not found",
                               Phylum = "Not found",
                               phylumAuthor = "Not found",
                               phylumSource = "Not found",
                               phylumOrigin = "Not found",
                               Class = "Not found",
                               classAuthor = "Not found",
                               classSource = "Not found",
                               classOrigin = "Not found",
                               Order = "Not found",
                               orderAuthor = "Not found",
                               orderSource = "Not found",
                               orderOrigin = "Not found",
                               Family = "Not found",
                               familyAuthor = "Not found",
                               familySource = "Not found",
                               familyOrigin = "Not found",
                               Genus = "Not found",
                               genusAuthor = "Not found",
                               genusSource = "Not found",
                               genusOrigin = "Not found",
                               Species = "Not found",
                               speciesAuthor = "Not found",
                               speciesSource = "Not found",
                               speciesOrigin = "Not found",
                               Subspecies = "Not found",
                               subspeciesAuthor = "Not found",
                               subspeciesSource = "Not found",
                               subspeciesOrigin = "Not found",
                               originalStatus = "Not found",
                               taxonRank = "Not Found",
                               brackish = "Not Found",
                               freshwater = "Not Found",
                               marine = "Not Found",
                               terrestrial = "Not Found")
    } else {
      
      # Check name status
      status <- json$result$usage$status
      
      # Named with more taxonomic status
      if(length(status) > 1) {
        
        acc <- grepl("accepted", status)
        
        if (all(unique(acc)) | length(which(acc == "TRUE")) > 1 | length(which(acc == "FALSE")) > 1) {
          
          showNotification(paste("The taxon", sp.1, "has more then one accepted name"),
                           type = "error",
                           duration = NULL)
          break
        }
        
        classification <- as.data.frame(json$result$classification[which(json$result$usage$status == status[acc])])
        rank <- classification$rank[nrow(classification)]
        
        classificationID <- classification$id[classification$rank == rank]
        
        # Classification rank into the list 
        # Api COL: https://api.checklistbank.org/dataset/9923/taxon/8TN37
        classificationLower <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/taxon/", classificationID))
        
        taxonLower <- ch0_to_Na(classificationLower$name$scientificName) 
        authorLower <- ch0_to_Na(classificationLower$name$authorship)
        
        # Habitat
        # habitat <- ch0_to_Na(classificationLower$environments)
        
        # Higher classification compared to the rank into the list 
        # Api COL: https://api.checklistbank.org/dataset/9923/taxon/8TN37/classification
        classificationHigher <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/taxon/", classificationID, "/classification"))
        
        # Taxon classification
        taxonHigherKingdom <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "kingdom"])
        authorHigherKingdom <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "kingdom"])
        taxonHigherPhylum <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "phylum"])
        authorHigherPhylum <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "phylum"])
        taxonHigherClass <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "class"])
        authorHigherClass <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "class"])
        taxonHigherOrder <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "order"])
        authorHigherOrder <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "order"])
        taxonHigherFamily <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "family"])
        authorHigherFamily <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "family"])
        taxonHigherGenus <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "genus"])
        authorHigherGenus <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "genus"])
        taxonHigherSpecies <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "species"])
        authorHigherSpecies <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "species"])
        taxonHigherSubspecies <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "subspecies"])
        authorHigherSubspecies <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "subspecies"])
        
        
        # Dataframe to add to the main one
        colNames.1 <- data.frame(originalName = sp.1,
                                 colNamesAccepted = classification$name[classification$rank == rank],
                                 colID = classificationID,
                                 Kingdom = ifelse(rank == "kingdom", taxonLower, taxonHigherKingdom),
                                 kingdomAuthor = ifelse(rank == "kingdom", authorLower, authorHigherKingdom),
                                 kingdomSource = "",
                                 kingdomOrigin = "",
                                 Phylum = ifelse(rank == "phylum", taxonLower, taxonHigherPhylum),
                                 phylumAuthor = ifelse(rank == "phylum", authorLower, authorHigherPhylum),
                                 phylumSource = "",
                                 phylumOrigin = "",
                                 Class = ifelse(rank == "class", taxonLower, taxonHigherClass),
                                 classAuthor = ifelse(rank == "class", authorLower, authorHigherClass),
                                 classSource = "",
                                 classOrigin = "",
                                 Order = ifelse(rank == "order", taxonLower, taxonHigherOrder),
                                 orderAuthor = ifelse(rank == "order", authorLower, authorHigherOrder),
                                 orderSource = "",
                                 orderOrigin = "",
                                 Family = ifelse(rank == "family", taxonLower, taxonHigherFamily),
                                 familyAuthor = ifelse(rank == "family", authorLower, authorHigherFamily),
                                 familySource = "",
                                 familyOrigin = "",
                                 Genus = ifelse(rank == "genus", taxonLower, taxonHigherGenus),
                                 genusAuthor = ifelse(rank == "genus", authorLower, authorHigherGenus),
                                 genusSource = "",
                                 genusOrigin = "",
                                 Species = ifelse(rank == "species", word(taxonLower, -1), word(taxonHigherSpecies, -1)),
                                 speciesAuthor = ifelse(rank == "species", authorLower, authorHigherSpecies),
                                 speciesSource = "",
                                 speciesOrigin = "",
                                 Subspecies = ifelse(rank == "subspecies", word(taxonLower, -1), word(taxonHigherSubspecies, -1)),
                                 subspeciesAuthor = ifelse(rank == "subspecies", authorLower, authorHigherSubspecies),
                                 subspeciesSource = "",
                                 subspeciesOrigin = "",
                                 originalStatus = json$result$usage[which(json$result$usage$status == status[acc]), ]$status, #ifelse(any(status %in% "accepted"), "accepted", "Many status"),
                                 taxonRank = rank,
                                 brackish = "brackish" %in% classificationLower$environments,
                                 freshwater = "freshwater" %in% classificationLower$environments,
                                 marine = "marine" %in% classificationLower$environments,
                                 terrestrial = "terrestrial" %in% classificationLower$environments) %>% 
          unique()
        
        colNames.1$kingdomSource <- rm_origin(colNames.1$Kingdom, taxonSource)
        colNames.1$kingdomOrigin <- rm_origin(colNames.1$Kingdom, taxonOrigin)
        colNames.1$phylumSource <- rm_origin(colNames.1$Phylum, taxonSource)
        colNames.1$phylumOrigin <- rm_origin(colNames.1$Phylum, taxonOrigin)
        colNames.1$classSource <- rm_origin(colNames.1$Class, taxonSource)
        colNames.1$classOrigin <- rm_origin(colNames.1$Class, taxonOrigin)
        colNames.1$orderSource <- rm_origin(colNames.1$Order, taxonSource)
        colNames.1$orderOrigin <- rm_origin(colNames.1$Order, taxonOrigin)
        colNames.1$familySource <- rm_origin(colNames.1$Family, taxonSource)
        colNames.1$familyOrigin <- rm_origin(colNames.1$Family, taxonOrigin)
        colNames.1$genusSource <- rm_origin(colNames.1$Genus, taxonSource)
        colNames.1$genusOrigin <- rm_origin(colNames.1$Genus, taxonOrigin)
        colNames.1$speciesSource <- rm_origin(colNames.1$Species, taxonSource)
        colNames.1$speciesOrigin <- rm_origin(colNames.1$Species, taxonOrigin)
        colNames.1$subspeciesSource <- rm_origin(colNames.1$Subspecies, taxonSource)
        colNames.1$subspeciesOrigin <- rm_origin(colNames.1$Subspecies, taxonOrigin)
        # any(): check if there are TRUE values in a string
      }
      
      # Accepted names
      if(length(status) == 1 && status == "accepted"){
        
        classification <- as.data.frame(json$result$classification)
        rank <- classification$rank[nrow(classification)]
        
        # Lower classification ID
        classificationID <- classification$id[classification$rank == rank]
        
        # Classification rank into the list 
        # Api COL: https://api.checklistbank.org/dataset/9923/taxon/8TN37
        classificationLower <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/taxon/", classificationID))
        
        taxonLower <- ch0_to_Na(classificationLower$name$scientificName) 
        authorLower <- ch0_to_Na(classificationLower$name$authorship)
        
        # Habitat
        # habitat <- ch0_to_Na(classificationLower$environments)

        # Higher classification compared to the rank into the list 
        # Api COL: https://api.checklistbank.org/dataset/9923/taxon/8TN37/classification
        classificationHigher <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/taxon/", classificationID, "/classification"))
        
        # Taxon classification
        taxonHigherKingdom <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "kingdom"])
        authorHigherKingdom <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "kingdom"])
        taxonHigherPhylum <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "phylum"])
        authorHigherPhylum <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "phylum"])
        taxonHigherClass <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "class"])
        authorHigherClass <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "class"])
        taxonHigherOrder <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "order"])
        authorHigherOrder <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "order"])
        taxonHigherFamily <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "family"])
        authorHigherFamily <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "family"])
        taxonHigherGenus <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "genus"])
        authorHigherGenus <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "genus"])
        taxonHigherSpecies <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "species"])
        authorHigherSpecies <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "species"])
        taxonHigherSubspecies <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "subspecies"])
        authorHigherSubspecies <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "subspecies"])
        
        
        # Dataframe to add to the main one
        colNames.1 <- data.frame(originalName = sp.1,
                                 colNamesAccepted = classification$name[classification$rank == rank],
                                 colID = json$result$id,
                                 Kingdom = ifelse(rank == "kingdom", taxonLower, taxonHigherKingdom),
                                 kingdomAuthor = ifelse(rank == "kingdom", authorLower, authorHigherKingdom),
                                 kingdomSource = "",
                                 kingdomOrigin = "",
                                 Phylum = ifelse(rank == "phylum", taxonLower, taxonHigherPhylum),
                                 phylumAuthor = ifelse(rank == "phylum", authorLower, authorHigherPhylum),
                                 phylumSource = "",
                                 phylumOrigin = "",
                                 Class = ifelse(rank == "class", taxonLower, taxonHigherClass),
                                 classAuthor = ifelse(rank == "class", authorLower, authorHigherClass),
                                 classSource = "",
                                 classOrigin = "",
                                 Order = ifelse(rank == "order", taxonLower, taxonHigherOrder),
                                 orderAuthor = ifelse(rank == "order", authorLower, authorHigherOrder),
                                 orderSource = "",
                                 orderOrigin = "",
                                 Family = ifelse(rank == "family", taxonLower, taxonHigherFamily),
                                 familyAuthor = ifelse(rank == "family", authorLower, authorHigherFamily),
                                 familySource = "",
                                 familyOrigin = "",
                                 Genus = ifelse(rank == "genus", taxonLower, taxonHigherGenus),
                                 genusAuthor = ifelse(rank == "genus", authorLower, authorHigherGenus),
                                 genusSource = "",
                                 genusOrigin = "",
                                 Species = ifelse(rank == "species", word(taxonLower, -1), word(taxonHigherSpecies)),
                                 speciesAuthor = ifelse(rank == "species", authorLower, authorHigherSpecies),
                                 speciesSource = "",
                                 speciesOrigin = "",
                                 Subspecies = ifelse(rank == "subspecies", word(taxonLower, -1), word(taxonHigherSubspecies, -1)),
                                 subspeciesAuthor = ifelse(rank == "subspecies", authorLower, authorHigherSubspecies),
                                 subspeciesSource = "",
                                 subspeciesOrigin = "",
                                 originalStatus = status,
                                 taxonRank = rank,
                                 brackish = "brackish" %in% classificationLower$environments,
                                 freshwater = "freshwater" %in% classificationLower$environments,
                                 marine = "marine" %in% classificationLower$environments,
                                 terrestrial = "terrestrial" %in% classificationLower$environments) %>% 
          unique()
        
        colNames.1$kingdomSource <- rm_origin(colNames.1$Kingdom, taxonSource)
        colNames.1$kingdomOrigin <- rm_origin(colNames.1$Kingdom, taxonOrigin)
        colNames.1$phylumSource <- rm_origin(colNames.1$Phylum, taxonSource)
        colNames.1$phylumOrigin <- rm_origin(colNames.1$Phylum, taxonOrigin)
        colNames.1$classSource <- rm_origin(colNames.1$Class, taxonSource)
        colNames.1$classOrigin <- rm_origin(colNames.1$Class, taxonOrigin)
        colNames.1$orderSource <- rm_origin(colNames.1$Order, taxonSource)
        colNames.1$orderOrigin <- rm_origin(colNames.1$Order, taxonOrigin)
        colNames.1$familySource <- rm_origin(colNames.1$Family, taxonSource)
        colNames.1$familyOrigin <- rm_origin(colNames.1$Family, taxonOrigin)
        colNames.1$genusSource <- rm_origin(colNames.1$Genus, taxonSource)
        colNames.1$genusOrigin <- rm_origin(colNames.1$Genus, taxonOrigin)
        colNames.1$speciesSource <- rm_origin(colNames.1$Species, taxonSource)
        colNames.1$speciesOrigin <- rm_origin(colNames.1$Species, taxonOrigin)
        colNames.1$subspeciesSource <- rm_origin(colNames.1$Subspecies, taxonSource)
        colNames.1$subspeciesOrigin <- rm_origin(colNames.1$Subspecies, taxonOrigin)
        
      }
      
      # Synonyms and more
      if(length(status) == 1 && status != "accepted"){
        
        # ID of synonym
        id.sp <- json$result$usage$id
        
        # Accepted name from synonym ID
        json.syn <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/synonym/", id.sp))
        json.syn.acc <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/nameusage/search?content=SCIENTIFIC_NAME&q=", gsub(" ", "%20", json.syn$accepted$name$scientificName), "&type=EXACT&offset=0&limit=1"))
        
        
        classification <- as.data.frame(json.syn.acc$result$classification)
        rank <- classification$rank[nrow(classification)]
        
        # Lower classification ID
        classificationID <- classification$id[classification$rank == rank][1]
        
        # Classification rank into the list 
        # Api COL: https://api.checklistbank.org/dataset/9923/taxon/8TN37
        classificationLower <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/taxon/", classificationID))
        
        taxonLower <- ch0_to_Na(classificationLower$name$scientificName) 
        authorLower <- ch0_to_Na(classificationLower$name$authorship)
        
        # Habitat
        # habitat <- ch0_to_Na(classificationLower$environments)
        
        # Higher classification compared to the rank into the list 
        # Api COL: https://api.checklistbank.org/dataset/9923/taxon/8TN37/classification
        classificationHigher <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/taxon/", classificationID, "/classification"))
        
        # Taxon classification
        taxonHigherKingdom <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "kingdom"])
        authorHigherKingdom <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "kingdom"])
        taxonHigherPhylum <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "phylum"])
        authorHigherPhylum <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "phylum"])
        taxonHigherClass <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "class"])
        authorHigherClass <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "class"])
        taxonHigherOrder <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "order"])
        authorHigherOrder <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "order"])
        taxonHigherFamily <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "family"])
        authorHigherFamily <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "family"])
        taxonHigherGenus <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "genus"])
        authorHigherGenus <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "genus"])
        taxonHigherSpecies <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "species"])
        authorHigherSpecies <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "species"])
        taxonHigherSubspecies <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "subspecies"])
        authorHigherSubspecies <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "subspecies"])
        
        
        # Dataframe to add to the main one
        colNames.1 <- data.frame(originalName = sp.1,
                                 colNamesAccepted = classification$name[classification$rank == rank],
                                 colID = json$result$id,
                                 Kingdom = ifelse(rank == "kingdom", taxonLower, taxonHigherKingdom),
                                 kingdomAuthor = ifelse(rank == "kingdom", authorLower, authorHigherKingdom),
                                 kingdomSource = "",
                                 kingdomOrigin = "",
                                 Phylum = ifelse(rank == "phylum", taxonLower, taxonHigherPhylum),
                                 phylumAuthor = ifelse(rank == "phylum", authorLower, authorHigherPhylum),
                                 phylumSource = "",
                                 phylumOrigin = "",
                                 Class = ifelse(rank == "class", taxonLower, taxonHigherClass),
                                 classAuthor = ifelse(rank == "class", authorLower, authorHigherClass),
                                 classSource = "",
                                 classOrigin = "",
                                 Order = ifelse(rank == "order", taxonLower, taxonHigherOrder),
                                 orderAuthor = ifelse(rank == "order", authorLower, authorHigherOrder),
                                 orderSource = "",
                                 orderOrigin = "",
                                 Family = ifelse(rank == "family", taxonLower, taxonHigherFamily),
                                 familyAuthor = ifelse(rank == "family", authorLower, authorHigherFamily),
                                 familySource = "",
                                 familyOrigin = "",
                                 Genus = ifelse(rank == "genus", taxonLower, taxonHigherGenus),
                                 genusAuthor = ifelse(rank == "genus", authorLower, authorHigherGenus),
                                 genusSource = "",
                                 genusOrigin = "",
                                 Species = ifelse(rank == "species", word(taxonLower, -1), word(taxonHigherSpecies, -1)),
                                 speciesAuthor = ifelse(rank == "species", authorLower, authorHigherSpecies),
                                 speciesSource = "",
                                 speciesOrigin = "",
                                 Subspecies = ifelse(rank == "subspecies", word(taxonLower, -1), word(taxonHigherSubspecies, -1)),
                                 subspeciesAuthor = ifelse(rank == "subspecies", authorLower, authorHigherSubspecies),
                                 subspeciesSource = "",
                                 subspeciesOrigin = "",
                                 originalStatus = status,
                                 taxonRank = rank,
                                 brackish = "brackish" %in% classificationLower$environments,
                                 freshwater = "freshwater" %in% classificationLower$environments,
                                 marine = "marine" %in% classificationLower$environments,
                                 terrestrial = "terrestrial" %in% classificationLower$environments) %>% 
          unique()
        
        colNames.1$kingdomSource <- rm_origin(colNames.1$Kingdom, taxonSource)
        colNames.1$kingdomOrigin <- rm_origin(colNames.1$Kingdom, taxonOrigin)
        colNames.1$phylumSource <- rm_origin(colNames.1$Phylum, taxonSource)
        colNames.1$phylumOrigin <- rm_origin(colNames.1$Phylum, taxonOrigin)
        colNames.1$classSource <- rm_origin(colNames.1$Class, taxonSource)
        colNames.1$classOrigin <- rm_origin(colNames.1$Class, taxonOrigin)
        colNames.1$orderSource <- rm_origin(colNames.1$Order, taxonSource)
        colNames.1$orderOrigin <- rm_origin(colNames.1$Order, taxonOrigin)
        colNames.1$familySource <- rm_origin(colNames.1$Family, taxonSource)
        colNames.1$familyOrigin <- rm_origin(colNames.1$Family, taxonOrigin)
        colNames.1$genusSource <- rm_origin(colNames.1$Genus, taxonSource)
        colNames.1$genusOrigin <- rm_origin(colNames.1$Genus, taxonOrigin)
        colNames.1$speciesSource <- rm_origin(colNames.1$Species, taxonSource)
        colNames.1$speciesOrigin <- rm_origin(colNames.1$Species, taxonOrigin)
        colNames.1$subspeciesSource <- rm_origin(colNames.1$Subspecies, taxonSource)
        colNames.1$subspeciesOrigin <- rm_origin(colNames.1$Subspecies, taxonOrigin)
      }
      
    }
    
    colNames <- rbind(colNames, colNames.1)
    
    # print(paste(i, "---- of ----", length(x)))
    
    # Increment the progress bar, and update the detail text.
    incProgress(1/length(x), detail = paste("Doing:", i))
    
  }
   ) 
  
  return(colNames)
  
}