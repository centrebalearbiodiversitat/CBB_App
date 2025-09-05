# X is the vector containing the species to search.

# x <- fread("~/OneDrive - Universitat de les Illes Balears/CBB objectives/CBB_DB/Taxonomy/ToCheck/thTaxa_2023_11_16.csv")
# x <- zoo$x
# 
# i=1

cbbdbCol <- function(x, dataset_number = 312092) {
  
  resolved_df <- data.frame()
  ambiguous_list <- list()
  
  withProgress(message = "Downloading taxonomy", value = 0, {
    
    for(i in seq_along(x)) {
      
      sp.1 <- x[i]
      json.sp <- gsub(" ", "%20", sp.1)
      json <- fromJSON(paste0(
        "https://api.checklistbank.org/dataset/", dataset_number,
        "/nameusage/search?content=SCIENTIFIC_NAME&q=", json.sp,
        "&type=EXACT&offset=0&limit=50"
      ))
      
      # Species not found
      if(isTRUE(json$empty)) {
        resolved_df <- rbind(resolved_df,
                             data.frame(originalName = sp.1,
                                        colNamesAccepted = "Not found",
                                        colID = "Not found",
                                        Kingdom = "Not found",
                                        kingdomAuthor = "Not found",
                                        Phylum = "Not found",
                                        phylumAuthor = "Not found",
                                        Class = "Not found",
                                        classAuthor = "Not found",
                                        Order = "Not found",
                                        orderAuthor = "Not found",
                                        Family = "Not found",
                                        familyAuthor = "Not found",
                                        Genus = "Not found",
                                        genusAuthor = "Not found",
                                        Species = "Not found",
                                        speciesAuthor = "Not found",
                                        Subspecies = "Not found",
                                        subspeciesAuthor = "Not found",
                                        Variety = "Not found",
                                        varietyAuthor = "Not found",
                                        originalStatus = "Not found",
                                        taxonRank = "Not Found",
                                        brackish = "Not Found",
                                        freshwater = "Not Found",
                                        marine = "Not Found",
                                        terrestrial = "Not Found"))
      } else {
        
        status <- json$result$usage$status
        acc <- grepl("accepted", status)
        
        # Multiple accepted names
        if(length(which(acc)) > 1) {
          ambiguous_list[[sp.1]] <- json$result$usage$id[acc]
          next
        }
        
        # Single accepted name
        if(any(acc)) {
          classification <- bind_rows(json$result$classification[acc])
          rank <- classification$rank[nrow(classification)]
          classificationID <- classification$id[classification$rank == rank]
          
          classificationLower <- fromJSON(paste0("https://api.checklistbank.org/dataset/",
                                                 dataset_number, "/taxon/", classificationID))
          taxonLower <- ch0_to_Na(classificationLower$name$scientificName)
          authorLower <- ch0_to_Na(classificationLower$name$authorship)
          
          classificationHigher <- fromJSON(paste0("https://api.checklistbank.org/dataset/",
                                                  dataset_number, "/taxon/", classificationID, "/classification"))
          getHigher <- function(rank) {
            val <- classificationHigher$name[classificationHigher$rank == rank]
            if(length(val) == 0) return(NA) else return(val)
          }
          getAuthor <- function(rank) {
            val <- classificationHigher$authorship[classificationHigher$rank == rank]
            if(length(val) == 0) return(NA) else return(val)
          }
          
          resolved_df <- rbind(resolved_df,
                               data.frame(
                                 originalName = sp.1,
                                 colNamesAccepted = taxonLower,
                                 colID = classificationID,
                                 Kingdom = getHigher("kingdom"),
                                 kingdomAuthor = getAuthor("kingdom"),
                                 Phylum = getHigher("phylum"),
                                 phylumAuthor = getAuthor("phylum"),
                                 Class = getHigher("class"),
                                 classAuthor = getAuthor("class"),
                                 Order = getHigher("order"),
                                 orderAuthor = getAuthor("order"),
                                 Family = getHigher("family"),
                                 familyAuthor = getAuthor("family"),
                                 Genus = getHigher("genus"),
                                 genusAuthor = getAuthor("genus"),
                                 Species = word(taxonLower, -1),
                                 speciesAuthor = authorLower,
                                 Subspecies = NA,
                                 subspeciesAuthor = NA,
                                 Variety = NA,
                                 varietyAuthor = NA,
                                 originalStatus = "accepted",
                                 taxonRank = rank,
                                 brackish = "brackish" %in% classificationLower$environments,
                                 freshwater = "freshwater" %in% classificationLower$environments,
                                 marine = "marine" %in% classificationLower$environments,
                                 terrestrial = "terrestrial" %in% classificationLower$environments
                               ))
        }
      }
      
      # Increment progress
      incProgress(1/length(x), detail = paste("Processing:", i, "of", length(x)))
    } # end loop
    
  }) # end withProgress
  
  return(list(resolved = resolved_df, ambiguous = ambiguous_list))
}



