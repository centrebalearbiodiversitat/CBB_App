# X is the vector containing the species to search.

# library(jsonlite)
# library(tidyverse)
# taxa <- read.csv("/home/tom/Documents/GitHub/CBB_App/data/data.csv")
# taxa <- unique(taxa$Taxa)
# x <- taxa
# i=5
# dataset_number=314965
# source("/home/tom/Documents/GitHub/CBB_App/utils.R")

cbbdbCol <- function(x, dataset_number = 312361) {
  
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
                                        Life = "Not found",
                                        lifeAuthor = "Not found",
                                        Kingdom = "Not found",
                                        kingdomAuthor = "Not found",
                                        Phylum = "Not found",
                                        phylumAuthor = "Not found",
                                        Parvphylum = "Not found",
                                        parvphylumAuthor = "Not found",
                                        Gigaclass = "Not found",
                                        gigaclassAuthor = "Not found",
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
          
          # Classification of the lower taxonomic level
          classificationLower <- fromJSON(paste0("https://api.checklistbank.org/dataset/",
                                                 dataset_number, "/taxon/", classificationID))
          
          taxonLower_df <- data.frame(id = ch0_to_Na(classificationLower$id),
                                      name = ch0_to_Na(classificationLower$name$scientificName),
                                      authorship = ch0_to_Na(classificationLower$name$authorship),
                                      rank = ch0_to_Na(classificationLower$name$rank))
          
          # Classification of the higher taxonomic level
          classificationHigher <- fromJSON(paste0("https://api.checklistbank.org/dataset/",
                                                  dataset_number, "/taxon/", classificationID, "/classification")) %>%
            select(id, name, authorship, rank)
          
          classificationTotal <- rbind(taxonLower_df, classificationHigher)
          
          resolved_df <- rbind(resolved_df,
                               data.frame(
                                 originalName = sp.1,
                                 colNamesAccepted = ch0_to_Na(classificationLower$name$scientificName),
                                 colID = classificationID,
                                 Life = "Life",
                                 lifeAuthor = NA,
                                 Kingdom = getInfo(df = classificationTotal, colName = "name", rank = "kingdom"),
                                 kingdomAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "kingdom"),
                                 Phylum = getInfo(df = classificationTotal, colName = "name", rank = "phylum"),
                                 phylumAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "phylum"),
                                 Parvphylum = getInfo(df = classificationTotal, colName = "name", rank = "parvphylum"),
                                 parvphylumAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "parvphylum"),
                                 Gigaclass = getInfo(df = classificationTotal, colName = "name", rank = "gigaclass"),
                                 gigaclassAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "gigaclass"),
                                 Class = getInfo(df = classificationTotal, colName = "name", rank = "class"),
                                 classAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "class"),
                                 Order = getInfo(df = classificationTotal, colName = "name", rank = "order"),
                                 orderAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "order"),
                                 Family = getInfo(df = classificationTotal, colName = "name", rank = "family"),
                                 familyAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "family"),
                                 Genus = getInfo(df = classificationTotal, colName = "name", rank = "genus"),
                                 genusAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "genus"),
                                 Species = word(getInfo(df = classificationTotal, colName = "name", rank = "species"), -1),
                                 speciesAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "species"),
                                 Subspecies = word(getInfo(df = classificationTotal, colName = "name", rank = "subspecies"), -1),
                                 subspeciesAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "subspecies"),
                                 Variety = word(getInfo(df = classificationTotal, colName = "name", rank = "variety"), -1),
                                 varietyAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "variety"),
                                 taxonRank = rank,
                                 brackish = "brackish" %in% classificationLower$environments,
                                 freshwater = "freshwater" %in% classificationLower$environments,
                                 marine = "marine" %in% classificationLower$environments,
                                 terrestrial = "terrestrial" %in% classificationLower$environments)
                               )
        } else {
          classification <- bind_rows(json$result$classification)
          rank <- classification$rank[nrow(classification)]
          classificationID <- classification$id[classification$rank == rank & classification$status == "accepted"]

          # Classification of the lower taxonomic level
          classificationLower <- fromJSON(paste0("https://api.checklistbank.org/dataset/",
                                                 dataset_number, "/taxon/", classificationID))
          
          taxonLower_df <- data.frame(id = ch0_to_Na(classificationLower$id),
                                      name = ch0_to_Na(classificationLower$name$scientificName),
                                      authorship = ch0_to_Na(classificationLower$name$authorship),
                                      rank = ch0_to_Na(classificationLower$name$rank))
          
          # Classification of the higher taxonomic level
          classificationHigher <- fromJSON(paste0("https://api.checklistbank.org/dataset/",
                                                  dataset_number, "/taxon/", classificationID, "/classification")) %>%
            select(id, name, authorship, rank)
          
          classificationTotal <- rbind(taxonLower_df, classificationHigher)
          
          resolved_df <- rbind(resolved_df,
                               data.frame(
                                 originalName = sp.1,
                                 colNamesAccepted = ch0_to_Na(classificationLower$name$scientificName),
                                 colID = classificationID,
                                 Life = "Life",
                                 lifeAuthor = NA,
                                 Kingdom = getInfo(df = classificationTotal, colName = "name", rank = "kingdom"),
                                 kingdomAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "kingdom"),
                                 Phylum = getInfo(df = classificationTotal, colName = "name", rank = "phylum"),
                                 phylumAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "phylum"),
                                 Parvphylum = getInfo(df = classificationTotal, colName = "name", rank = "parvphylum"),
                                 parvphylumAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "parvphylum"),
                                 Gigaclass = getInfo(df = classificationTotal, colName = "name", rank = "gigaclass"),
                                 gigaclassAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "gigaclass"),
                                 Class = getInfo(df = classificationTotal, colName = "name", rank = "class"),
                                 classAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "class"),
                                 Order = getInfo(df = classificationTotal, colName = "name", rank = "order"),
                                 orderAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "order"),
                                 Family = getInfo(df = classificationTotal, colName = "name", rank = "family"),
                                 familyAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "family"),
                                 Genus = getInfo(df = classificationTotal, colName = "name", rank = "genus"),
                                 genusAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "genus"),
                                 Species = word(getInfo(df = classificationTotal, colName = "name", rank = "species"), -1),
                                 speciesAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "species"),
                                 Subspecies = word(getInfo(df = classificationTotal, colName = "name", rank = "subspecies"), -1),
                                 subspeciesAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "subspecies"),
                                 Variety = word(getInfo(df = classificationTotal, colName = "name", rank = "variety"), -1),
                                 varietyAuthor = getInfo(df = classificationTotal, colName = "authorship", rank = "variety"),
                                 taxonRank = rank,
                                 brackish = "brackish" %in% classificationLower$environments,
                                 freshwater = "freshwater" %in% classificationLower$environments,
                                 marine = "marine" %in% classificationLower$environments,
                                 terrestrial = "terrestrial" %in% classificationLower$environments)
                               )
      }
      } 
      
      # Increment progress
      incProgress(1/length(x), detail = paste("Processing:", i, "of", length(x)))
    } # end loop
    
  }) # end withProgress
  
  return(list(resolved = resolved_df, ambiguous = ambiguous_list))
}
