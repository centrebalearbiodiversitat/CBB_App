#-------#
# Utils #
#-------#

#' Retrieve Value by Rank
#'
#' Returns the value from a specified column in a data frame
#' where the column "rank" matches the provided rank value.
#'
#' @param df A data frame containing a column named \code{rank}.
#' @param colName A character string specifying the column name
#'   from which to extract the value.
#' @param rank A value used to match against the \code{rank} column.
#'
#' @examples
#' df <- data.frame(
#'   rank = 1:3,
#'   name = c("Alice", "Bob", "Charlie")
#' )
#'
#' getInfo(df, "name", 2)
#'
#' @export
getInfo <- function(df = NULL, colName = NULL, rank = NULL) {
  if (is.null(df) || is.null(colName) || is.null(rank)) return(NA)
  val <- df[df[["rank"]] == rank, colName]
  if (length(val) == 0) return(NA) else return(val)
}


#' Resolve Scientific Names Using ChecklistBank API
#'
#' Queries the ChecklistBank API to resolve scientific names against
#' a specified dataset and returns accepted names with their full
#' taxonomic classification.
#'
#' The function attempts to match each supplied name exactly. If a name
#' is not found, "Not found" values are returned. If multiple accepted
#' matches are detected, the name is flagged as ambiguous.
#'
#' @param x A character vector of scientific names to be resolved.
#' @param dataset_number Numeric. ChecklistBank dataset identifier.
#'   Default is \code{312361}.
#'   
#' If multiple accepted matches are found, the name and corresponding
#' IDs are stored in the \code{ambiguous} element of the returned list.
#'
#'
#' @section API:
#' Uses the ChecklistBank API:
#' \url{https://api.checklistbank.org/}
#'
#' @examples
#' \dontrun{
#' species <- c("Homo sapiens", "Puma concolor")
#' result <- cbbdbCol(species)
#'
#' # Resolved names
#' result$resolved
#'
#' # Ambiguous names (if any)
#' result$ambiguous
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows select
#' @importFrom stringr word
#' @export
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
        }
      }
      
      # Increment progress
      incProgress(1/length(x), detail = paste("Processing:", i, "of", length(x)))
    } # end loop
    
  }) # end withProgress
  
  return(list(resolved = resolved_df, ambiguous = ambiguous_list))
}


#' Replace Empty or Missing Values
#'
#' Replaces empty or missing values with a specified string.
#' This is a small utility function useful when parsing API results
#' where elements may be missing or of length zero.
#'
#' @param x An object to evaluate (typically a character vector).
#' @param str A character string used as replacement when \code{x}
#'   is empty (length 0) or \code{NA}. Default is an empty string \code{""}.
#'
#' @examples
#' ch0_to_Na(character(0))
#' ch0_to_Na(NA, str = "Not found")
#' ch0_to_Na("Homo sapiens")
#'
#' @keywords internal
ch0_to_Na <- function(x, str = "") {
  
  y <- ifelse(length(x) == 0 || is.na(x), str, x)
  
  return(y)
}

#' Conditional Replacement Utility
#'
#' Returns a replacement value \code{y} if \code{x} is missing, empty, or \code{NA}.
#' Otherwise, returns an empty string. Useful for cleaning data fields
#' when original values should be ignored under certain conditions.
#'
#' @param x A value (or vector) to test.
#' @param y A value to return if \code{x} is missing, empty, or \code{NA}.
#'
#' @examples
#' rm_origin(NA, "Replacement")
#' rm_origin("", "Replacement")
#' rm_origin("Data", "Replacement")
#'
#' @keywords internal
rm_origin <- function(x, y) {
  z <- ifelse(length(x) == 0 || is.na(x) || x == "", "", y)
  return(z)
}


#' Create an Info Icon with Tooltip in Shiny
#'
#' Generates a superscript info icon (`info-circle`) with a customizable tooltip.
#' Useful for providing inline help or explanations in Shiny applications.
#'
#' @param tooltipText Character. The text to display inside the tooltip. Can include HTML.
#' @param color Character. The color of the info icon. Default is `"#be4358"`.
#' @param width Numeric. Maximum width of the tooltip in pixels. Default is 300.
#'
#' @details
#' The function uses Bootstrap tooltip functionality and allows HTML content
#' in the tooltip. It injects custom CSS to justify text and control the
#' tooltip width. Include this in Shiny UI code where inline help is needed.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' ui <- fluidPage(
#'   "Hover over the icon:", 
#'   infoIconTooltip("This is some helpful information.", color = "blue", width = 250)
#' )
#' shinyApp(ui, server = function(input, output) {})
#' }
#'
#' @importFrom shiny tagList tags icon
#' @export
infoIconTooltip <- function(tooltipText, color = "#be4358", width = 300) {
  tagList(
    tags$head(tags$style(HTML(paste0(
      ".tooltip-inner {  \n",
      "  text-align: justify;  \n",
      "  max-width: ", width, "px;  \n",
      "  white-space: normal;  \n",
      "}"
    )))),
    tags$sup(
      icon(
        "info-circle",
        style = paste0("color: ", color, ";"),
        `data-toggle` = "tooltip",
        `data-html` = "true",
        title = tooltipText
      )
    ),
    tags$script(HTML("
      $(function () {
        $('[data-toggle=\"tooltip\"]').tooltip({html: true});
      });
    "))
  )
}


#' Extract Taxonomic Hierarchy from WORMS API Response
#'
#' Recursively extracts the taxonomic hierarchy from a WORMS (World Register of Marine Species)
#' API response object. Returns a data frame with ranks and scientific names.
#'
#' @param x A list representing a taxon node returned from the WORMS API.
#'   Each node is expected to contain \code{rank}, \code{scientificname}, and optionally \code{child}.
#'
#' @details
#' The function works recursively:
#' - If the node has no children (\code{x$child} is \code{NULL}), it returns a single-row data frame.
#' - If the node has children, it extracts them recursively and combines them with the current node.
#'
#' @examples
#' \dontrun{
#' # Assuming 'taxon' is a list from WORMS API
#' result <- extractTaxonWorms(taxon)
#' head(result)
#' }
#'
#' @export
extractTaxonWorms <- function(x) {
  if (is.null(x$child)) {
    return(data.frame(rank = x$rank, scientificname = x$scientificname))
  } else {
    child_df <- extractTaxonWorms(x$child)
    current_df <- data.frame(rank = x$rank, scientificname = x$scientificname)
    return(rbind(current_df, child_df))
  }
}


#' Resolve Scientific Names Using WORMS (World Register of Marine Species)
#'
#' Queries the WORMS API to retrieve accepted names, taxonomic classification,
#' status, and habitat information for a vector of scientific names.
#'
#' @param x A character vector of scientific names to resolve.
#'
#' @return A data frame with one row per input name containing:
#' \describe{
#'   \item{originalName}{The original input name.}
#'   \item{colNamesAccepted}{Accepted scientific name from WORMS.}
#'   \item{Life}{Always "Life".}
#'   \item{Kingdom, Phylum, Class, Order, Family, Genus}{Taxonomic hierarchy.}
#'   \item{Species, Subspecies}{Species-level names (lower taxa).}
#'   \item{originalStatus}{Taxonomic status from WORMS.}
#'   \item{taxonRank}{Lowest available rank for the name.}
#'   \item{brackish, freshwater, marine, terrestrial}{Logical values indicating habitat.}
#' }
#'
#' @details
#' For each input name, the function:
#' \itemize{
#'   \item Searches the WORMS API using exact matching.
#'   \item Handles cases where names are not found, returning "Not found".
#'   \item Handles multiple matches, returning "Multiple matches".
#'   \item Retrieves the taxonomic hierarchy using \code{extractTaxonWorms()}.
#'   \item Retrieves habitat and status information.
#' }
#'
#' Progress is displayed using \code{withProgress()} and \code{incProgress()} (Shiny compatible).
#' -999 when multiple matches are found
#' 
#' @examples
#' \dontrun{
#' species <- c("Homo sapiens", "Puma concolor")
#' result <- specifyWorms(species)
#' head(result)
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr %>% unique
#' @importFrom stringr word
#' @importFrom shiny withProgress incProgress
#' @export
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

