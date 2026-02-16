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
get_info <- function(df = NULL, col_name = NULL, rank = NULL) {
  if (is.null(df) || is.null(col_name) || is.null(rank)) {
    return(NA)
  }
  val <- df[df[["rank"]] == rank, col_name]
  if (length(val) == 0) {
    return(NA)
  } else {
    return(val)
  }
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
col_by_name <- function(x, dataset_number = 312361) {
  fun_result <- list(
    resolved = data.frame(),
    ambiguous = list()
  )

  # Only define progress if running inside Shiny
  in_shiny <- exists("getDefaultReactiveDomain") && !is.null(getDefaultReactiveDomain())

  if (in_shiny) {
    withProgress(message = "Processing species names...", value = 0, {
      n <- length(x)

      for (i in seq_along(x)) {
        name <- x[i]

        sp_name <- URLencode(name)
        json_sp <- tryCatch(
          {
            fromJSON(paste0(
              "https://api.checklistbank.org/dataset/", dataset_number,
              "/nameusage/search?content=SCIENTIFIC_NAME&q=", sp_name,
              "&type=EXACT&offset=0&limit=20"
            ))
          },
          error = function(e) NULL
        )

        # Species not found
        if (is.null(json_sp) || isTRUE(json_sp$empty)) {
          fun_result$resolved <- rbind(
            fun_result$resolved,
            data.frame(
              originalName = name,
              colNamesAccepted = "Not found",
              colID = "Not found",
              taxonRank = "Not found",
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
              stringsAsFactors = FALSE
            )
          )
          if (in_shiny) incProgress(1 / n)
          next
        }

        status <- json_sp$result$usage$status
        acc <- grepl("accepted", status)

        # Multiple accepted names
        if (length(status) > 1) {
          ambiguous_temp <- list()
          ambiguous_temp[[name]] <- json_sp$result$usage$id
          fun_result$ambiguous <- c(fun_result$ambiguous, ambiguous_temp)
          if (in_shiny) incProgress(1 / length(x))
          next
        }

        # Single accepted name
        if (any(acc)) {
          taxon_lower_df <- data.frame(
            id = ch0_to_Na(json_sp$result$usage$id),
            name = ch0_to_Na(json_sp$result$usage$name$scientificName),
            authorship = ch0_to_Na(json_sp$result$usage$name$authorship),
            rank = ch0_to_Na(json_sp$result$usage$name$rank),
            stringsAsFactors = FALSE
          )

          # Classification of the higher taxonomic level
          taxon_higher_df <- tryCatch(
            {
              fromJSON(paste0(
                "https://api.checklistbank.org/dataset/", dataset_number,
                "/taxon/", taxon_lower_df$id, "/classification"
              )) %>%
                select(id, name, authorship, rank) %>%
                slice(n():1) # Revert order
            },
            error = function(e) data.frame(id = NA, name = NA, authorship = NA, rank = NA)
          )

          classification <- rbind(taxon_higher_df, taxon_lower_df)

          resolved_temp <- data.frame(
            originalName = name,
            colNamesAccepted = taxon_lower_df$name,
            colID = taxon_lower_df$id,
            taxonRank = taxon_lower_df$rank,
            Life = "Life",
            lifeAuthor = NA,
            Kingdom = get_info(classification, "name", "kingdom"),
            kingdomAuthor = get_info(classification, "authorship", "kingdom"),
            Phylum = get_info(classification, "name", "phylum"),
            phylumAuthor = get_info(classification, "authorship", "phylum"),
            Parvphylum = get_info(classification, "name", "parvphylum"),
            parvphylumAuthor = get_info(classification, "authorship", "parvphylum"),
            Gigaclass = get_info(classification, "name", "gigaclass"),
            gigaclassAuthor = get_info(classification, "authorship", "gigaclass"),
            Class = get_info(classification, "name", "class"),
            classAuthor = get_info(classification, "authorship", "class"),
            Order = get_info(classification, "name", "order"),
            orderAuthor = get_info(classification, "authorship", "order"),
            Family = get_info(classification, "name", "family"),
            familyAuthor = get_info(classification, "authorship", "family"),
            Genus = get_info(classification, "name", "genus"),
            genusAuthor = get_info(classification, "authorship", "genus"),
            Species = word(get_info(classification, "name", "species"), -1),
            speciesAuthor = get_info(classification, "authorship", "species"),
            Subspecies = word(get_info(classification, "name", "subspecies"), -1),
            subspeciesAuthor = get_info(classification, "authorship", "subspecies"),
            Variety = word(get_info(classification, "name", "variety"), -1),
            varietyAuthor = get_info(classification, "authorship", "variety"),
            stringsAsFactors = FALSE
          )

          fun_result$resolved <- rbind(fun_result$resolved, resolved_temp)
        }

        if (in_shiny) incProgress(1 / length(x))
      } # end for
    }) # end withProgress
  } else {
    # If not in Shiny, just run the loop without progress bar
    for (name in x) {
      sp_name <- URLencode(name)

      json_sp <- tryCatch(
        {
          fromJSON(paste0(
            "https://api.checklistbank.org/dataset/", dataset_number,
            "/nameusage/search?content=SCIENTIFIC_NAME&q=", sp_name,
            "&type=EXACT&offset=0&limit=20"
          ))
        },
        error = function(e) NULL
      )

      # Species not found
      if (is.null(json_sp) || isTRUE(json_sp$empty)) {
        fun_result$resolved <- rbind(
          fun_result$resolved,
          data.frame(
            originalName = name,
            colNamesAccepted = "Not found",
            colID = "Not found",
            taxonRank = "Not found",
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
            stringsAsFactors = FALSE
          )
        )
        next
      }

      status <- json_sp$result$usage$status
      acc <- grepl("accepted", status)

      # Multiple accepted names
      if (length(status) > 1) {
        ambiguous_temp <- list()
        ambiguous_temp[[name]] <- json_sp$result$usage$id
        fun_result$ambiguous <- c(fun_result$ambiguous, ambiguous_temp)
        next
      }

      # Single accepted name
      if (any(acc)) {
        taxon_lower_df <- data.frame(
          id = ch0_to_Na(json_sp$result$usage$id),
          name = ch0_to_Na(json_sp$result$usage$name$scientificName),
          authorship = ch0_to_Na(json_sp$result$usage$name$authorship),
          rank = ch0_to_Na(json_sp$result$usage$name$rank),
          stringsAsFactors = FALSE
        )

        # Classification of the higher taxonomic level
        taxon_higher_df <- tryCatch(
          {
            fromJSON(paste0(
              "https://api.checklistbank.org/dataset/", dataset_number,
              "/taxon/", taxon_lower_df$id, "/classification"
            )) %>%
              select(id, name, authorship, rank) %>%
              slice(n():1) # Revert df order (from Domain to species)
          },
          error = function(e) data.frame(id = NA, name = NA, authorship = NA, rank = NA)
        )

        classification <- rbind(taxon_higher_df, taxon_lower_df)

        # Create dataframe with taxonomic information
        resolved_temp <- data.frame(
          originalName = name,
          colNamesAccepted = taxon_lower_df$name,
          colID = taxon_lower_df$id,
          taxonRank = taxon_lower_df$rank,
          Life = "Life",
          lifeAuthor = NA,
          Kingdom = get_info(classification, "name", "kingdom"),
          kingdomAuthor = get_info(classification, "authorship", "kingdom"),
          Phylum = get_info(classification, "name", "phylum"),
          phylumAuthor = get_info(classification, "authorship", "phylum"),
          Parvphylum = get_info(classification, "name", "parvphylum"),
          parvphylumAuthor = get_info(classification, "authorship", "parvphylum"),
          Gigaclass = get_info(classification, "name", "gigaclass"),
          gigaclassAuthor = get_info(classification, "authorship", "gigaclass"),
          Class = get_info(classification, "name", "class"),
          classAuthor = get_info(classification, "authorship", "class"),
          Order = get_info(classification, "name", "order"),
          orderAuthor = get_info(classification, "authorship", "order"),
          Family = get_info(classification, "name", "family"),
          familyAuthor = get_info(classification, "authorship", "family"),
          Genus = get_info(classification, "name", "genus"),
          genusAuthor = get_info(classification, "authorship", "genus"),
          Species = word(get_info(classification, "name", "species"), -1),
          speciesAuthor = get_info(classification, "authorship", "species"),
          Subspecies = word(get_info(classification, "name", "subspecies"), -1),
          subspeciesAuthor = get_info(classification, "authorship", "subspecies"),
          Variety = word(get_info(classification, "name", "variety"), -1),
          varietyAuthor = get_info(classification, "authorship", "variety"),
          stringsAsFactors = FALSE
        )

        fun_result$resolved <- rbind(fun_result$resolved, resolved_temp)
      }
    }
  }
  return(fun_result)
}


#' Resolve Taxa by ID Using the ChecklistBank API
#'
#' Retrieves accepted taxonomic information for one or more taxon IDs
#' from a specified ChecklistBank dataset via the API.
#'
#' For each supplied taxon ID, the function returns the accepted
#' scientific name and its full taxonomic classification
#' (Kingdom to infraspecific ranks, when available).
#'
#' If a taxon ID is not found or an error occurs during the API request,
#' "Not found" values are returned for all taxonomic fields.
#'
#' When executed inside a Shiny application, a progress bar is displayed.
#'
#' @param x A character vector of ChecklistBank taxon IDs.
#' @param dataset_number Numeric. ChecklistBank dataset identifier.
#'   Default is \code{312361}.
#'
#' @details
#' The function uses the ChecklistBank API:
#' \url{https://api.checklistbank.org/}
#'
#' Classification data are retrieved from the corresponding
#' \code{/classification} endpoint of the dataset.
#'
#' @examples
#' \dontrun{
#' ids <- c("BDW6P", "7QH3K")
#' result <- col_by_id(ids)
#'
#' # View resolved taxa
#' result
#' }
#'
col_by_id <- function(x, dataset_number = 312361) {
  fun_result <- data.frame()


  # Only define progress if running inside Shiny
  in_shiny <- exists("getDefaultReactiveDomain") && !is.null(getDefaultReactiveDomain())

  if (in_shiny) {
    withProgress(message = "Processing species names...", value = 0, {
      n <- length(x)

      for (i in seq_along(x)) {
        sp_id <- URLencode(x[i])
        json_sp <- tryCatch(
          {
            fromJSON(paste0("https://api.checklistbank.org/dataset/", dataset_number, "/taxon/", sp_id))
          },
          error = function(e) NULL
        )

        # Species not found
        if (is.null(json_sp) || isTRUE(json_sp$empty)) {
          fun_result <- rbind(
            fun_result,
            data.frame(
              originalName = "Not found",
              colNamesAccepted = "Not found",
              colID = sp_id,
              taxonRank = "Not found",
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
              stringsAsFactors = FALSE
            )
          )
          if (in_shiny) incProgress(1 / n)
          next
        }

        status <- json_sp$status
        acc <- grepl("accepted", status)

        # Single accepted name
        if (any(acc)) {
          taxon_lower_df <- data.frame(
            id = ch0_to_Na(json_sp$id),
            name = ch0_to_Na(json_sp$name$scientificName),
            authorship = ch0_to_Na(json_sp$name$authorship),
            rank = ch0_to_Na(json_sp$name$rank),
            stringsAsFactors = FALSE
          )

          # Classification of the higher taxonomic level
          taxon_higher_df <- tryCatch(
            {
              fromJSON(paste0(
                "https://api.checklistbank.org/dataset/",
                dataset_number, "/taxon/", taxon_lower_df$id, "/classification"
              )) %>%
                select(id, name, authorship, rank) %>%
                slice(n():1) # Revert df order (from Domain to species)
            },
            error = function(e) data.frame(id = NA, name = NA, authorship = NA, rank = NA)
          )

          classification <- rbind(taxon_higher_df, taxon_lower_df)

          resolved_temp <- data.frame(
            originalName = taxon_lower_df$name,
            colNamesAccepted = taxon_lower_df$name,
            colID = taxon_lower_df$id,
            taxonRank = taxon_lower_df$rank,
            Life = "Life",
            lifeAuthor = NA,
            Kingdom = get_info(classification, "name", "kingdom"),
            kingdomAuthor = get_info(classification, "authorship", "kingdom"),
            Phylum = get_info(classification, "name", "phylum"),
            phylumAuthor = get_info(classification, "authorship", "phylum"),
            Parvphylum = get_info(classification, "name", "parvphylum"),
            parvphylumAuthor = get_info(classification, "authorship", "parvphylum"),
            Gigaclass = get_info(classification, "name", "gigaclass"),
            gigaclassAuthor = get_info(classification, "authorship", "gigaclass"),
            Class = get_info(classification, "name", "class"),
            classAuthor = get_info(classification, "authorship", "class"),
            Order = get_info(classification, "name", "order"),
            orderAuthor = get_info(classification, "authorship", "order"),
            Family = get_info(classification, "name", "family"),
            familyAuthor = get_info(classification, "authorship", "family"),
            Genus = get_info(classification, "name", "genus"),
            genusAuthor = get_info(classification, "authorship", "genus"),
            Species = word(get_info(classification, "name", "species"), -1),
            speciesAuthor = get_info(classification, "authorship", "species"),
            Subspecies = word(get_info(classification, "name", "subspecies"), -1),
            subspeciesAuthor = get_info(classification, "authorship", "subspecies"),
            Variety = word(get_info(classification, "name", "variety"), -1),
            varietyAuthor = get_info(classification, "authorship", "variety"),
            stringsAsFactors = FALSE
          )

          fun_result <- rbind(fun_result, resolved_temp)
        }

        if (in_shiny) incProgress(1 / length(x))
      } # end for
    }) # end withProgress
  } else {
    # If not in Shiny, just run the loop without progress bar
    for (id in x) {
      sp_id <- URLencode(id)

      json_sp <- tryCatch(
        {
          fromJSON(paste0("https://api.checklistbank.org/dataset/", dataset_number, "/taxon/", sp_id))
        },
        error = function(e) NULL
      )

      # Species not found
      if (is.null(json_sp) || isTRUE(json_sp$empty)) {
        fun_result <- rbind(
          fun_result,
          data.frame(
            originalName = "Not found",
            colNamesAccepted = "Not found",
            colID = sp_id,
            taxonRank = "Not found",
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
            stringsAsFactors = FALSE
          )
        )
        next
      }

      status <- json_sp$status
      acc <- grepl("accepted", status)

      # Single accepted name
      if (any(acc)) {
        taxon_lower_df <- data.frame(
          id = ch0_to_Na(json_sp$id),
          name = ch0_to_Na(json_sp$name$scientificName),
          authorship = ch0_to_Na(json_sp$name$authorship),
          rank = ch0_to_Na(json_sp$name$rank),
          stringsAsFactors = FALSE
        )

        # Classification of the higher taxonomic level
        taxon_higher_df <- tryCatch(
          {
            fromJSON(paste0(
              "https://api.checklistbank.org/dataset/",
              dataset_number, "/taxon/", taxon_lower_df$id, "/classification"
            )) %>%
              select(id, name, authorship, rank) %>%
              slice(n():1) # Revert df order (from Domain to species)
          },
          error = function(e) data.frame(id = NA, name = NA, authorship = NA, rank = NA)
        )

        classification <- rbind(taxon_higher_df, taxon_lower_df)

        # Create dataframe with taxonomic information
        resolved_temp <- data.frame(
          originalName = taxon_lower_df$name,
          colNamesAccepted = taxon_lower_df$name,
          colID = taxon_lower_df$id,
          taxonRank = taxon_lower_df$rank,
          Life = "Life",
          lifeAuthor = NA,
          Kingdom = get_info(classification, "name", "kingdom"),
          kingdomAuthor = get_info(classification, "authorship", "kingdom"),
          Phylum = get_info(classification, "name", "phylum"),
          phylumAuthor = get_info(classification, "authorship", "phylum"),
          Parvphylum = get_info(classification, "name", "parvphylum"),
          parvphylumAuthor = get_info(classification, "authorship", "parvphylum"),
          Gigaclass = get_info(classification, "name", "gigaclass"),
          gigaclassAuthor = get_info(classification, "authorship", "gigaclass"),
          Class = get_info(classification, "name", "class"),
          classAuthor = get_info(classification, "authorship", "class"),
          Order = get_info(classification, "name", "order"),
          orderAuthor = get_info(classification, "authorship", "order"),
          Family = get_info(classification, "name", "family"),
          familyAuthor = get_info(classification, "authorship", "family"),
          Genus = get_info(classification, "name", "genus"),
          genusAuthor = get_info(classification, "authorship", "genus"),
          Species = word(get_info(classification, "name", "species"), -1),
          speciesAuthor = get_info(classification, "authorship", "species"),
          Subspecies = word(get_info(classification, "name", "subspecies"), -1),
          subspeciesAuthor = get_info(classification, "authorship", "subspecies"),
          Variety = word(get_info(classification, "name", "variety"), -1),
          varietyAuthor = get_info(classification, "authorship", "variety"),
          stringsAsFactors = FALSE
        )

        fun_result <- rbind(fun_result, resolved_temp)
      }
    }
  }
  return(fun_result)
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
  colNames <- data.frame()

  withProgress(
    message = "Downloading taxonomy", value = 0,
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
            terrestrial = "Multiple matches"
          )
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
            Species = ch0_to_Na(word(classification$scientificname[classification$rank == "Species"], -1)),
            Subspecies = ch0_to_Na(word(classification$scientificname[classification$rank == "Subspecies"], -1)),
            originalStatus = statusAndHabitat$status[statusAndHabitat$AphiaID == json],
            taxonRank = tolower(classification$rank[nrow(classification)]),
            brackish = statusAndHabitat$isBrackish != 0,
            freshwater = statusAndHabitat$isFreshwater != 0,
            marine = statusAndHabitat$isMarine != 0,
            terrestrial = statusAndHabitat$isTerrestrial != 0
          ) %>%
            unique()
        }
      }

      colNames <- rbind(colNames, colNames.1)

      if (exists("json")) {
        rm(json)
      }

      # print(paste(i, "---- of ----", length(x)))

      # Increment the progress bar, and update the detail text.
      incProgress(1 / length(x), detail = paste("Doing:", i))
    }
  )

  return(colNames)
}
