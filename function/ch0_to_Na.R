
# Function that substitute "" or NA values with selected string.

ch0_to_Na <- function(x, str = ""){
  
  y <- ifelse(length(x) == 0 || is.na(x), str, x)
  
  return(y)
  
}


# Function that remove "Source" or "Origin" if taxon data is missing

# x: taxon
# y: origin

# Example
# taxon <-  c("A", "B", NA)
# origin <-  c("x", "x", "x")
# rm_origin(taxon, origin)

rm_origin <- function(x, y){
  
  z <- ifelse(length(x) == 0 || is.na(x) || x == "", "", y)  
  
  return(z)
}


