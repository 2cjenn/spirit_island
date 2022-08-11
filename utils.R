paste_noNA <- function(x, sep=", ") {
  gsub(", " , sep, toString(abbreviations[x[!is.na(x) & x!="" & x!="NA"]] ) )
}