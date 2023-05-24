#The following functions are used to clean column headers into a usable format
capitaliseAfterDot <- function(x) {
    words <- strsplit(x, "\\.")  # Split the string
    # Capitalise first letter of each word
    words <- sapply(words, function(word) paste0(toupper(substr(word, 1, 1)), substr(word, 2, nchar(word))))
    # Combine the words back together
    sapply(words, paste, collapse = '')
}


ExtractColumnHeaders <- function(headers){

    result <- str_extract(headers, "(?<=\\.\\.).*" ) # Extract relevant column headers
    string <- str_extract(result, "(?<=\\.\\.).*" )
    result <- ifelse(str_detect(string, "\\.\\."), str_extract(string, ".*(?=\\.\\.)"), string)

    string <- capitaliseAfterDot(result)
    return(string)
}
