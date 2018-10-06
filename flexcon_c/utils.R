#' @description Install packages if it not installed and load them.
#'
installNeedPacks <- function() {
  packages <- c("ssc", "plyr", "DMwR", "DMwR2", "RWeka",
                "rminer", "e1071", "ggplot2", "stats")
  for (pack in packages) {
    if (!require(pack, character.only = TRUE)) {
      install.packages(pack)
    }
    library(pack, character.only = TRUE)
  }
}

installNeedPacks()

#' @description Provide a way to paste 2 or more words.
#'
#' @param vec the words to be pasted in the order.
#'
#' @return a string with the vec words collapsed in _.
join <- function(vec, collapse = "_") {
  return (paste(vec[1:length(vec)], collapse = collapse))
}

#' @description Provide a way to read files and put names in the columns.
#'
#' @param file The file must be read.
#'
#' @return The content of the file.
#'
readFile <- function(file) {
  return (read.csv(file, header = FALSE, col.names = c("V1", "V2", "V3", "V4",
                                                       "V5")))
}

#' @description Write in the output file the content.
#'
#' @usage write_archive (title, content, append = TRUE)
#'
#' @param title the title of the file.
#' @param content the content of the file.
#' @param append the method to write in the archive.
#'
writeArchive <- function(title, content, append = TRUE) {
  write.table(content, title, row.names = FALSE, append = append, sep = ",",
              col.names = FALSE)
}
