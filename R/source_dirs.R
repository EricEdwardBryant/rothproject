#' Source all R scripts in a directory
#'
#' Sources all R scripts in a directory in the order returned by [list.files].
#'
#' @param dirs `[character]` directories to source.
#' @param lists `[character]` names of empty list objects to create before
#' sourcing directories.
#'
#' @details This function sources all scripts ending in `.R` or `.r` for each
#' directory specified in `dirs`. Empty list objects may be created with names
#' specified in `lists`. This allows scripts to assign to to these list
#' objects, which can be useful for modularizing functionality in a project.
#'
#' @export
#' @md

source_dirs <- function(dirs, lists = NULL) {
  files <- list.files(dirs, '[.][Rr]', full.names = TRUE)
  lapply(lists, assign, value = list(), envir = globalenv())
  lapply(files, source)
  return(invisible(files))
}
