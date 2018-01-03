#' Are packages installed?
#'
#' Vectorized check for package installation.
#'
#' @param ... `[character]` one or more package names.
#'
#' @return Returns a `[named:logical]`. Names correspond to package names,
#' values correspond to whether the package is installed (`TRUE/FALSE`).
#' Length of result corresponds to unique package names passed to `...`.
#'
#' @examples \dontrun{
#'   others <- c('rmarkdown', 'ggplot2')
#'   is_installed <- .Rprofile$installed('tidyverse', 'devtools', others)
#'   is_installed['devtools']
#' }
#'
#' @importFrom utils installed.packages
#' @export
#' @md

pkg_installed  <- function(...) {
  pkgs   <- unique(unlist(c(...)))
  result <- pkgs %in% rownames(utils::installed.packages())
  names(result) <- pkgs
  return(result)
}

pkg_version <- function(...) {
  pkgs   <- unique(unlist(c(...)))
  result <- sapply(pkgs, function(x) utils::packageDescription(x)$Version)
  names(result) <- pkgs
  return(result)
}

pkg_repo <- function(..., cran_version = NULL, bioc_version = NULL) {
  pkgs   <- unique(unlist(c(...)))
  description <- utils::packageDescription(x)

  if (!is.null(d$GithubRef)) {
    return(with(description, paste0('(Github.com/', GithubUsername, '/', GithubRepo, '@', GithubRef, ')')))
  } else if (!is.null(d$biocViews)) {
    return(paste0('(Bioc ', bioc_version, ')'))
  } else {
    return(paste0('(CRAN ', cran_version, ')'))
  }
}

#' Get GitHub commit reference
#'
#' Vectorized GitHub commit reference determination for packages installed with
#' `devtools::install_github`, or packages with a `GithubRef` field in their
#' DESCRIPTION file.
#'
#' @param ... `[character]` one or more package names.
#'
#' @return Returns a `[named:character]`. Names correspond to package names,
#' values correspond to GitHub SHA as determined from the `GithubRef` in the
#' package's DESCRIPTION file. Returns `NA` for packages without a `GithubRef`
#' and for uninstalled packages.
#'
#' @importFrom utils packageDescription
#' @export
#' @md

pkg_github_ref <- function(...) {

  pkgs <- unique(unlist(c(...)))

  result <- vapply(
    pkgs,
    FUN = function(x) {
      if (!.Rprofile$installed(x)) {
        return(NA_character_)
      } else {
        ref <- utils::packageDescription(x)$GithubRef
        if (is.null(ref)) ref <- NA_character_
      }
    },
    FUN.VALUE = character(length(pkgs))
  )
  names(result) <- pkgs
  return(result)
}
