#' Install project requirements
#'
#' @param cran `[character]` packages to install from a CRAN like repository.
#' @param bioc `[character]` packages to install from Bioconductor.
#' @param github `[character]` packages to install from Github.
#' @param lib_cran `[character(1)]` where to install packages from CRAN like repositories.
#' @param lib_bioc `[character(1)]` where to install packages from Bioconductor.
#' @param lib_github `[character(1)]` where to install packages from GitHub
#' (their dependencies will also be installed here).
#' @param check_only `[logical(1)]` should installation be skipped?
#'
#' @return Returns a `[character]` vector of packages that need/needed to be
#' installed. These packages will be installed if `check_only` is `FALSE`
#' (the default).
#'
#' @export
#' @md

install <- function(cran, bioc, github, lib_cran, lib_bioc, lib_github, check_only = FALSE) {

  need_cran <- install_from_repos(cran, lib = lib_cran, check_only = check_only)
  need_bioc <- install_from_repos(bioc, lib = lib_bioc, check_only = check_only)
  need_gith <- install_from_github(github, libs = lib_github, check_only = check_only)

  install_needed <- c(need_cran, need_bioc, need_gith)

  if (!check_only & length(install_needed)) {
    message('If all packages installed successfully, please run .Rprofile$load_requirements()')
  }

  return(invisible(install_needed))
}

#' Install from CRAN style repositories
#'
#' @param ... `[character]` one or more package names.
#' @param lib `[character(1)]` a single library path. Location of package
#' installation.
#' @param check_only `[logical(1)]` if `TRUE` (the default) skip installation.
#'
#' @return Returns a `[character]` vector of packages that need to be
#' installed. If `check_only` is `FALSE`, these packages will be installed
#' via [utils::install.packages].
#'
#' @importFrom utils install.packages
#' @md

install_from_repos <- function(..., lib, check_only = TRUE) {

  pkgs <- unique(unlist(c(...)))

  # Determine package installation status
  not_installed <- !installed(pkgs)
  needs_install <- pkgs[not_installed]

  # Install if not only checking
  if (!check_only) {
    lapply(needs_install, utils::install.packages, lib = lib)
  }

  return(invisible(needs_install))
}


#' Install from GitHub style repositories
#'
#' @param urls `[character]` GitHub repository URLs (e.g. username/reponame@commit)
#' @param lib `[character(1)]` where to install packages.
#'
#' @importFrom withr with_libpaths
#' @importFrom devtools install_github
#' @md

install_from_github <- function(urls, lib, check_only = TRUE) {

  # Parse github url
  user <- gsub('/.*$', '', urls)
  pkgs <- gsub('^.*?/|@.*?$', '', urls)
  sha  <- gsub('^.*?@', '', urls)

  # Determine package installation status
  not_installed <- !installed(pkgs)
  wrong_version <- sha != github_ref(pkgs)
  needs_install <- urls[not_installed | wrong_version]

  # Install if not only checking
  if (!check_only) {
    lapply(needs_install, function(pkg) {
      # Install into GitHub library
      withr::with_libpaths(
        lib,
        devtools::install_github(pkg, dependencies = TRUE, upgrade_dependencies = FALSE)
      )
    })
  }

  return(invisible(needs_install))
}
