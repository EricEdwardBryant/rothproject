#' @importFrom cli rule symbol
#' @importFrom crayon bold green blue magenta col_align col_nchar

load <- function(proj, show_system = TRUE, show_attached = TRUE, show_loaded = FALSE) {

  # Check for required packages specified in .Rprofile
  needs_install <- install(proj, check_only = TRUE)

  if (length(needs_install)) {
    message(
      'This project requires the following package(s):\n  ',
      paste0(needs_install, collapse = '\n  '),
      '\nPlease install them by running rothproject::install()',
      '\nOnce all requirements have been installed, run rothproject::load()'
    )
    return(invisible())
  }

  # Load libraries and source directories
  suppressPackageStartupMessages(lapply(Rprofile$load, library, character.only = TRUE, warn.conflicts = FALSE))
  lapply(Rprofile$source_dirs, source_directories, lists = names(Rprofile$source_dirs))


  if (show_system) {
    message(cli::rule(left = crayon::bold(with(session$R.version, paste0('R ', major, '.', minor, ' "', nickname, '"'))), width = 80))
    info <- c(session$running, session$platform, normalizePath(Rprofile$Library_root))
    fields <- c('System', 'Platform', 'Library')
    msg <-
      paste0(
        crayon::magenta(cli::symbol$bullet), ' ',
        crayon::blue(format(fields)), ' ',
        crayon::col_align(info, max(crayon::col_nchar(info)))
      )
    message(paste(msg, collapse = '\n'))
  }

  if (show_attached) {
    message(cli::rule(left = crayon::bold("Packages attached"), width = 80))
    attached <- rev(names(session$otherPkgs))
    versions <- vapply(attached, package_version, character(1))
    repos    <- vapply(attached, repo_version, character(1))
    i <- 1:floor(length(attached) / 2)
    col1 <-
      paste0(
        crayon::green(cli::symbol$tick), " ",
        crayon::blue(format(attached[i])), " ",
        crayon::col_align(versions[i], max(crayon::col_nchar(versions[i]))), " ",
        crayon::col_align(repos[i], max(crayon::col_nchar(repos[i])))
      )
    col2 <-
      paste0(
        crayon::green(cli::symbol$tick), " ",
        crayon::blue(format(attached[-i])), " ",
        crayon::col_align(versions[-i], max(crayon::col_nchar(versions[-i]))), " ",
        crayon::col_align(repos[-i], max(crayon::col_nchar(repos[-i])))
      )

    info <- paste0(col1, "   ", col2)
    message(paste(info, collapse = "\n"))
  }


  if (show_loaded) {
    message(cli::rule(left = crayon::bold("Packages loaded via namespace"), width = 80))
    loaded <- rev(names(session$loadedOnly))
    versions <- vapply(loaded, package_version, character(1))
    repos    <- vapply(loaded, repo_version, character(1))
    i <- 1:floor(length(loaded) / 2)
    col1 <-
      paste0(
        crayon::green(cli::symbol$tick), " ",
        crayon::blue(format(loaded[i])), " ",
        crayon::col_align(versions[i], max(crayon::col_nchar(versions[i]))), " ",
        crayon::col_align(repos[i], max(crayon::col_nchar(repos[i])))
      )
    col2 <-
      paste0(
        crayon::green(cli::symbol$tick), " ",
        crayon::blue(format(loaded[-i])), " ",
        crayon::col_align(versions[-i], max(crayon::col_nchar(versions[-i]))), " ",
        crayon::col_align(repos[-i], max(crayon::col_nchar(repos[-i])))
      )

    info <- paste0(col1, "   ", col2)
    message(paste(info, collapse = "\n"))
  }
  invisible()
}
