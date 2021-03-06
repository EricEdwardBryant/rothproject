% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install.R
\name{install_from_repos}
\alias{install_from_repos}
\title{Install from CRAN style repositories}
\usage{
install_from_repos(..., lib, check_only = TRUE)
}
\arguments{
\item{...}{\code{[character]} one or more package names.}

\item{lib}{\code{[character(1)]} a single library path. Location of package
installation.}

\item{check_only}{\code{[logical(1)]} if \code{TRUE} (the default) skip installation.}
}
\value{
Returns a \code{[character]} vector of packages that need to be
installed. If \code{check_only} is \code{FALSE}, these packages will be installed
via \link[utils:install.packages]{utils::install.packages}.
}
\description{
Install from CRAN style repositories
}
