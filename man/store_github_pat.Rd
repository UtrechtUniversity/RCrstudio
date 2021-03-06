% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github.R
\name{store_github_pat}
\alias{store_github_pat}
\title{Store a GitHub PAT}
\usage{
store_github_pat()
}
\description{
Store GitHub PAT To facilitate the use of GitHub for push/pull upstream repos in RStudio projects
and for installing packages from GitHub.
}
\details{
The user is asked to enter a GitHub PAT.
This PAT is stored in a file in the user environment.
The HOME/.Rprofile is edited to set the GITHUB_PAT variable at the beginning of
every R session to the value of the stored PAT.
}
