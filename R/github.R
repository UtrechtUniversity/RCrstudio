#' Store a GitHub PAT
#'
#' Store GitHub PAT To facilitate the use of GitHub for push/pull upstream repos in RStudio projects
#' and for installing packages from GitHub.
#'
#' @details
#' The user is asked to enter a GitHub PAT.
#' This PAT is stored in a file in the user environment.
#' The HOME/.Rprofile is edited to set the GITHUB_PAT variable at the beginning of
#' every R session to the value of the stored PAT.
#'
#'
#' @export

store_github_pat <- function() {
  credentials::credential_helper_set(helper = 'store', global = TRUE)  # key chain would be better
  pat_ok <- credentials::set_github_pat(force_new = TRUE)                        # asks user to insert a PAT
  if (!pat_ok) {
    warning('GitHub PAT not OK')
    return(FALSE)
  }

  # set github pat as environment variable at start of every session
  #
  rprofile_name <- file.path(Sys.getenv('HOME'), '.Rprofile')
  rprofile_cmd <- c(
    "# Set github PAT for session (added by config_workspace)",
    "#",
    "credentials::set_github_pat()",
    ""
  )
  write(x = rprofile_cmd, file = rprofile_name, append = TRUE)
  Sys.chmod(paths = rprofile_name, mode = '0600')

  return(TRUE)
}
