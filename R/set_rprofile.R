#' Set GitHub PAT at start of every session
#'
#' @export

set_rprofile <- function() {
  rprofile_name <- file.path(Sys.getenv('HOME'), '.Rprofile')
  rprofile_cmd <- c(
    "# Set github PAT for session (added by config_workspace)",
    "credentials::set_github_pat()",
    ""
  )
  write(x = rprofile_cmd, file = rprofile_name, append = TRUE)
  Sys.chmod(paths = rprofile_name, mode = '0600')
}
