#' Config RStudio workspace in Research Cloud
#'
#'
config_workspace <- function() {

  github <- rstudioapi::showQuestion(title = 'GITHUB access', message = 'Store GitHub PAT?', ok = "Yes", cancel = 'No')
  if(github) {
    credentials::set_github_pat(force_new = TRUE)
  }

  # set .Rprofile
  #
  rprofile <- rstudioapi::showQuestion(title = 'Rprofile', message = 'Set PAT each session?', ok = "Yes", cancel = 'No')
  if(rprofile) {
    rprofile_name <- file.path(Sys.getenv('HOME'), '.Rpprofile')
    rprofile_cmd <- c(
      "# Set github PAT for session (added by config_workspace)",
      "credentials::set_github_pat()",
      ""
    )
    write(x = rprofile_cmd, file = rprofile_name, append = TRUE)
    Sys.chmod(paths = rprofile_name, mode = '0600')
  }

  # install icommands
  #
  icommands <- rstudioapi::showQuestion(title = 'Yoda', message = 'Install icommands?', ok = "Yes", cancel = 'No')
  if (icommands) {
    install_icommands()

  }

  # install irods environment on user level
  #
  irods_user <- rstudioapi::showQuestion(title = 'Yoda', message = 'Install/override irods environment?', ok = "Yes", cancel = 'No')
  if (irods_user) {
    set_irods_env(override = TRUE)
  }

  # sync Yoda

  #
}

