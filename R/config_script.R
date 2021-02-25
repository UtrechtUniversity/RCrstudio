#' Config RStudio workspace in Research Cloud
#'
#' @export
config_workspace <- function() {

  github <- rstudioapi::showQuestion(title = 'GITHUB access', message = 'Store GitHub PAT?', ok = "Yes", cancel = 'No')
  if(github) {
    credential_helper_set(helper = 'store', global = TRUE)
    credentials::set_github_pat(force_new = TRUE)
  }

  # set .Rprofile
  #
  rprofile <- rstudioapi::showQuestion(title = 'Rprofile', message = 'Set PAT each session?', ok = "Yes", cancel = 'No')
  if(rprofile) {
    set_rprofile()
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

  # login_yoda
  irods_login <- rstudioapi::showQuestion(title = 'Yoda', message = 'Test yoda login?', ok = "Yes", cancel = 'No')
  if (irods_login) {
    login_yoda()
  }

  #
}

