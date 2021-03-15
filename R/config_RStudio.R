#' Configure an RStudio Workspace in the Research Cloud
#'
#' R script for configuring GitHub and iRODS connections from within an RStudio session.
#' Set a GitHub PAT
#' Install icomcands, create an irods_environment
#' Check connection
#'
#'
#' @export
config_workspace <- function() {

  if(!rstudioapi::isAvailable()) {
    stop('We are NOT in an RStudio session')
  }

  github <- rstudioapi::showQuestion(title = 'GITHUB', message = 'Store GitHub PAT?', ok = "Yes", cancel = 'No')
  if (github) {
    if(!store_github_pat()) {
      rstudioapi::showDialog(title = 'GitHub', message = 'PAT was not set')
    }
  }

  # install icommands
  #
  icommands <- rstudioapi::showQuestion(title = 'iRODS', message = 'Install iRODS icommands?', ok = "Yes", cancel = 'No')
  if (icommands) {
    install_ok <- install_icommands()
    if(!install_ok) {
      rstudioapi::showDialog(title = 'iRODS', message = 'Installation of icommands was not successful')
    }
  }

  # install irods environment on user level
  #
  irods_user <- rstudioapi::showQuestion(title = 'iRODS', message = 'Install iRODS environment?', ok = "Yes", cancel = 'No')
  if (irods_user) {
    host <- rstudioapi::showPrompt(title = 'YODA', message = 'host: ', default = 'its.data.uu.nl')
    zone <- rstudioapi::showPrompt(title = 'YODA', message = 'zone: ', default = 'nluu12p')
    user <- rstudioapi::showPrompt(title = 'YODA', message = 'name: ', default = 'j.jansen@uu.nl')

    set_ok <-set_irods_env(host = host, zone = zone, user = user, override = TRUE)
    if(!set_ok) {
      rstudioapi::showDialog(title = 'iRODS', message = 'Setting of iRODS environment was not successful')
    }
  }

  # check connection
  #
  irods_login <- rstudioapi::showQuestion(title = 'iRODS', message = 'Test iRODS connection?', ok = "Yes", cancel = 'No')
  if (irods_login) {
    login_ok <- rstudio_login_irods()
  }
}

