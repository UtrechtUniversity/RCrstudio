#' Login iRODS from an RStudio session
#'
#' @export
#'
rstudio_login_irods <- function() {

  if(!rstudioapi::isAvailable()) {
    stop('We are NOT in an RStudio session')
  }

  login_ok <- login_irods(password = rstudioapi::askForPassword(prompt = 'Enter your iRODS password: '))
  if (!login_ok) {
    rstudioapi::showDialog(title = 'iRODS', message = 'Connecting to iRODS was not successful')
    return(FALSE)
  }
  return(TRUE)
}

