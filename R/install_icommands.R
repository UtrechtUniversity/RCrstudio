#' Install irods icommands
#'
#'
#' @export

install_icommands <- function() {
  install_script <- system.file('linux', 'install_icommands.sh', package = 'RCrstudio')
  system2(command = 'sh', args = install_script, stdout = 'ii.out', stderr = 'ii.err')
}

#' Inlog Yoda
#'
#' @param ttl Integer value indicating the hours the login has to be valid (time-to-live)
#'
#' @export
#'
login_yoda <- function(ttl = 1L) {
  #iinit_cmd <- sprintf('iinit --ttl %d', as.integer(ttl))
  rstudioapi::terminalExecute(command = 'iinit -l')
}
