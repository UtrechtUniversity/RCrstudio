#' Install irods icommands
#'
#'
#' @export

install_icommands <- function() {
  install_script <- system.file('linux', 'install_icommands.sh', package = 'RCrstudio')
  system2(command = 'sh', args = install_script, stdout = 'ii.out', stderr = 'ii.err')
}
