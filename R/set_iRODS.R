#' Install irods icommands
#'
#' @param out_file Basename of file in which to write output of installation script
#' @param err_file Idem for errors.
#'
#' @details
#' The output and error files are stored in the working directory
#'
#' @note
#' Only works for Ubuntu x.x
#'
#' @export

install_icommands <- function(out_file = 'ii.out', err_file = 'ii.err') {
  if (is.null(out_file) || is.null(err_file)) {
    warning('Provide base names of output and error files, or use defaults')
    return(FALSE)
  }
  out <- file.path(getwd(), basename(out_file))
  err <- file.path(getwd(), basename(err_file))
  install_script <- system.file('linux', 'install_icommands.sh', package = 'RCrstudio')
  install_ok     <- system2(command = 'sh', args = install_script, stdout = out, stderr = err)
  if (install_ok != 0) {
    warning(sprintf('Install icommands not succesful. See %s', err))
    return(FALSE)
  }
  return(TRUE)
}


#' Set irods environment
#'
#' Writes the irods host and user configuration items in HOME/.irods/irods_environment.json` file.
#'
#' @param host Name (character string) of the organisation/department in data.uu.nl (e.g. 'i-lab.data.uu.nl')
#' @param zone Zone name (char string) of the irods organisation (e.g. 'nluu12p')
#' @param user Login name of user on irods host (e.g. j.jansen@uu.nl)
#' @param override Boolean (default FALSE) to indicate if it's allowed to override an existing irods environment
#'
#' @note Currently very UU specific
#'
#' @export
#'
set_irods_env <- function(host = NULL, zone = NULL, user = NULL, override = FALSE) {

  if (is.null(host) || is.null(zone) || is.null(user)) {
    warning('Host, user or zone is NULL; irods environment not set')
    return(FALSE)
  }

  irods_dir <- file.path(Sys.getenv('HOME'), '.irods')
  if (!dir.exists(irods_dir)) {
    dir.create(path = irods_dir, mode = '0700')
  }

  irods_file <- file.path(Sys.getenv('HOME'), '.irods', 'irods_environment.json')
  if (!override && file.exists(irods_file)) {
    warning('irods  environment already exists! Use override = TRUE')
    return(FALSE)
  }

  irods_env_tmpl <- '{
      "irods_host": "HOST",
      "irods_port": 1247,
      "irods_home": "/ZONE/home",
      "irods_user_name": "USER",
      "irods_zone_name": "ZONE",
      "irods_authentication_scheme": "pam",
      "irods_encryption_algorithm": "AES-256-CBC",
      "irods_encryption_key_size": 32,
      "irods_encryption_num_hash_rounds": 16,
      "irods_encryption_salt_size": 8,
      "irods_client_server_negotiation": "request_server_negotiation"
  }'

  irods_env <- stringr::str_replace_all(string = irods_env_tmpl, pattern = c('HOST' = host, 'ZONE' = zone, 'USER' = user))
  write(x = irods_env, file = irods_file, append = FALSE)
  Sys.chmod(paths = irods_file, mode = '0600')

  return(TRUE)
}

#' Login to iRods
#'
#' @param ttl Integer value indicating the hours the login has to be valid (time-to-live).
#' @param password Plain text password for irods, but never use literal string. See notes.
#'
#' @note Never use a literal password string in a script.
#' Instead use a function that asks the user to enter a password or
#' a function that retrieves the password from a save location (e.g. keychain)
#'
#' @return TRUE if login succeeded, otherwise FALSE
#'
#' @export
#'
login_irods <- function(ttl = 1L, password = NULL) {
  if(is.null(password)) {
    stop('No password')
  }
  init_args <- c(
    sprintf('--ttl %d', ttl),
    password
  )
  ret_val <- system2(command = 'iinit', args = init_args, stdout = FALSE, stderr = FALSE) # Linux returns 0 if command was succesful,
                                                                                          # otherwise non-zero
  login_succes <- ifelse(ret_val == 0, yes = TRUE, no = FALSE)
  if(!login_succes) {
    warning('Connecting to iRODS not successful')
  }
  return(login_succes)
}

