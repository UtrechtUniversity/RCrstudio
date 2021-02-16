#' Set irods environment
#'
#' @param host Name (character string) of the organisation/department in data.uu.nl (e.g. 'i-lab')
#' @param zone Zone name (char string) of the irods organisation (e.g. 'nluu12p')
#' @param user Name of user; part before @@uu.nl
#' @param override Boolean (default FALSE) to indicate if it's allowed to override an existing irods environment file
#'
#' @export
#'
set_irods_env <- function(host = NULL, zone = NULL, user = NULL, override = FALSE) {
  if(is.null(host) && rstudioapi::isAvailable()) {
    host <- rstudioapi::showPrompt(title = 'YODA', message = 'host: ')
  }
  if(is.null(zone) && rstudioapi::isAvailable()) {
    zone <- rstudioapi::showPrompt(title = 'YODA', message = 'zone: ')
  }
  if(is.null(user) && rstudioapi::isAvailable()){
    user <- rstudioapi::showPrompt(title = 'YODA', message = 'name: ')
  }
  if (is.null(host) || is.null(zone) || is.null(user)) {
    print('Host, user or zone is NULL; irods environment not set')
    return(FALSE)
  }
  irods_dir <- file.path(Sys.getenv('HOME'), '.irrods')
  if (!dir.exists(irods_dir)) {
    dir.create(path = irods_dir, mode = '0700')
  }
  irods_file <- file.path(Sys.getenv('HOME'), '.irrods', 'irods_environment.json')
  if (!override && file.exists(irods_file)) {
    print('irods  environment already exists! Use override = TRUE')
    return(FALSE)
  }

  irods_env_tmpl <- '{
      "irods_host": "HOST.data.uu.nl",
      "irods_port": 1247,
      "irods_home": "/ZONE/home",
      "irods_user_name": "USER@uu.nl",
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
}

