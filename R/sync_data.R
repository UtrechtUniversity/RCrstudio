#' Synchronize Project Data
#'
#' Synchronize input data of a project from projects data store in Yoda
#' to the projects data store in Research Cloud
#'
#' @param project Name (character string) of the project
#' @param check Type of check to perform before synchronizing. See Details
#' @param delete What to do with files in target not in source anymore. If FALSE (default) those files remain in target unchanged.
#' If TRUE they are removed.
#'
#' @export
#'
sync_data  <- function(project = NULL, sub = NULL, delete = FALSE, check = 'hash') {

  # read Yoda home from json file
  irods_env_json <- file.path(Sys.getenv('HOME'), '.irods', 'irods_environment.json')
  if(!file.exists(irods_env_json)) {
    warning(sprintf('No irods environment file %s', irods_env))
    return(FALSE)
  }
  yoda_home <- fromJSON(txt = irods_env_json)$irods_home

  # check if icommands are initialized
  #
  irods_not_connected <- system2('ils', stdout = FALSE, stderr = FALSE)
  if (irods_not_connected) {
    warning('No irods connection')
    return(FALSE)
  }

  # construct collection path out of yoda home, project name and subproject name
  if (is.null(project)) {
    warning('Project name is NULL')
    return(FALSE)
  }
  collection_path <- file.path(yoda_home, project, 'data')
  if(!is.null(sub)) {
    collection_path <- file.path(collection_path, sub)
  }

  # test existence of data collection in Yoda
  irods_value <- system2(command = 'icd', args = collection_path, stdout = FALSE, stderr = FALSE)
  if (irods_value != 0) {
    warning(sprintf('%s: Yoda collection does not exists', collection_path))
    return(FALSE)
  }

  # construct the path of the projects data folder on Research Cloud
  #
  local_project<- basename(getwd())
  if (local_project != project) {
    warning("%s: working directory not project directory", getwd())
    retrun(FALSE)
  }

  local_data <- file.path(getwd(), 'data')
  if (!is.null(sub)) {
    local_data <- file.path(local_data, sub)
  }
  if (!dir.exists(local_data)) {
    warning(sprintf("%s: local data collection doesn't exists", local_data))
  }

  # make irsync command
  source      <- sprintf("i:%s", collection_path)
  destination <- local_data
  irsync_args <- c('-r', source, destination)
  #print(sprintf('irsync %s %s %s %s', irsync_args[1], irsync_args[2], irsync_args[3], irsync_args[4]))
  irsync_value <- system2(command = 'irsync', args = irsync_args, stdout = FALSE, stderr = FALSE)
  if (irsync_value != 0) {
    warning(sprintf("Error while executing: 'irsync %s'", irsync_args))
    return(FALSE)
  }

  if (!delete) {
    return(TRUE)
  }

  # look for left-overs in destination
  #
  irsync_args  <- c('-r', '-l', destination, source)
  irsync_list <- system2(command = 'irsync', args = irsync_args, stdout = TRUE, stderr = TRUE)
  if(length(irsync_list) > 1) {
    left_overs  <- get_left_overs(irsync_list)
  }




  return(irsync_value)

  #

  # execute irsync

  # check irsync results

}

get_left_overs <- function(irsync_list) {
  l <- map(.x = out, .f = function(x, pattern) str_split(string = x, pattern = pattern)[[1]], pattern = '   ')
  m <- reduce(.x = l, .f = function(x, y) c(x, y[[1]]))

}
