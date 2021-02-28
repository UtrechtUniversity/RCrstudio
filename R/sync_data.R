#' Synchronize Project Data
#'
#' Synchronize input data of a project from projects data store in Yoda
#' to the projects data store in Research Cloud
#'
#' @param project Name (character string) of the project
#' @param check Type of check to perform before synchronizing. See Details
#' @param delete What to do with files in target directory which are  not in source collection anymore.
#' If FALSE (default) those files remain in target directory, else they are removed.
#' @param ... path to a subcollection within collection
#'
#' @return
#' If delete is FALSE, a list of files in target which aren't in source collection anymore. If TRUE list of removed files.
#' @export
#'
sync_data  <- function(project = NULL, collection = NULL, delete = FALSE, ...) {

  # read Yoda home from json file
  irods_env_json <- file.path(Sys.getenv('HOME'), '.irods', 'irods_environment.json')
  if(!file.exists(irods_env_json)) {
    stop(sprintf('No irods environment file %s', irods_env))
  }
  yoda_home <- fromJSON(txt = irods_env_json)$irods_home

  # check if icommands are initialized
  #
  irods_not_connected <- system2('ils', stdout = FALSE, stderr = FALSE)
  if (irods_not_connected) {
    stop('No irods connection')
  }

  # construct collection path out of yoda home, project name and subproject name
  if (is.null(project)) {
    stop('Project name is NULL')
  }
  collection_path <- file.path(yoda_home, project, 'data')
  if(!is.null(collection)) {
    collection_path <- file.path(collection_path, collection, ...)
  }

  # test existence of data collection in Yoda
  irods_value <- system2(command = 'icd', args = collection_path, stdout = FALSE, stderr = FALSE)
  if (irods_value != 0) {
    stop(sprintf('%s: Yoda collection does not exists', collection_path))
    return(FALSE)
  }

  # construct the path of the projects data folder on Research Cloud
  #
  local_project<- basename(getwd())
  if (local_project != project) {
    stop("%s: working directory not project directory", getwd())
  }

  local_data <- file.path(getwd(), 'data')
  if (!is.null(collection)) {
    local_data <- file.path(local_data, collection, ...)
  }
  if (!dir.exists(local_data)) {
    stop(sprintf("%s: local data collection doesn't exists", local_data))
  }

  # make irsync command
  source      <- sprintf("i:%s", collection_path)
  destination <- local_data
  irsync_args <- c('-r', source, destination)
  irsync_value <- system2(command = 'irsync', args = irsync_args, stdout = FALSE, stderr = FALSE)
  if (irsync_value != 0) {
    stop(sprintf("Error while executing: 'irsync %s'", irsync_args))
  }

  # look for left-overs in destination
  #
  irsync_args <- c('-r', '-l', destination, source)
  irsync_list <- system2(command = 'irsync', args = irsync_args, stdout = TRUE, stderr = TRUE) # Find left overs by a dry run of a reverse sync
  if(length(irsync_list) > 1) {
    left_overs  <- get_filenames_irsync(irsync_list)
  } else {
    left_overs <- NULL
  }

  if (delete && !is.null(left_overs)) {
    file.remove(left_overs)
  }

  return(left_overs)

  #

  # execute irsync

  # check irsync results

}

#' Get Filenames from irsync List
#'
#' @details
#' Each line (except first) has three parts seperated by 3 spaces: filename, byte count, and ?
#  The first line is a general message and can be discarded
#'
#'
#' @param irsync_list Output text (vector of strings) of \emph{irsync -l}
#'
get_filenames_irsync <- function(irsync_list) {
  irsync_list <- irsync_list[2:length(irsync_list)]

  # get first part of each line in a list and convert to vector
  #
  l <- map_chr(.x = irsync_list, .f = function(x) str_split(string = x, pattern = '   ')[[1]][[1]])
  l
}

construct_file_path <- function(...) {
  file.path(getwd(), ...)
}
