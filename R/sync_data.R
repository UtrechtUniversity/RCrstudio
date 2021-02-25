#' Synchronize Project Data
#'
#' Synchronize input data of a project from projects data store in Yoda
#' to the projects data store in Research Cloud
#'
#' @param project Name (character string) of the project
#' @param check Type of check to perform before synchronizing. See Details
#'
#' @export
#'
sync_data  <- function(project = 'research-keestest', sub = NULL, check = 'hash') {

  # read Yoda home from json file
  irods_env_json <- file.path(Sys.getenv('HOME'), '.irods', 'irods_environment.json')
  if(!file.exists(irods_env_json)) {
    warning(sprintf('No irods environment file %s', irods_env))
    return(FALSE)
  }
  yoda_home <- fromJSON(txt = irods_env_json)$irods_home

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
  print(collection_path)




  # construct the path of the projects data folder on Research Cloud

  # check if icommands are initialized

  # check existence of collection

  # make irsync command

  # test irsync command

  # execute irsync

  # check irsync results

}
