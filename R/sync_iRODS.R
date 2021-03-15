#' Synchronize Project Input and Output Data
#'
#' Synchronize project data of a from/to an irods zone with an R/Rstudio environment on Ubuntu. E.g. the Research Cloud of SURFsara.
#' This function is a wrapper around `irsync` form `icommands`
#'
#' @param project Name (character string) of the project
#' @param stage Which files to stage? If stage = 'data' then files will be synced from irods to RC; if 'output' vice versa.
#' @param delete What to do with files in target directory that are  not in source collection anymore. See details.
#' @param ... Path to a (sub)collection as a comma separated series of unnamed parameter values (strings). See details.
#'
#' @return
#' If delete is FALSE, a list of files in target that aren't in source collection anymore. If TRUE list of removed files.
#'
#' @details
#' On the irods side we expect the project folder in the home directory as defined in current irods_environment.json file.
#' On the R side the project folder lives in the working directory. On both sides the project folders must have the same name.
#'
#' The data/output convention is followed.
#' If stage = 'data' then (input) data from irods will be transfered to the R/RStudio environment.
#' Irods is then the source and RStudio the target.
#' In both project folders there must be a directory named 'data'
#' If stage = 'output' (meaning output data), then the transfer is from RStudio to irods.
#'
#' The applied sync of icommands is one way from source to target.
#' This means that files in the target which aren't in the source (anymore).
#' The question is what to do with those files?
#' If delete = FALSE (the default), then these files aren't removed,
#' but only a list of filenames is returned to the calling function.
#' If delete = TRUE the files are also removed, but, currently, the empty folders are not remnoved.
#' We strongly advise to only produce a list of files and do clean-up afterwards.
#'
#' If input or output data are organized in collections and subcollections,
#' the user can add a series of unnamed, comma separated string parameters (...) to specify the path to the (sub) collection.
#' For instance 'collect_A', 'sub_B', 'sub_sub_C' for irods_home/project_name/data/collect_A/sub_B/sub_sub_C.
#'
#' This function doesn't perform an `iinit`, but it checks if there is a running irods session.
#'
#' @export
#'
sync_irods  <- function(project = NULL, stage = c('data', 'output'), delete = FALSE, ...) {

  # read irods home from json file
  #
  irods_env_json <- file.path(Sys.getenv('HOME'), '.irods', 'irods_environment.json')
  if(!file.exists(irods_env_json)) {
    stop(sprintf('No irods environment file %s', irods_env_json))
  }
  irods_home <- jsonlite::fromJSON(txt = irods_env_json)$irods_home

  # check if icommands are initialized
  #
  irods_not_connected <- system2('ils', stdout = FALSE, stderr = FALSE)
  if (irods_not_connected) {
    stop('No irods connection')
  }

  # What is the sync direction
  #
  if (is.null(stage) || length(stage) > 1 || !stage %in% c('data', 'output')) {
    stop('type of staging must be data or output')
  }

  # construct collection path out of irods home, project name and (sub)collection names (...)
  #
  if (is.null(project)) {
    stop('Project name is NULL')
  }
  irods_path <- file.path(irods_home, project, stage, ...)

  # test existence of (sub)collection in irods
  #
  if(stage == 'data') {
    irods_value <- system2(command = 'icd', args = irods_path, stdout = FALSE, stderr = FALSE)
  } else {
    irods_value <- system2(command = 'icd', args = dirname(irods_path), stdout = FALSE, stderr = FALSE) # irsync can create last sub collection
  }
  if (irods_value != 0) {
    stop(sprintf('%s: Irods collection does not exists', irods_path))
    return(FALSE)
  }

  # construct the path of the project folder on Research Cloud
  #
  if (basename(getwd()) != project) {
    stop(sprintf("%s: working directory not project directory", getwd()))
  }

  rc_path <- file.path(getwd(), stage, ...)

  if (stage == 'output') {
    dir_exists <- dir.exists(rc_path)
  } else {
    dir_exists <- dir.exists(dirname(rc_path))       # irsync can create the last subfolder!
  }
  if (!dir_exists) {
    stop(sprintf("%s: local collection doesn't exists", rc_path))
  }

  # make irsync command
  #
  if (stage == 'data') {
    source      <- sprintf("i:%s", irods_path)
    destination <- rc_path
  } else {
    source      <- rc_path
    destination <- sprintf("i:%s", irods_path)
  }
  irsync_args  <- c('-r', source, destination)
  irsync_value <- system2(command = 'irsync', args = irsync_args, stdout = FALSE, stderr = FALSE)
  if (irsync_value != 0) {
    stop(sprintf("Error while executing: 'irsync %s'", irsync_args))
  }
  print('LEFTOVERS')

  # look for left-overs in destination
  #
  irsync_args <- c('-r', '-l', destination, source)
  irsync_list <- system2(command = 'irsync', args = irsync_args, stdout = TRUE, stderr = TRUE) # Find left overs by a dry run of a reverse sync
  if(length(irsync_list) > 1) {
    left_overs  <- get_filenames_irsync(irsync_list)
  } else {
    left_overs <- NULL
  }

  if (delete && (!is.null(left_overs))) {
    print('DELETE')
    if(stage == 'data') {
      lo <- file.remove(left_overs)
      print(lo)
    } else {
      arg_files   <- paste(left_overs, collapse = '')
      irods_error <- system2('irm', args)
      if (irods_error) {
        warning('irods: not all files deleted')
      }
    }
  }

  return(left_overs)
}

#' Get Filenames from irsync list
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
  l <- purrr::map_chr(.x = irsync_list, .f = function(x) stringr::str_split(string = x, pattern = '   ')[[1]][[1]])
  l
}

construct_file_path <- function(...) {
  file.path(getwd(), ...)
}


