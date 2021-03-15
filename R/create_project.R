#' Create Project in R Workspace
#'
#' @param project_name Name (string) of the project
#' @param git Do we want version control (default TRUE)
#' @param github Do we want an upstream/origin (default TRUE)
#'
create_ws_project <- function(project_name = NULL, git = TRUE, github = TRUE) {
  if(!git && github) {
    warning("Can't have GitHUb without Git!")
    return(FALSE)
  }
  if (is.null(project_name)) {
    warning('Project must have a name')
    return(FALSE)
  }

  # create an R project in working directory
  #
  project_path <- file.path(getwd(), project_name)
  usethis::create_project(path = project_path, open = FALSE)

  # use git
  #
  if (git) {
    setwd(project_path)
    usethis::use_git()
  }

  # use github
  #
  gh::gh("POST /user/repos", name = project_name)
  # git
  # git remote add origin https://github.com/KvEijden/foobar.git
  # git branch -M main
  # git push -u origin main



}
