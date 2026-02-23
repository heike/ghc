#' Clone a GitHub repository into the working directory
#'
#' @param html Repository url
#' @param dest_dir Destination directory (default: working directory)
#' @export
clone_github_repo <- function(html,
                              dest_dir = getwd()) {

  repo_url <- sprintf("%s.git", html)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(dest_dir)

  cmd <- sprintf("git clone %s", repo_url)

  system(cmd)
}
