#' Order a set of repos by most recent change
#'
#' @param selected list of repos as returned from `get_org_repos`
#' @param dest_dir Destination directory (default: working directory)
#' @param post_fix user defined ending for folders, defaults to "-submissions"
#' @return data frame of repositories arranged by date and time of the last push
#' @export
pull_folder <- function(selected, dest_dir, postfix = "submissions") {
  dest_dir <- paste0(dest_dir, postfix)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  if (!dir.exists(dest_dir)) {
    stop(sprintf("<%s> does not exist in <%s>", dest_dir, getwd()))
  }
  setwd(dest_dir)

  for (i in 1:length(selected)) {
    if (!dir.exists(selected[[i]]$name)) {
      stop(sprintf("<%s> does not exist in <%s>", selected[[i]]$name, dest_dir))
    }
    setwd(selected[[i]]$name)
    system("git stash") # remove any existing changes
    # Remove untracked files/directories that may block checkout
    system("git clean -fd")
    # Pull changes
    pull_status <- system(
      sprintf("git pull")
    )
    print(pull_status)
    setwd("..")
  }


}
