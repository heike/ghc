#' Clone a set of github repos into a folder
#'
#' @param selected list of repos as returned from `get_org_repos`
#' @param dest_dir Destination directory (default: working directory)
#' @param post_fix user defined ending for folders, defaults to "-submissions"
#' @export
#' @examples
#' \dontrun{
#'   ghc("451", clone_folder)
#'   }
clone_folder <- function(selected, dest_dir, postfix = "submissions") {
  dest_dir <- paste0(dest_dir, postfix)
  if (!dir.exists(dest_dir)) dir.create(dest_dir)
  # clone directories
  sapply(selected, FUN = function(x) clone_github_repo(
    x$html_url,
    dest_dir = dest_dir
  ))
}
