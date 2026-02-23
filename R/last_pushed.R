#' Order a set of repos by most recent change
#'
#' @param selected list of repos as returned from `get_org_repos`
#' @param dest_dir Destination directory (default: working directory)
#' @return data frame of repositories arranged by date and time of the last push
#' @importFrom dplyr select
#' @export
last_pushed <- function(selected, dest_dir) {
  #  browser()
  dframe <- data.frame(
    name = selected |> purrr::map_chr(.f = function(x) x$"name"),
    pushed_at = selected |> purrr::map_chr(.f = function(x) x$"pushed_at"))
  dframe |>
    mutate(
      pushed_at = lubridate::as_datetime(pushed_at)
    ) |> arrange(desc(pushed_at)) |>
    select(name, pushed_at)
}


