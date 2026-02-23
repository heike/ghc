#' Get all repositories for a GitHub organization
#'
#' @param org Character string. GitHub organization name.
#' @param visibility One of "all", "public", or "private".
#' @param type One of "all", "sources", "forks", or "member".
#' @param per_page Number of results per page (max 100).
#' @return A list of repository objects as returned by the GitHub API.
#' @export
#' @examples
#' repos <- get_org_repos("stat451-at-unl")
#' length(repos)
get_org_repos <- function(org,
                          visibility = "private",
                          type = "all",
                          per_page = 100) {
  gh::gh(
    "/orgs/{org}/repos",
    org = org,
    visibility = visibility,
    type = type,
    per_page = per_page,
    .limit = Inf
  )
}


#' Longest common substring across multiple strings
#'
#' @param x Character vector (length >= 2)
#' @return Character string: longest common substring
#' @examples
#' longest_common_substring(c("statistics", "states", "taste"))
#' longest_common_substring(c("abcdef", "zabcf", "tabcxy"))
longest_common_substring <- function(x) {
  stopifnot(is.character(x), length(x) >= 2)

  # Use the shortest string to minimize search space
  x <- x[order(nchar(x))]
  s <- x[[1]]
  others <- x[-1]

  n <- nchar(s)

  # Try substrings from longest to shortest
  for (len in seq(n, 1)) {
    for (start in seq_len(n - len + 1)) {
      sub <- substr(s, start, start + len - 1)
      if (all(vapply(others, grepl, logical(1), pattern = sub, fixed = TRUE))) {
        return(sub)
      }
    }
  }

  ""
}

#' Longest common sprefix
#'
#' @param x Character vector (length >= 1)
#' @return Character string giving the longest common prefix
#' @examples
#' longest_common_prefix(c("statistics", "state", "station"))
#' longest_common_prefix(c("apple", "banana"))
longest_common_prefix <- function(x) {
  stopifnot(is.character(x), length(x) > 0)

  if (length(x) == 1) {
    return(x)
  }

  # Use the shortest string to limit comparisons
  x <- x[order(nchar(x))]
  s <- x[[1]]
  others <- x[-1]

  n <- nchar(s)
  prefix_len <- 0

  for (i in seq_len(n)) {
    candidate <- substr(s, 1, i)
    if (all(startsWith(others, candidate))) {
      prefix_len <- i
    } else {
      break
    }
  }

  substr(s, 1, prefix_len)
}

#' Pairwise distance matrix for strings
#'
#' Wrapper for the `stringdist::stringdist` function that calculates
#' distance between a pair of characters
#' @param x character vector
#' @param method name of the method
#' @param ... additional parameters passed on to the method
#' @return numeric matrix of distances
#' @examples
#' distance_matrix(c("statistics", "states", "status"), method="jw")
distance_matrix <- function(x, method, ...) {
  stopifnot(is.character(x))

  n <- length(x)
  out <- matrix(0, n, n, dimnames = list(x, x))

  for (i in seq_len(n)) {
    for (j in i:n) {
      d <- stringdist::stringdist(x[i], x[j],
                                  method = method,
                                  ...)
      out[i, j] <- d
      out[j, i] <- d
    }
  }

  out
}






#gh_org(course = "451", f = clone_folder)

#gh_org(course = "451", f = last_pushed)
