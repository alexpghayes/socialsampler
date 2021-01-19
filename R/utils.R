#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

empty_edgelist <- function() {
  tibble::tibble(from = character(), to = character())
}

utils::globalVariables(
  c(
    "app",
    "attempts",
    "friends",
    "limit",
    "node",
    "remaining",
    "token",
    "user_id",
    "users"
  )
)
