.onAttach <- function(...) {
  if(requireNamespace("gagglr", quietly = TRUE))
    gagglr::oha_check("cascade", suppress_success = TRUE)
}
