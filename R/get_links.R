#' Get all links from morningconsult.com
#'
#' @return tibble of all story links
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' links <- get_links()
#' }
get_links <- function() {
  tmp <- get_frontpage()

  section_links <- dplyr::pull(tmp$sectionlinks) %>%
    purrr::map(get_sectionstories) %>%
    purrr::discard(is.null) %>%
    purrr::reduce(dplyr::bind_rows)

  ## combine all story links
  all_links <- dplyr::bind_rows(tmp$storylinks, section_links) %>%
    dplyr::distinct(link_base) %>%
    dplyr::mutate(link_base = paste0("https://", link_base))
}
