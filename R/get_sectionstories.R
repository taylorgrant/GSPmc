#' Get links from morningconsult sections
#'
#' @param ll
#'
#' @return tibble of links
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' link <- "morningconsult.com/coronavirus"
#' ss_links <- get_sectionstories(link)
#' }
get_sectionstories <- purrr::possibly(function(ll) {

  url_stem_reg <- "morningconsult\\.com"
  section_reg <- paste0(url_stem_reg, "/[0-9]+",
                        "/[0-9]+","/[0-9]+","/[a-z-0-9]+")
  fnll <- paste0("https://", ll)
  links_raw <- fnll %>%
    rvest::read_html() %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  links_tmp <- tibble::tibble(
    link_base = links_raw %>%
      stringr::str_extract_all(section_reg) %>%
      base::unlist()
  ) %>%
    dplyr::distinct(link_base) %>%
    tidyr::drop_na()

}, otherwise = NULL)
