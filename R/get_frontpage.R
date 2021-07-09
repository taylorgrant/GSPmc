#' Get links in news section of morningconsult.com
#'
#' Story links from `news` section
#' @return list of tibbles - main page story links and links to page sections
#' @export
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' links <- get_frontpage()
#' }
get_frontpage <- function() {

  base_url <- "https://morningconsult.com/news"

  links_raw <- base_url %>%
    rvest::read_html() %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  # 1. get story articles links listed on homepage #
  url_stem_reg <- "morningconsult\\.com"
  story_reg <- paste0(url_stem_reg, "/[0-9]+","/[0-9]+","/[0-9]+","/[a-z-0-9]+")

  storylinks <- tibble::tibble(
    link_base = links_raw %>%
      stringr::str_extract_all(story_reg) %>%
      base::unlist()
  ) %>%
    dplyr::distinct(link_base) %>%
    tidyr::drop_na()

  # 2. get section headers #
  section_reg <- paste0(url_stem_reg, "/[a-z-0-9]+")

  sectionlinks <- tibble::tibble(
    link_base = links_raw %>%
      stringr::str_extract_all(section_reg) %>%
      base::unlist()
  ) %>%
    dplyr::distinct(link_base) %>%
    tidyr::drop_na() %>%
    dplyr::filter(!stringr::str_detect(basename(link_base), # drop basename starting numeric
                       stringr::regex("^[0-9+]", ignore_case = TRUE))) %>%
    dplyr::filter(!basename(link_base) %in% c("company", "product", "privacy-policy",
                                       "security", "terms-and-conditions=of-use",
                                       "form", "subscribe"))
  links <- list(storylinks = storylinks,
                sectionlinks = sectionlinks)

}
