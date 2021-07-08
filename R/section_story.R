section_story <- possibly(function(ll) {

  section_reg <- paste0(url_stem_reg, "/[0-9]+",
                        "/[0-9]+","/[0-9]+","/[a-z-0-9]+")
  fnll <- paste0("https://", ll)
  links_raw <- fnll %>%
    read_html() %>%
    html_nodes("a") %>%
    html_attr("href")

  links_tmp <- tibble(
    link_base = links_raw %>%
      str_extract_all(section_reg) %>%
      unlist()
  ) %>%
    distinct(link_base) %>%
    drop_na()

}, otherwise = NULL)
