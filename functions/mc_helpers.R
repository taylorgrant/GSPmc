# helper functions to get Morning Consult survey crosstabs #


# -------------------------------------------------------------------------
## Get all article links available on Morning Consult ##

# function extracts all story links on homepage
# then crawls across each section and extracts
# story links from each section

extract_links <- function() {

  # 1. load packages
  pacman::p_load(tidyverse, janitor, here, glue, rvest)

  base_url <- "https://morningconsult.com/news"

  # get all links on news page
  links_raw <- base_url %>%
    read_html() %>%
    html_nodes("a") %>%
    html_attr("href")

  # 2. get story articles links listed on news page #
  url_stem_reg <- "morningconsult\\.com"
  story_reg <- paste0(url_stem_reg, "/[0-9]+","/[0-9]+","/[0-9]+","/[a-z-0-9]+")

  # extract all urls with date and slug (%Y/%m/%d)
  storylinks <- tibble(
    link_base = links_raw %>%
      str_extract_all(story_reg) %>%
      unlist()
  ) %>%
    distinct(link_base) %>%
    drop_na()

  # 3a. get section headers #
  section_reg <- paste0(url_stem_reg, "/[a-z-0-9]+")

  sectionlinks <- tibble(
    link_base = links_raw %>%
      str_extract_all(section_reg) %>%
      unlist()
  ) %>%
    distinct(link_base) %>%
    drop_na() %>%
    filter(!str_detect(basename(link_base), # drop basename starting numeric
                       regex("^[0-9+]", ignore_case = TRUE))) %>%
    filter(!basename(link_base) %in% c("company", "product", "privacy-policy",
                                       "security", "terms-and-conditions=of-use",
                                       "form", "subscribe"))

  # 3b. get story links within each section #
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

  sec_story_links <- pull(sectionlinks) %>%
    map(section_story) %>%
    discard(is.null) %>%
    reduce(bind_rows)

  # 4. combine all story links #
  all_links <- bind_rows(storylinks, sec_story_links) %>%
    distinct(link_base) %>%
    mutate(link_base = paste0("https://", link_base))

}


# -------------------------------------------------------------------------

# Morning Consult surveys are frequently linked in stories
# and they are pdf's. We want those links along with the
# links to the stories that provide context about them

extract_crosstab_links <- possibly(function(l) {

  get_pdf <- function(l) {
    l %>%
      read_html() %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      as_tibble() %>%
      drop_na() %>%
      filter(str_detect(value, ".pdf")) %>%
      distinct(value) %>%
      pull(value)
  }
  p <- get_pdf(l)

  # story link and crosstab link in tibble
  # if p is empty, the whole tibble is empty
  tmpout <- tibble(story_link = l,
                   pdf_link = p)
}, otherwise = NA)


# -------------------------------------------------------------------------

# get date of each story article
extract_date <- function(x) {
  story_date <- lubridate::ymd(sub("/[^/]+$", "", urltools::url_parse(x)$path))
}

# -------------------------------------------------------------------------

# get the filenumber of the survey
extract_filenumber <- function(x) {
  paste0("mc", gsub("\\_.*", "", sub(".*[/]", "", x)))
}

# -------------------------------------------------------------------------

## check to see if pdf already processed; if not, put into new folder ##
pdf_download <- possibly(function(l, f) {

  # check to see if previously processed #
  destfile <- glue(here("data", "mc_crosstabs", "processed", "{f}.pdf"))
  newfile <- glue(here("data", "mc_crosstabs", "new", "{f}.pdf"))

  if (!file.exists(destfile)) {
    download.file(l, newfile, quiet = FALSE)
  }
}, otherwise = NULL)

# -------------------------------------------------------------------------



