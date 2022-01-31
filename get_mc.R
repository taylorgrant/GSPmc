## Get pdf crosstabs from Morning Consult, download the pdf crosstabs,
## read them in, and grab the table of contents...We also want the
## stories that are written along with the crosstabs for extra information...
## Finally, everything will be compiled into an html table

# load packages and helper functions #
pacman::p_load(tidyverse, janitor, here, glue, rvest)
ff <- list.files(here::here("functions"), full.names = TRUE)
walk(ff, source)


# running data with story links, pdf links, survey date, who was surveyed, etc
# make sure we're not downloading any duplicate pdfs
indx <- readRDS(here("mc_index.rds"))
mc_table <- readRDS(here("mc_TOC.rds"))


# GET LINKS TO ALL STORIES AND PDFS ---------------------------------------

# get the links to all stories
tmp <- extract_links()

# keep links we don't have yet
tmp <- tmp %>%
  anti_join(indx, by = c("link_base" = "story_link"))

# get links to all pdf files
pdflinks <- tmp$link_base %>%
  map(extract_crosstab_links)

# keep the "crosstab" pdf links
pdflinks <- pdflinks %>%
  reduce(bind_rows) %>%
  filter(str_detect(pdf_link, "crosstab"))

# add the story date and filenumber to the tibble
pdflinks <- pdflinks %>%
  mutate(story_date = extract_date(story_link),
         filenumber = extract_filenumber(pdf_link))

## add these to the file index and SAVE ##
pdflinks <- pdflinks %>%
  bind_rows(indx) %>%
  distinct(story_link, .keep_all = TRUE)

# save the rds file as the index
saveRDS(pdflinks, here("mc_index.rds"))

# sometimes pdfs are at dead links, so we'll use possibly #
try_pdf_download <- possibly(pdf_download, otherwise = NULL)

walk2(pdflinks$pdf_link, pdflinks$filenumber, try_pdf_download)

# READ IN PDFS  -----------------------------------------------------------

# identify the folder with unprocessed pdfs
folder <- here('data', 'mc_crosstabs', 'new')
file_list <- list.files(path=folder, pattern="*.pdf")

table_list <- file_list %>%
  map(pdf_toc)

table_df <- table_list %>%
  reduce(bind_rows) %>%
  ungroup()

# BREAKOUT TEXT AND KEYWORDS ----------------------------------------------

## break out keywords ##
library(udpipe)
library(textrank)

# load udpipe model for POS tagging
udpipe::udpipe_download_model(language = "english",
                              model_dir = here(),
                              overwrite = FALSE)
ud_model <- udpipe::udpipe_load_model(list.files(path = here(),
                                                 pattern = "*.udpipe",
                                                 full.names = TRUE))
mc_POS <- udpipe_annotate(ud_model, x = table_df$question,
                          doc_id = paste0(table_df$survey_file, "-", table_df$V1))
mc_POS <- as.data.frame(mc_POS)

# not using textrank b/c it relies on larger corpus rather than sentence
keywords <- mc_POS %>%
  group_by(doc_id) %>%
  distinct(lemma, upos) %>%
  filter(upos %in% c("NOUN", "ADJ", "PROPN", "SYM")) %>%
  mutate(keywords = str_trim(paste0(lemma, collapse = ", "))) %>%
  select(doc_id, keywords) %>%
  distinct(doc_id, .keep_all = TRUE)

table_final <- table_df %>%
  ungroup %>%
  mutate(doc_id = paste0(table_df$survey_file, "-", table_df$V1)) %>%
  left_join(keywords, by = c("doc_id"))

mc_questions <- pdflinks %>%
  inner_join(table_final, by = c("filenumber" = "survey_file")) %>%
  arrange(desc(story_date)) %>%
  select(-doc_id)

## add to old file and resave
mc_questions <- mc_questions %>%
  bind_rows(mc_table) %>%
  distinct(story_link, filenumber, V1, .keep_all = TRUE)

saveRDS(mc_questions, here::here("mc_TOC.rds"))


# RENDER TOC AS HTML FILE -------------------------------------------------
render_mc_toc()
