## get the TOC for each pdf using pdfdtools
pdf_toc <- function(file) {

  # this portion of the function gets the pages with the TOC
  library(pdftools)
  dat <- glue(here('data', 'mc_crosstabs', 'new', file))

  # read in the pdf
  tmptxt <<- pdf_text(dat)

  ## move files after they'e been read in ##
  file.copy(from = here::here('data', 'mc_crosstabs', 'new', file),
            to = glue::glue("~/Library/CloudStorage/Box-Box/Morning Consult Surveys/surveys/{file}"))
  unlink(glue::glue(here::here('data', 'mc_crosstabs', 'new', file)))

  ## which pages are table of contents
  pg_start <- which(str_detect(tmptxt, "Table Index"))
  pg_stop <- which(str_detect(tmptxt, "Summary Statistics"))[1]
  pages <- pg_start:pg_stop

  ## function to pull in the questions in the TOC ##
  read_toc <- possibly(function(page) {

    ## which page to read in ##
    tmp_pg <- tmptxt[page]
    ## split on the \n character ##
    tmp_table <- str_split(tmp_pg, "\n", simplify = TRUE)

    # identify where to start
    tbl_start <- if (identical(which(str_detect(tmp_table, "Table Index")), integer(0))) {
      dim(tmp_table)[1]
    } else {
      which(str_detect(tmp_table, "Table Index"))
    }
    tbl_stop <- if (identical(which(str_detect(tmp_table, "Summary Statistics")), integer(0))) {
      dim(tmp_table)[2]-1 # if no summary stat, 2 empty lines
    } else {
      which(str_detect(tmp_table, "Summary Statistics"))
    }

    # set the table
    tbl1 <- tmp_table[,(tbl_start+1):(tbl_stop-1)]
    tbl1 <- str_trim(tbl1) # trim
    # add delim for colon only on first line (with question number and Table)
    tbl1 <- ifelse((str_detect(tbl1, '^\\d+') & str_detect(tbl1, "Table")),
                   str_replace(tbl1, "\\:", "|"), tbl1)
    tbl1 <- ifelse((str_detect(tbl1, '^\\d+') & str_detect(tbl1, "Table")),
                   str_replace_all(tbl1, "^\\d+", ""), tbl1)
    tbl1 <- str_replace_all(tbl1, "^[0-9A-Za-z]|^[[:punct:]]|^\\$", paste0("|", str_sub(tbl1,1,1))) # add delim if no number to start line

    tbl1 <- str_replace_all(tbl1, "\\d+$", "") # drop leading or tailing digits
    tbl1 <- str_trim(tbl1) # trim

    text_con <- textConnection(tbl1)
    data_table <- read.csv(text_con, sep = "|", header = FALSE) %>%
      mutate(V1 = na_if(V1, "")) %>% # if empty, then NA
      fill(V1, .direction = "down") %>% # fill
      group_by(V1) %>%
      mutate(question = str_trim(paste0(V2, collapse = " "))) %>%
      mutate(question = str_replace_all(question, " \\.", ""),
             question = str_replace_all(question, "- ", "")) %>%
      select(-V2) %>%
      distinct(V1, .keep_all = TRUE) %>%
      mutate(survey_file = gsub("\\..*", "", file))

    # get respondent pool out of methodology section and date from page 1
    tmp_pg2 <- tmptxt[1]
    tmp_table2 <- str_split(tmp_pg2, "\n", simplify = TRUE)
    tmp_pg2 <- gsub(".*\\:", "", tmp_pg2)
    tmp_pg2 <- str_replace_all(tmp_pg2, "\n", " ")
    sample <- str_extract(tmp_pg2, "sample of\\s*(.*?)\\s*\\.")
    data_table <- data_table %>%
      mutate(sample = str_trim(gsub(".*\\of", "", sample)),
             sample = str_replace_all(sample, "\\.|\\- ", ""),
             sample = sub(".*? ",'', sample),
             sample = toupper(sample),
             survey_date = str_trim(tmp_table2[which(str_detect(tmp_table2, "National Tracking Poll"))+1]))

  }, otherwise = NULL)

  toc_qs <- pages %>%
    map(read_toc)

}
