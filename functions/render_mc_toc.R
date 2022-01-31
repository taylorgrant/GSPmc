# function
render_mc_toc <- function() {
  library(tidyverse)
  # always set
  d <- "/Users/taylor_grant/R/mcSurvey"
  data <- file.path(d, "mc_TOC.rds")
  logo <- file.path(d, "gsp_logo_lumen.png")

  rmarkdown::render(input = file.path(d, "mc_toc.Rmd"),
                    output_file = "~/Box/Morning Consult Surveys/mc_TOC.html",
                    params = list(logo = logo,
                                  data = data))
}

