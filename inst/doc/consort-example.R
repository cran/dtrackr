## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

here::i_am("vignettes/consort-example.Rmd")


## ----setup--------------------------------------------------------------------
library(tidyverse)
library(dtrackr)


## -----------------------------------------------------------------------------
# Some useful formatting options
old = options(
  dtrackr.strata_glue="{tolower(.value)}",
  dtrackr.strata_sep=", ",
  dtrackr.default_message = "{.count} patients",
  dtrackr.default_headline = NULL
)

ilpd = dtrackr::ILPD %>% 
  track() %>%
  capture_exclusions() %>%
  include_any(
    Gender == "Female" & Total_Bilirubin >= 0.7 ~ "{.included} women with bili>0.7",
    Gender == "Male" & Total_Bilirubin >= 0.8 ~ "{.included} men with bili>0.8"
  ) %>%
  group_by(Case_or_Control, .messages="cases versus controls") %>%
  comment() %>%
  exclude_all(
    Age<18 ~ "{.excluded} subjects under 18",
    Age>80 ~ "{.excluded} subjects over 80"
  ) %>%
  comment(.messages = "{.count} after exclusions") %>%
  status(
    mean_bili = sprintf("%1.2f \u00B1 %1.2f",mean(Total_Bilirubin),sd(Total_Protein)),
    mean_alb = sprintf("%1.2f \u00B1 %1.2f",mean(Albumin),sd(Albumin)),
    .messages = c(
      "bilirubin: {mean_bili}",
      "albumin: {mean_alb}"
    )                    
  ) %>%
  ungroup(.messages = "{.count} in final data set")

# restore to originals
options(old)

## -----------------------------------------------------------------------------
# saving this flowchart for the JOSS paper.
# ilpd %>% flowchart(filename = here::here("vignettes/joss/figure1-ilpd-consort.pdf"))
ilpd %>% flowchart()

## -----------------------------------------------------------------------------

# here we filter out the majority of the actual content of the excluded data to focus on the 
# metadata recovered during the exclusion.
ilpd %>% excluded() %>% select(.stage,.message,.filter,Age, Gender)


## -----------------------------------------------------------------------------

ilpd = dtrackr::ILPD %>% 
  
  track(.tag="initial cohort") %>%
  #     ^^^^^^^^^^^^^^^^^^^^^
  #     TAGS DEFINED
  
  capture_exclusions() %>%
  
  include_any(
    Gender == "Female" & Total_Bilirubin >= 0.7 ~ "{.included} women with bili>0.7",
    Gender == "Male" & Total_Bilirubin >= 0.8 ~ "{.included} men with bili>0.8"
  ) %>%
  
  group_by(Case_or_Control, .messages="cases versus controls") %>%
  
  comment(.tag="study cohort") %>%
  #       ^^^^^^^^^^^^^^^^^^^
  #       SECOND SET OF TAGS DEFINED
  
  exclude_all(
    Age<18 ~ "{.excluded} subjects under 18",
    Age>80 ~ "{.excluded} subjects over 80"
  ) %>%
  
  comment(.messages = "{.count} after exclusions") %>%
  
  status(
    mean_bili = sprintf("%1.2f \u00B1 %1.2f",mean(Total_Bilirubin),sd(Total_Protein)),
    mean_alb = sprintf("%1.2f \u00B1 %1.2f",mean(Albumin),sd(Albumin)),
    .messages = c(
      "bilirubin: {mean_bili}",
      "albumin: {mean_alb}"
    ),
    .tag = "qualifying patients"
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # THIRD SET TAGS DEFINED
  ) %>%
  
  ungroup(.messages = "{.count} in final data set", .tag="final set")
  #                                                 ^^^^^^^^^^^^^^^^
  #                                                 LAST TAGS DEFINED

## -----------------------------------------------------------------------------
ilpd %>% tagged() %>% tidyr::unnest(.content)

## -----------------------------------------------------------------------------
initialSet = ilpd %>% tagged(.tag = "initial cohort", .glue = "{.count} patients")
finalSet = ilpd %>% tagged(.tag = "final set", .glue = "{.count} patients")

# there were `r initialSet` in the study, of whom `r finalSet` met the eligibility criteria.

## -----------------------------------------------------------------------------
ilpd %>% tagged(
    .tag = "qualifying patients", 
    .glue = "{.strata}: {.count}/{.total} ({sprintf('%1.1f', .count/.total*100)}%) patients on {sysDate}, with a mean bilirubin of {mean_bili}", 
    sysDate = Sys.Date()
    # we could have included any number of other parameters here from the global environment
  ) %>% dplyr::pull(.label)

## -----------------------------------------------------------------------------
ilpd %>% tagged(.glue = "{.count}/{.total} patients")

## -----------------------------------------------------------------------------
ilpd %>% 
  tagged() %>%   # selects only top level content
  tidyr::unnest(.content) %>% 
  dplyr::select(.tag, .total) %>% 
  dplyr::distinct() %>%
  tidyr::pivot_wider(values_from=.total, names_from=.tag) %>% 
  glue::glue_data("Out of {`initial cohort`} patients, {`study cohort`} were eligible for inclusion on the basis of their liver function tests but {`study cohort`-`qualifying patients`} were 
                  outside the age limits. This left {`final set`} patients included in the final study (i.e. overall {`initial cohort`-`final set`} were removed).")

