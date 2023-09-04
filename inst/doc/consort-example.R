## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

here::i_am("vignettes/consort-example.Rmd")

library(dplyr)
library(dtrackr)

## -----------------------------------------------------------------------------
# Some useful formatting options
old = options(
  dtrackr.strata_glue="{tolower(.value)}",
  dtrackr.strata_sep=", ",
  dtrackr.default_message = "{.count} records",
  dtrackr.default_headline = NULL
)

demo_data = survival::cgd %>% 
  track() %>%
  filter(enum == 1, .type="inclusion", .messages="{.count.out} first observation") %>%
  include_any(
    hos.cat == "US:NIH" ~ "{.included} NIH patients",
    hos.cat == "US:other" ~ "{.included} other US patients"
  ) %>%
  group_by(treat, .messages="cases versus controls") %>%
  comment() %>%
  capture_exclusions() %>%
  exclude_all(
    age<5 ~ "{.excluded} subjects under 5",
    age>35 ~ "{.excluded} subjects over 35",
    steroids == 1 ~ "{.excluded} on steroids at admission"
  ) %>%
  comment(.messages = "{.count} after exclusions") %>%
  status(
    mean_height = sprintf("%1.2f \u00B1 %1.2f",mean(height),sd(height)),
    mean_weight = sprintf("%1.2f \u00B1 %1.2f",mean(weight),sd(weight)),
    .messages = c(
      "average height: {mean_height}",
      "average weight: {mean_weight}"
    )                    
  ) %>%
  ungroup(.messages = "{.count} in final data set")

# restore to originals
options(old)

## ----include=FALSE------------------------------------------------------------
# saving this flowchart for the JOSS paper.
if (interactive()) demo_data %>% flowchart(filename = here::here("vignettes/joss/figure1-consort.pdf")) %>% invisible()

## -----------------------------------------------------------------------------
demo_data %>% flowchart()

## -----------------------------------------------------------------------------

# here we filter out the majority of the actual content of the excluded data to focus on the 
# metadata recovered during the exclusion.
demo_data %>% excluded() %>% select(.stage,.message,.filter,age, steroids)


## -----------------------------------------------------------------------------

demo_data = survival::cgd %>% 
  track(.messages = NULL) %>%
  filter(enum == 1, .type="inclusion", .messages="{.count.out} first observation") %>%
  comment(.tag = "initial cohort") %>%
  #         ^^^^^^^^^^^^^^^^^^^^^
  #         TAGS DEFINED
  
  include_any(
    hos.cat == "US:NIH" ~ "{.included} NIH patients",
    hos.cat == "US:other" ~ "{.included} other US patients"
  ) %>%
  group_by(treat, .messages="cases versus controls") %>%

  comment(.tag="study cohort") %>%
  #       ^^^^^^^^^^^^^^^^^^^
  #       SECOND SET OF TAGS DEFINED
  
  capture_exclusions() %>%
  exclude_all(
    age<5 ~ "{.excluded} subjects under 5",
    age>35 ~ "{.excluded} subjects over 35",
    steroids == 1 ~ "{.excluded} on steroids at admission"
  ) %>%
  
  comment(.messages = "{.count} after exclusions") %>%
  
  status(
    mean_height = sprintf("%1.2f \u00B1 %1.2f",mean(height),sd(height)),
    mean_weight = sprintf("%1.2f \u00B1 %1.2f",mean(weight),sd(weight)),
    .messages = c(
      "average height: {mean_height}",
      "average weight: {mean_weight}"
    ),
    .tag = "qualifying patients"
  #  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  #  THIRD SET TAGS DEFINED                    
  ) %>%
  ungroup(.messages = "{.count} in final data set", .tag="final set")
  #                                                 ^^^^^^^^^^^^^^^^
  #                                                 LAST TAGS DEFINED


## -----------------------------------------------------------------------------
demo_data %>% tagged() %>% tidyr::unnest(.content)

## -----------------------------------------------------------------------------
initialSet = demo_data %>% tagged(.tag = "initial cohort", .glue = "{.count} patients")
finalSet = demo_data %>% tagged(.tag = "final set", .glue = "{.count} patients")

# there were `r initialSet` in the study, of whom `r finalSet` met the eligibility criteria.

## -----------------------------------------------------------------------------
demo_data %>% tagged(
    .tag = "qualifying patients", 
    .glue = "{.strata}: {.count}/{.total} ({sprintf('%1.1f', .count/.total*100)}%) patients on {sysDate}, with a mean height of {mean_height}", 
    sysDate = Sys.Date()
    # we could have included any number of other parameters here from the global environment
  ) %>% dplyr::pull(.label)

## -----------------------------------------------------------------------------
demo_data %>% tagged(.glue = "{.count}/{.total} patients")

## -----------------------------------------------------------------------------
demo_data %>% 
  tagged() %>%   # selects only top level content
  tidyr::unnest(.content) %>% 
  dplyr::select(.tag, .total) %>% 
  dplyr::distinct() %>%
  tidyr::pivot_wider(values_from=.total, names_from=.tag) %>% 
  glue::glue_data("Out of {`initial cohort`} patients, {`study cohort`} were eligible for inclusion on the basis of their age
  but {`study cohort`-`qualifying patients`} were outside the age limits. 
  This left {`final set`} patients included in the final study (i.e. overall {`initial cohort`-`final set`} were removed).")

## -----------------------------------------------------------------------------

# This is a reusable function to restrict ages
age_restrict = function(df, age_col, min_age = 18, max_age = 65) {
  age_col = rlang::ensym(age_col)
  message = sprintf("{.included} between\n%d and %d years", min_age, max_age)
  dtrackr::include_any(df,
    # injection support for parameters must be made explicit using
    # rlang::inject in any functions using include_any or exclude_all
    rlang::inject(min_age <= !!age_col & max_age >= !!age_col ~ !!message)
  )
}

survival::cgd %>% 
  # the `age` column is in the cgd dataset: 
  age_restrict(age, max_age = 30) %>%
  # demonstrating that this works in 2 stages
  age_restrict(age, min_age = 20) %>% 
  flowchart()


