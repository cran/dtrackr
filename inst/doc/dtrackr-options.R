## ----setup, include=FALSE-----------------------------------------------------
library(dplyr)
library(dtrackr)

knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------

# these are the defaults
old = options(
  dtrackr.strata_glue="{.group}:{.value}",
  dtrackr.strata_sep="; "
)

survival::cgd %>%
  track() %>%
  group_by(treat) %>%
  comment() %>%
  group_by(sex,.add = TRUE) %>%
  comment(
    .messages = c(
    "{.count} patients",
    "{sprintf('%1.0f',.count/.total*100)}% of the total")) %>%
  ungroup() %>%
  flowchart()

# reset options 
options(old)


## -----------------------------------------------------------------------------

# only include the group value in the description of the group
old = options(
  dtrackr.strata_glue="{tolower(.value)}",
  dtrackr.strata_sep=", "
)

survival::cgd %>%
  track() %>%
  group_by(treat, .messages = "case or control") %>%
  comment() %>%
  group_by(sex,
    .add = TRUE, 
    .messages = "by {tolower(.cols)}" #.cols contains a csv string of the grouping variables
  ) %>%
  comment(
    .messages = c(
    "{.count} patients",
    "{sprintf('%1.0f',.count/.total*100)}% of the total")) %>%
  ungroup() %>%
  flowchart()

# reset options 
options(old)


## -----------------------------------------------------------------------------
old = options(
  dtrackr.strata_glue="{tolower(.value)}",
  dtrackr.strata_sep=", ",
  dtrackr.default_message = "containing {.count} patients",
  dtrackr.default_headline = "subgroup: {.strata}"
)

survival::cgd %>%
  track() %>%
  group_by(
    treat,
    .messages = "case or control"
  ) %>%
  comment() %>%
  group_by(
    sex,
    .add = TRUE, 
    .messages = "by gender"
  ) %>%
  comment(
    .messages = c(
    "{.count} patients",
    "{sprintf('%1.0f',.count/.total*100)}% of the total")) %>%
  ungroup() %>%
  flowchart()

# N.b. this setting includes some unwanted headlines in the ungrouped stages of
# the flow chart. If a headline evaluates to "" then the headline is suppressed
# and we can get rid of unwanted headlines. An example of doing this is as
# follows:
# options(dtrackr.default_headline = "{ifelse(.strata != '', glue::glue('subgroup: {.strata}'), '')}")

# reset options 
options(old)


## -----------------------------------------------------------------------------

old = options(
  dtrackr.default_headline = "{.strata}",
  dtrackr.default_count_subgroup = "{tolower(.name)}: {.count}/{.subtotal}"
)

survival::cgd %>%
  track() %>%
  group_by(
    treat,
    .messages = "case or control"
  ) %>%
  comment() %>%
  count_subgroup(
    sex
  ) %>%
  ungroup() %>%
  flowchart()


# reset options 
options(old)


