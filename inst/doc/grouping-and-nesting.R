## ----setup, include=FALSE-----------------------------------------------------
library(dplyr)
library(tidyr)
library(dtrackr)
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------

age_cats = factor(sprintf("%02d-%02d",seq(0,80,5),seq(4,84,5)))

# A set of synthetic patients:
patients = tibble::tibble(
  patient_id = 1:100,
  age_category = sample(age_cats,100, replace=TRUE),
  ethnicity = sample(1:6, 100, replace = TRUE),
  gender = sample(c("Male","Female"), 100, replace=TRUE),
  group = sample(c("Cases","Controls"), 100, replace=TRUE)
) 

# each patient is going to have a random selection of tests
tests = tibble::tibble(
  test_id = 1:1000,
  patient_id = sample(1:100,1000, replace = TRUE),
  test_type = sample(c("FBC","LFT","Electrolytes"), 1000, replace=TRUE),
  test_date = as.Date("2025-01-01")+sample.int(50, 1000, replace=TRUE)
)

# and each test a random selection of results consisting of components and
# values:
tests = tests %>% mutate(
  result = purrr::map(test_type, ~ case_when(
    .x == "FBC" ~ list(tibble::tibble(
      component = c("HB","platelets","WCC"),
      value = c( runif(1,13.5,15), runif(1,100,1000), runif(1,0,30))
    )),
    .x == "LFT" ~ list(tibble::tibble(
      component = c("AST","GGT"),
      value = c( runif(1,0,100), runif(1,0,100))
    )),
    .x == "Electrolytes" ~ list(tibble::tibble(
      component = c("NA","K","Glucose"),
      value = c( runif(1,130,150), runif(1,3.3,5.2), runif(1,50,150))
    ))
  ))
)

data = patients %>% inner_join(
  tests %>% unnest(result) %>% unnest(result),
  by="patient_id"
)

data %>% glimpse()

## -----------------------------------------------------------------------------

processed = data %>%
  
  # the data is originally long format with one row per test result:
  track("{.count} test results") %>%
  mutate(maybe_diabetic = any(component == "Glucose" & value>130), .by = patient_id) %>%
  nest(test_panel = c(component,value), .messages="") %>%
  
  # Now the data is long format with one row per test:
  comment("{.count} tests") %>%
  nest(tests = starts_with("test_"), .messages="") %>%
  
  # and now long format with one row per patient:
  comment("{.count} patients") %>%
  group_by(group) %>%
  comment("{.count} patients") %>%
  
  # these exclusions are at the patient level
  exclude_all(
    .headline = "people",
    maybe_diabetic ~ "{.excluded} diabetics",
    age_category %in% age_cats[1:4] ~ "{.excluded} under 20"
  ) %>%
  
  # these are now back at the test level
  unnest(tests) %>%
  comment("{.count} tests",.headline = "") %>%
  exclude_all(
    .headline = "tests",
    test_date < "2025-01-07" ~ "{.excluded} with invalid dates"
  ) %>%
  count_subgroup(test_type, .headline = "") %>%
  
  # and finally at the granular test result level
  unnest(test_panel) %>%
  exclude_all(
    .headline = "results",
    component == "HB" & value < 14 ~ "{.excluded} invalid Hb results",
    component == "K" & value < 3.5 ~ "{.excluded} haemolysed K+"
  ) %>%
  group_by(test_type, .add=TRUE, .messages="By tests") %>%
  count_subgroup(component, .headline = "{test_type}") %>%
  ungroup(.messages = "{.count} eligible results") %>%
  nest(test_panel = c(component,value), .messages="") %>%
  comment("{.count} eligible tests") %>%
  nest(tests = starts_with("test_"), .messages="") %>%
  comment("{.count} eligible patients")
  

processed %>%
  flowchart()
  


## -----------------------------------------------------------------------------

data %>% 
  group_by(age_category, gender, group) %>%
  summarise(
    n = n_distinct(patient_id)
  ) %>%
  pivot_wider(values_from = n, names_from = group) %>%
  filter(abs(Cases-Controls) <= 1) %>%
  glimpse()


## -----------------------------------------------------------------------------

old = options(dtrackr.verbose=TRUE)

data %>% 
  track() %>%
  group_by(gender) %>%
  comment(c("{.count} items","before pause")) %>%
  
  # the tracking is paused on this next step as the number of groups becomes >16
  group_by(age_category, group, .add=TRUE) %>%
  comment("This message is not tracked") %>%
  summarise(
    n = n_distinct(patient_id)
  ) %>%
  pivot_wider(values_from = n, names_from = group) %>%
  filter(abs(Cases-Controls) <= 1) %>%
  
  # the tracking is automatically resumed at this point as the grouping has
  # returned to manageable levels.
  group_by(gender) %>%
  comment(c("{.count} summarised rows","after resume")) %>%
  flowchart()

options(old)

