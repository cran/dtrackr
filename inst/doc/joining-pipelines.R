## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(tidyr)
library(dtrackr)

## -----------------------------------------------------------------------------

# here we create a set of linked data from the starwars data
# in a real example these data sets would have come from different places
people = starwars %>% select(-films, -vehicles, -starships)
vehicles = starwars %>% select(name,vehicles) %>% unnest(cols = c(vehicles))
starships = starwars %>% select(name,starships) %>% unnest(cols = c(starships))
films = starwars %>% select(name,films) %>% unnest(cols = c(films))
# these 4 data frames are linked together by the name attribute

# we track both input data sets:
tmp1 = people %>% track() %>% comment("People df {.total}")
tmp2 = films %>% track() %>% comment("Films df {.total}") %>% comment("a test comment")

# and here we (re)join the two data sets:
tmp1 %>% 
  inner_join(tmp2, by="name") %>% 
  comment("joined {.total}") %>% 
  flowchart()

# The join message used by inner_join here is configurable but defaults to 
# {.count.lhs} on LHS
# {.count.rhs} on RHS
# {.count.out} in linked set


## -----------------------------------------------------------------------------

tmp = people %>% comment("start")

tmp1 = tmp %>% include_any(
  species == "Human" ~ "{.included} humans",
  species == "Droid" ~ "{.included} droids"
  )

tmp2 = tmp %>% include_any(
  species == "Gungan" ~ "{.included} gungans"
) %>% comment("{.count} gungans")

tmp3 = bind_rows(tmp1,tmp2) %>% comment("{.count} human,droids and gungans") 
tmp3 %>% flowchart()


