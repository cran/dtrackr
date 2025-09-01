## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# devtools::load_all()
library(dplyr)
library(dtrackr)


## -----------------------------------------------------------------------------
# devtools::load_all()
filename = "~/tmp/iris.csv"
# this is just a pretend example
# iris = read.csv(filename)


iris %>%
  track(
    .headline = "Iris data:",
    .messages = c(
      "loaded from \"{filename}\"",
      "starts with {.count} items")
  ) %>%
  group_by(Species) %>%
  comment(
    .headline = "{.strata}",
    .messages = c(
    "In {Species}",
    "there are {.count} items",
    "that is {sprintf('%1.0f',.count/.total*100)}% of the total"),
    .tag = "note1"
    ) %>%
  ungroup() %>%
  comment("Final data has {.total} items", .tag="note2") %>%  
  flowchart()


## -----------------------------------------------------------------------------

iris %>%
  track("starts with {.count} items") %>%
  group_by(Species) %>%
  status(
    petalMean = sprintf("%1.1f", mean(Petal.Width)),
    petalSd = sprintf("%1.1f", sd(Petal.Width)),
    .messages = c(
    "In {Species} the petals are",
    "on average {petalMean} \u00B1 {petalSd} cms wide")) %>%
  ungroup(.messages = "ends with {.total} items") %>%
  flowchart()


## -----------------------------------------------------------------------------
ggplot2::diamonds %>%
  track() %>%
  group_by(cut) %>%
  count_subgroup(
    color,
    .messages = "colour {.name}: {sprintf('%1.1f%%', {.count}/{.subtotal}*100)}"
  ) %>%
  ungroup() %>%
  flowchart()

## -----------------------------------------------------------------------------

iris %>%
  track() %>%
  group_by(Species) %>%
  filter(
    Petal.Width < mean(Petal.Width)+sd(Petal.Width)
  ) %>%
  ungroup() %>%
  flowchart()


## -----------------------------------------------------------------------------

dataset1 = iris %>%
  track() %>%
  comment("starts with {.count} items") %>%
  exclude_all(
    Species=="versicolor" ~ "removing {.excluded} versicolor"
  ) %>%
  group_by(Species) %>%
  comment("{Species} has {.count} items") %>%
  exclude_all(
    Petal.Width > mean(Petal.Width)+sd(Petal.Width) ~ "{.excluded} with petals > 1 SD wider than the mean",
    Petal.Length > mean(Petal.Length)+sd(Petal.Length) ~ "{.excluded} with petals > 1 SD longer than the mean",
    Sepal.Width > mean(Sepal.Width)+sd(Sepal.Width) ~ "{.excluded} with sepals > 1 SD wider than the mean",
    Sepal.Length > mean(Sepal.Length)+sd(Sepal.Length) ~ "{.excluded} with sepals > 1 SD longer than the mean"
  ) %>%
  comment("{Species} now has {.count} items") %>%
  ungroup() %>%
  comment("ends with {.total} items")

dataset1 %>% flowchart()


## -----------------------------------------------------------------------------

dataset2 = iris %>%
  track() %>%
  comment("starts with {.count} items") %>%
  include_any(
    Species=="versicolor" ~ "{.included} versicolor",
    Species=="setosa" ~ "{.included} setosa"
  ) %>%
  #mutate(Species = forcats::fct_drop(Species)) %>%
  group_by(Species) %>%
  comment("{Species} has {.count} items") %>%
  include_any(
    Petal.Width < mean(Petal.Width)+sd(Petal.Width) ~ "{.included} with petals <= 1 SD wider than the mean",
    Petal.Length < mean(Petal.Length)+sd(Petal.Length) ~ "{.included} with petals <= 1 SD longer than the mean",
    Sepal.Width < mean(Sepal.Width)+sd(Sepal.Width) ~ "{.included} with sepals <= 1 SD wider than the mean",
    Sepal.Length < mean(Sepal.Length)+sd(Sepal.Length) ~ "{.included} with sepals <= 1 SD longer than the mean"
  ) %>%
  comment("{Species} now has {.count} items") %>%
  ungroup() %>%
  comment("ends with {.total} items")
  
dataset2 %>% flowchart()


## -----------------------------------------------------------------------------

tmp = iris %>%
  track() %>% 
  capture_exclusions() %>%
  exclude_all(
    Petal.Length > 5.8 ~ "{.excluded} long ones",
    Petal.Length < 1.3 ~ "{.excluded} short ones",
    .stage = "petal length exclusion"
  ) %>%
  comment("leaving {.count}") %>%
  group_by(Species) %>%
  filter(
    Sepal.Length >= quantile(Sepal.Length, 0.05),
    .messages="removing {.count.in-.count.out} with sepals < q 0.05",
    .type = "comment",
    .stage = "sepal length exclusion"
  ) %>%
  comment("leaving {.count}") %>%
  exclude_all(
    Petal.Width < 0.2 ~ "{.excluded} narrow ones",
    Petal.Width > 2.1 ~ "{.excluded} wide ones"
  ) %>%
  comment("leaving {.count}")

tmp %>% flowchart()


## -----------------------------------------------------------------------------
tmp %>% excluded()


## -----------------------------------------------------------------------------
tmp %>% flowchart(
  fill = "blue", rounded=TRUE, fontname="Calibri", 
  rankdir = "LR", fontsize = 10, colour = "white", bgcolour="grey10")

## -----------------------------------------------------------------------------
# Grab the dot source, for manual editing:
cat(tmp %>% p_get_as_dot())

## -----------------------------------------------------------------------------
tmp2 = tmp %>% p_get()

# the nodes, .id is a graph unique identifier
tmp2$nodes %>% glimpse()

# the edges, .to and .from are foreign keys for .id
tmp2$edges %>% glimpse()

