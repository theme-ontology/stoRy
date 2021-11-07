
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stoRy

<!-- badges: start -->
[![R-CMD-check](https://github.com/theme-ontology/stoRy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/theme-ontology/stoRy/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/theme-ontology/stoRy/branch/master/graph/badge.svg)](https://app.codecov.io/gh/theme-ontology/stoRy?branch=master)
[![Life cycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/stoRy)](https://cran.r-project.org/package=stoRy)
[![](http://cranlogs.r-pkg.org/badges/grand-total/stoRy?color=green)](https://cran.r-project.org/package=stoRy)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

stoRy is a [Tidyverse](https://www.tidyverse.org) friendly package for
downloading, exploring, and analyzing [Literary Theme
Ontology](https://themeontology.org/) (LTO) data in **R**.

## Installation

``` r
# Install the released version of stoRy from CRAN with:
install.packages("stoRy")

# Or the developmental version from GitHub:
# install.packages("devtools")
devtools::install_github("theme-ontology/stoRy")
```

## Using stoRy

The easiest way to get started with stoRy is to make use of the LTO demo
version data. It consists of the themes and 335 thematically annotated
[The Twilight Zone](https://en.wikipedia.org/wiki/The_Twilight_Zone)
American media franchise stories from the latest [LTO
version](https://themeontology.org/pub/data/).

Begin by loading the stoRy package:

``` r
library(stoRy)
```

### Exploring the Demo Data

The LTO demo version is loaded by default:

``` r
which_lto()
```

Get a feel for the demo data by printing some basic information about it
to console:

``` r
print_lto(version = "demo")
```

See the demo data help page for a more in depth description:

``` r
?`lto-demo`
```

#### Exploring the Demo Stories

Thematically annotated stories are initialized by *story ID*. For
example, run

``` r
story <- Story$new(story_id = "tz1959e1x22")
```

to initialize a `Story` object representing the classic *The Twilight
Zone* (1959) television series episode *The Monsters Are Due on Maple
Street*.

Story thematic annotations along with episode identifying metadata are
printed to console in either the default or the standard `.st.txt`
format:

``` r
story
story$print(canonical = TRUE)
```

There are two complementary ways of going about finding story IDs.
First, the LTO website [story search
box](https://themeontology.org/stories) offers a quick-and-dirty way of
locating LTO developmental version story IDs of interest. Since story
IDs are stable, developmental version *The Twilight Zone* story IDs can
be expected to agree with their demo data counterparts. Alternatively, a
demo data story ID is directly obtained from an episode title as
follows:

``` r
# install.packages("dplyr")
library(dplyr)
title <- "The Monsters Are Due on Maple Street"
demo_stories_tbl <- clone_active_stories_tbl()
story_id <- demo_stories_tbl %>% filter(title == !!title) %>% pull(story_id)
story_id
```

The `dplyr` package is required to run the `%>%` mediated pipeline.

A tibble of thematic annotations is obtained by running:

``` r
themes <- story$themes()
themes
```

#### Exploring the Demo Themes

*The Monsters Are Due on Maple Street* is a story about how [mass
hysteria](https://themeontology.org/theme.php?name=mass%20hysteria) can
transform otherwise normal people into an angry mob. To view the *mass
hysteria* theme entry, initialize a `Theme` object with `theme_name`
argument defined accordingly:

``` r
theme <- Theme$new(theme_name = "mass hysteria")
theme
theme$print(canonical = TRUE)
```

To view a tibble of all demo data stories featuring *mass hysteria* run:

``` r
theme$annotations()
```

As with story IDs, there are two ways to look for themes of interest.
Developmental version themes are searchable from LTO website [theme
search box](https://themeontology.org/themes). Demo version themes are
explorable in tibble format. For example, here is one way to search for
*mass hysteria* directly in the demo themes:

``` r
# install.packages("stringr")
library(stringr)
demo_themes_tbl <- clone_active_themes_tbl()
demo_themes_tbl %>% filter(str_detect(theme_name, "mass"))
```

Notice that all themes containing the substring `"mass"` are returned.

#### Exploring the Demo Collections

Each story belongs to at least one collection (i.e. a set of related
stories). *The Monsters Are Due on Maple Street*, for instance, belongs
to the two collections:

``` r
story$collections()
```

To initialize a `Collection` object for *The Twilight Zone* (1959)
television series, of which *The Monsters Are Due on Maple Street* is an
episode, run:

``` r
collection <- Collection$new(collection_id = "Collection: tvseries: The Twilight Zone (1959)")
```

Collection info is printed to console in the same way as with stories
and themes:

``` r
collection
collection$print(canonical = TRUE)
```

In general, developmental version collections can be explored from the
LTO website [story search box](https://themeontology.org/stories) or
through the package in the usual way:

``` r
demo_collections_tbl <- clone_active_collections_tbl()
demo_collections_tbl
```

### Analyzing the Demo Data

The LTO thematically annotated story data can be analyzed in various
ways.

#### Topmost Featured Themes

To view the top 10 most featured themes in the *The Twilight Zone*
(1959) series run:

``` r
collection <- Collection$new(collection_id = "Collection: tvseries: The Twilight Zone (1959)")
result_tbl <- get_featured_themes(collection)
result_tbl
```

To view the top 10 most featured themes in the demo data as a whole run:

``` r
result_tbl <- get_featured_themes()
result_tbl
```

#### Topmost Enriched Themes

To view the top 10 most enriched, or over-represented themes in *The
Twilight Zone* (1959) series with all *The Twilight Zone* stories as
background run:

``` r
test_collection <- Collection$new(collection_id = "Collection: tvseries: The Twilight Zone (1959)")
result_tbl <- get_enriched_themes(test_collection)
result_tbl
```

To run the same analysis not counting *minor* level themes run:

``` r
result_tbl <- get_enriched_themes(test_collection, weights = list(choice = 1, major = 1, minor = 0))
result_tbl
```

#### Topmost Similar Stories

To view the top 10 most thematically similar *The Twilight Zone*
franchise stories to *The Monsters Are Due on Maple Street* run:

``` r
query_story <- Story$new(story_id = "tz1959e1x22")
result_tbl <- get_similar_stories(query_story)
result_tbl
```

#### Similar Story Clusters

Cluster *The Twilight Zone* franchise stories according to thematic
similarity:

``` r
# install.packages("isa2")
library(dplyr)
set.seed(123)
result_tbl <- get_story_clusters()
result_tbl
```

The command `set.seed(123)` is run here for the sake of reproducibility.

Explore a cluster of stories related to traveling back in time:

``` r
cluster_id <- 3
pull(result_tbl, stories)[[cluster_id]]
pull(result_tbl, themes)[[cluster_id]]
```

Explore a cluster of stories related to mass panics:

``` r
cluster_id <- 5
pull(result_tbl, stories)[[cluster_id]]
pull(result_tbl, themes)[[cluster_id]]
```

Explore a cluster of stories related to executions:

``` r
cluster_id <- 7
pull(result_tbl, stories)[[cluster_id]]
pull(result_tbl, themes)[[cluster_id]]
```

Explore a cluster of stories related to space aliens:

``` r
cluster_id <- 10
pull(result_tbl, stories)[[cluster_id]]
pull(result_tbl, themes)[[cluster_id]]
```

Explore a cluster of stories related to old people wanting to be young:

``` r
cluster_id <- 11
pull(result_tbl, stories)[[cluster_id]]
pull(result_tbl, themes)[[cluster_id]]
```

Explore a cluster of stories related to wish making:

``` r
cluster_id <- 13
pull(result_tbl, stories)[[cluster_id]]
pull(result_tbl, themes)[[cluster_id]]
```

## Downloading Data

The package works with data from these LTO versions:

``` r
lto_version_statuses()
```

To download and cache the latest versioned LTO release run

``` r
configure_lto(version = "latest")
```

This can take awhile.

Load the newly configured LTO version as the active version in the R
session:

``` r
set_lto(version = "latest")
```

To double check that it has been loaded successfully run

``` r
which_lto()
```

Now that the latest LTO version is loaded into the R session, its
stories and themes can be analyzed in the same way as with the “demo”
LTO version data as shown above.

## Getting Help

If you encounter a bug, please file a minimal reproducible example on
[GitHub issues](https://github.com/theme-ontology/stoRy/issues/). For
questions and other discussion, please post on the [GitHub discussions
board](https://github.com/theme-ontology/stoRy/discussions/).

## License

All code in this repository is published with the
[GPL v3](https://www.gnu.org/licenses/gpl-3.0.html) license.
