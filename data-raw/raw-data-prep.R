# Preliminaries
rm(list = ls())
library(tidyjson)
library(dplyr)
library(tibble)
library(rappdirs)
lto_version <- "v0.3.3"
root <- "/Users/Paul/Desktop/theme-ontology/stoRy/" # Local path to "stoRy-package" folder
cache_path <- file.path(user_cache_dir("stoRy"), lto_version)
working_dir <- paste0(root, "data-raw/")
output_dir <- paste0(root, "R/")
setwd(working_dir)

# Demo data collections
collection_ids <- c("Collection: tvseries: The Twilight Zone (1959)",
  "Collection: tvseries: The Twilight Zone (1985)",
  "Collection: tvseries: The Twilight Zone (2002)",
  "Collection: tvseries: The Twilight Zone (2019)",
  "Collection: The Twilight Zone Films")

# Copy LTO files to working directory. This will only work if you have already
# installed the stoRy package and configured the "lto_version" version of LTO.
system(paste0("cp ", cache_path, "/themes_tbl.Rds ", working_dir, "temp_themes_tbl.Rds"))
system(paste0("cp ", cache_path, "/stories_tbl.Rds ", working_dir, "temp_stories_tbl.Rds"))
system(paste0("cp ", cache_path, "/collections_tbl.Rds ", working_dir, "temp_collections_tbl.Rds"))

# Construct demo themes tibble
themes_tbl <- readRDS(paste0(working_dir, "temp_themes_tbl.Rds"))
theme_count <- nrow(themes_tbl)
out_file_path <- file.path(working_dir, "themes_tbl.Rds")
saveRDS(themes_tbl, file = out_file_path, compress = TRUE)
system(paste0("rm ", working_dir, "temp_themes_tbl.Rds"))

# Construct demo stories tibble
stories_tbl <- readRDS(paste0(working_dir, "temp_stories_tbl.Rds"))
out_file_path <- file.path(working_dir, "stories_tbl.Rds")
collection_ids_list <- stories_tbl %>% pull(var = collections)
keeper_ids <- NULL
for (i in seq.int(nrow(stories_tbl))) {
  if (length(intersect(pull(collection_ids_list[[i]]), collection_ids)) > 0) {
    keeper_ids <- c(keeper_ids, i)
  }
}
stories_tbl <- stories_tbl[keeper_ids, ]
stories_tbl$story_index <- 1 : nrow(stories_tbl)
story_count <- nrow(stories_tbl)
saveRDS(stories_tbl, file = out_file_path, compress = TRUE)
system(paste0("rm ", working_dir, "temp_stories_tbl.Rds"))

# Construct demo collection tibble
collections_tbl <- readRDS(paste0(working_dir, "temp_collections_tbl.Rds"))
out_file_path <- file.path(working_dir, "collections_tbl.Rds")
collections_tbl <- collections_tbl %>%
  filter(collection_id %in% collection_ids) %>%
  arrange(match(collection_id, collection_ids))
collections_tbl <- collections_tbl %>% add_row(tibble(
  collection_index = NA,
  collection_id = "Collection: All Twilight Zone Stories",
  title = "All Twilight Zone Stories",
  description = "All Twilight Zone stories from television and film.",
  date = "1959-2020",
  component_story_ids = list(tibble(component_story_ids = collections_tbl %>% pull(component_story_ids) %>% unlist(use.names = FALSE))),
  references = list(tibble(references = collections_tbl %>% pull(references) %>% unlist(use.names = FALSE))),
  themes = list(tibble(theme_name = character(0), level = character(0), motivation = character(0))),
  source = NA))
collections_tbl$collection_index <- 1 : nrow(collections_tbl)
collection_count <- nrow(collections_tbl)
saveRDS(collections_tbl, file = out_file_path, compress = TRUE)
system(paste0("rm ", working_dir, "temp_collections_tbl.Rds"))

# Construct metadata tibble
metadata_tbl <- tibble(
  name = character(),
  value = character()
)
metadata_tbl <- metadata_tbl %>% add_row(name = "version", value = "demo")
metadata_tbl <- metadata_tbl %>% add_row(name = "timestamp", value = NA)
metadata_tbl <- metadata_tbl %>% add_row(name = "git_commit_id", value = NA)
metadata_tbl <- metadata_tbl %>% add_row(name = "encoding", value = "UTF-8")
metadata_tbl <- metadata_tbl %>% add_row(name = "theme_count", value = as.character(theme_count))
metadata_tbl <- metadata_tbl %>% add_row(name = "story_count", value = as.character(story_count))
metadata_tbl <- metadata_tbl %>% add_row(name = "collection_count", value = as.character(collection_count))
out_file_path <- file.path(working_dir, "metadata_tbl.Rds")
saveRDS(metadata_tbl, file = out_file_path, compress = TRUE)

# Construct default background collection
# This makes use of stoRy package functions
library(stringr)
library(crayon)
library(tidyr)
source(paste0(root, "R/def_collection.R"))
source(paste0(root, "R/options.R"))
source(paste0(root, "R/set_active_version.R"))
background_collection <- Collection$new(collection_id = "Collection: All Twilight Zone Stories")
out_file_path <- file.path(working_dir, "background_collection.Rds")
saveRDS(background_collection, file = out_file_path, compress = TRUE)

# Save sysdata
metadata_tbl <- readRDS(paste0(working_dir, "metadata_tbl.Rds"))
collections_tbl <- readRDS(paste0(working_dir, "collections_tbl.Rds"))
stories_tbl <- readRDS(paste0(working_dir, "stories_tbl.Rds"))
themes_tbl <- readRDS(paste0(working_dir, "themes_tbl.Rds"))
background_collection <- readRDS(paste0(working_dir, "background_collection.Rds"))
usethis::use_data(metadata_tbl, collections_tbl, stories_tbl, themes_tbl, background_collection, internal = TRUE, overwrite = TRUE)

# Write tibbles to JSON and delete associated Rds files
library(jsonlite)
write(toJSON(metadata_tbl), file = paste0(working_dir, "demo_metadata.JSON"))
system(paste0("rm ", working_dir, "metadata_tbl.Rds"))
write(toJSON(collections_tbl), file = paste0(working_dir, "demo_collections.JSON"))
system(paste0("rm ", working_dir, "collections_tbl.Rds"))
write(toJSON(stories_tbl), file = paste0(working_dir, "demo_stories.JSON"))
system(paste0("rm ", working_dir, "stories_tbl.Rds"))
write(toJSON(themes_tbl), file = paste0(working_dir, "demo_themes.JSON"))
system(paste0("rm ", working_dir, "themes_tbl.Rds"))
system(paste0("rm ", working_dir, "background_collection.Rds"))

