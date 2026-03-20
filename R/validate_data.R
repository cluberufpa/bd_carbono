library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(janitor)

# Leitura
sites <- read_csv("data/clean/sites.csv", show_col_types = FALSE)
plots <- read_csv("data/clean/plots.csv", show_col_types = FALSE)
campaigns <- read_csv("data/clean/campaigns.csv", show_col_types = FALSE)
trees <- read_csv("data/clean/trees.csv", show_col_types = FALSE)
tree_measurements <- read_csv("data/clean/tree_measurements.csv", show_col_types = FALSE)
soil_cores <- read_csv("data/clean/soil_cores.csv", show_col_types = FALSE)
soil_layers <- read_csv("data/clean/soil_layers.csv", show_col_types = FALSE)
litter <- read_csv("data/clean/litter.csv", show_col_types = FALSE)
deadwood <- read_csv("data/clean/deadwood.csv", show_col_types = FALSE)

species_ref <- read_csv("data/dict/species_ref.csv", show_col_types = FALSE)
methods_alometries <- read_csv("data/dict/methods_alometries.csv", show_col_types = FALSE)

# Função para verificar duplicidade de chave
check_unique_key <- function(data, key_cols, table_name) {
  dup <- data %>%
    count(across(all_of(key_cols))) %>%
    filter(n > 1)

  if (nrow(dup) > 0) {
    stop(paste0("Duplicidade encontrada em ", table_name, " para a chave: ", paste(key_cols, collapse = ", ")))
  } else {
    message(paste("Chaves unicas validadas em", table_name))
  }
}

# Verificações de chaves primárias e compostas
check_unique_key(sites, "site_id", "sites")
check_unique_key(plots, "plot_id", "plots")
check_unique_key(campaigns, "campaign_id", "campaigns")
check_unique_key(trees, "tree_id", "trees")
check_unique_key(soil_cores, "core_id", "soil_cores")

check_unique_key(tree_measurements, c("campaign_id", "tree_id"), "tree_measurements")
check_unique_key(soil_layers, c("core_id", "z_top_cm", "z_bottom_cm"), "soil_layers")

# Integridade referencial
check_ref_integrity <- function(child, parent, by_col, child_name, parent_name) {
  missing <- anti_join(child, parent, by = by_col)
  if(nrow(missing) > 0) {
     stop(paste("Problema de integridade: Existem", nrow(missing), "registros em", child_name, "sem correspondencia em", parent_name, "pela coluna", paste(by_col, collapse=", ")))
  } else {
     message(paste("Integridade referencial validada entre", child_name, "e", parent_name))
  }
}

check_ref_integrity(plots, sites, "site_id", "plots", "sites")
check_ref_integrity(campaigns, plots, "plot_id", "campaigns", "plots")
check_ref_integrity(trees, plots, "plot_id", "trees", "plots")
check_ref_integrity(tree_measurements, trees, "tree_id", "tree_measurements", "trees")
check_ref_integrity(tree_measurements, campaigns, "campaign_id", "tree_measurements", "campaigns")
check_ref_integrity(soil_cores, campaigns, "campaign_id", "soil_cores", "campaigns")
check_ref_integrity(soil_layers, soil_cores, "core_id", "soil_layers", "soil_cores")

message("Todas as validações completadas com sucesso.")
