library(dplyr)
library(tidyr)
library(readr)

# Carrega os dados necessarios
trees <- read_csv("data/clean/trees.csv", show_col_types = FALSE)
tree_measurements <- read_csv("data/clean/tree_measurements.csv", show_col_types = FALSE)
species_ref <- read_csv("data/dict/species_ref.csv", show_col_types = FALSE)

# Enriquecimento com densidade da madeira e mescla com a base de arvores
tree_data <- tree_measurements %>%
  left_join(trees, by = "tree_id") %>%
  left_join(species_ref, by = "species", suffix = c("", "_ref")) %>%
  mutate(
    wood_density_final = coalesce(wood_density_g_cm3, wood_density_g_cm3_ref)
  )

# Calculo do carbono (equacao explemplificada e conversao com 0.48 de fracao de carbono)
carbon_fraction_default <- 0.48

tree_data <- tree_data %>%
  mutate(
    AGB_kg = 0.251 * wood_density_final * (dbh_cm ^ 2.46),
    C_AGB_kg = AGB_kg * carbon_fraction_default
  )

# Exportacao
write_csv(tree_data, "outputs/tree_calculations.csv")
message("Calculos de carbono arboreo concluidos e salvos (tree_calculations.csv).")
