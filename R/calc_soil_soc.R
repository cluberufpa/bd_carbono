library(dplyr)
library(readr)

soil_cores <- read_csv("data/clean/soil_cores.csv", show_col_types = FALSE)
soil_layers <- read_csv("data/clean/soil_layers.csv", show_col_types = FALSE)

# Preparacao e ajuste fisico do solo
soil_data <- soil_layers %>%
  left_join(soil_cores, by = "core_id") %>%
  mutate(
    thickness_cm = z_bottom_cm - z_top_cm,
    coarse_frac = coalesce(coarse_frac, 0),
    bulk_density_eff = bulk_density_g_cm3 * (1 - coarse_frac)
  )

# Formula conceitual para SOC (Mg C ha-1)
soil_data <- soil_data %>%
  mutate(
    soc_layer_Mg_ha = bulk_density_eff * thickness_cm * (c_org_g_kg / 1000) * 100
  )

# Soma por testemunho total e superficial (0-30 cm)
soc_by_core <- soil_data %>%
  group_by(core_id, campaign_id) %>%
  summarise(
    SOC_Mg_ha_total = sum(soc_layer_Mg_ha, na.rm = TRUE),
    SOC_Mg_ha_0_30 = sum(soc_layer_Mg_ha[z_top_cm < 30], na.rm = TRUE),
    .groups = "drop"
  )

# Exportacao
write_csv(soil_data, "outputs/soil_soc_layers.csv")
write_csv(soc_by_core, "outputs/soil_soc_cores.csv")
message("Calculos de carbono do solo da camada e testemunho concluidos.")
