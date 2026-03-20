library(dplyr)
library(readr)

# Carrega metadados e tabelas relacionais base
plots <- read_csv("data/clean/plots.csv", show_col_types = FALSE)
campaigns <- read_csv("data/clean/campaigns.csv", show_col_types = FALSE)
trees <- read_csv("data/clean/trees.csv", show_col_types = FALSE)
soil_cores <- read_csv("data/clean/soil_cores.csv", show_col_types = FALSE)

# Leitura dos outputs intermediarios
tree_data <- tryCatch(read_csv("outputs/tree_calculations.csv", show_col_types = FALSE), error=function(e) NULL)
soc_by_core <- tryCatch(read_csv("outputs/soil_soc_cores.csv", show_col_types = FALSE), error=function(e) NULL)

if (!is.null(tree_data) && !is.null(soc_by_core)) {
  # 11.1 Carbono arbóreo por parcela
  tree_plot_summary <- tree_data %>%
    left_join(trees %>% select(tree_id, plot_id), by = "tree_id") %>%
    left_join(campaigns %>% select(campaign_id, plot_id), by = "campaign_id", suffix = c("_tree", "_camp")) %>%
    mutate(plot_id = coalesce(plot_id_camp, plot_id_tree)) %>%
    left_join(plots %>% select(plot_id, plot_area_m2), by = "plot_id") %>%
    group_by(plot_id, campaign_id, plot_area_m2) %>%
    summarise(
      C_AGB_kg_plot = sum(C_AGB_kg, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      C_AGB_Mg_ha = (C_AGB_kg_plot / 1000) / (plot_area_m2 / 10000)
    )

  # 11.2 SOC médio por parcela
  soc_plot_summary <- soc_by_core %>%
    left_join(soil_cores %>% select(core_id, campaign_id), by = c("core_id", "campaign_id")) %>%
    left_join(campaigns %>% select(campaign_id, plot_id), by = "campaign_id") %>%
    group_by(plot_id, campaign_id) %>%
    summarise(
      SOC_Mg_ha_total = mean(SOC_Mg_ha_total, na.rm = TRUE),
      SOC_Mg_ha_0_30 = mean(SOC_Mg_ha_0_30, na.rm = TRUE),
      n_cores = n(),
      .groups = "drop"
    )

  # 11.3 Integração final (Parcela)
  plot_summary <- tree_plot_summary %>%
    full_join(soc_plot_summary, by = c("plot_id", "campaign_id")) %>%
    mutate(
      TotalC_Mg_ha_0_30 = coalesce(C_AGB_Mg_ha, 0) + coalesce(SOC_Mg_ha_0_30, 0)
    )

  # 11.4 Síntese por sítio
  site_summary <- plot_summary %>%
    left_join(plots %>% select(plot_id, site_id), by = "plot_id") %>%
    group_by(site_id, campaign_id) %>%
    summarise(
      C_AGB_Mg_ha = mean(C_AGB_Mg_ha, na.rm = TRUE),
      SOC_Mg_ha_0_30 = mean(SOC_Mg_ha_0_30, na.rm = TRUE),
      SOC_Mg_ha_total = mean(SOC_Mg_ha_total, na.rm = TRUE),
      TotalC_Mg_ha_0_30 = mean(TotalC_Mg_ha_0_30, na.rm = TRUE),
      n_plots = n(),
      .groups = "drop"
    )

  write_csv(plot_summary, "outputs/plot_summary.csv")
  write_csv(site_summary, "outputs/site_summary.csv")
  message("Resumos agregados por parcela e sitio salvos em 'outputs/' (plot_summary.csv e site_summary.csv).")
} else {
  message("Aviso: Dados originais ou intermediarios falharam na leitura.")
}
