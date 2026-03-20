cat("Creating CSV templates...\n")

# 4.1 sites
sites <- data.frame(site_id=character(), site_name=character(), municipality=character(), state_uf=character(), lat=numeric(), lon=numeric(), mangrove_type=character(), dominant_species=character(), notes=character())
write.csv(sites, "data/clean/sites.csv", row.names=FALSE, na="")

# 4.2 plots
plots <- data.frame(plot_id=character(), site_id=character(), plot_area_m2=numeric(), utm_e=numeric(), utm_n=numeric(), utm_zone=character(), datum=character(), install_date=character(), notes=character())
write.csv(plots, "data/clean/plots.csv", row.names=FALSE, na="")

# 4.3 campaigns
campaigns <- data.frame(campaign_id=character(), plot_id=character(), date=character(), tidal_stage=character(), rain_24h_mm=numeric(), air_temp_c=numeric(), team=character(), notes=character())
write.csv(campaigns, "data/clean/campaigns.csv", row.names=FALSE, na="")

# 4.4 trees
trees <- data.frame(tree_id=character(), plot_id=character(), species=character(), tag=character(), tree_status=character(), notes=character())
write.csv(trees, "data/clean/trees.csv", row.names=FALSE, na="")

# 4.5 tree_measurements
tree_measurements <- data.frame(campaign_id=character(), tree_id=character(), dbh_cm=numeric(), height_m=numeric(), crown_diam_m=numeric(), wood_density_g_cm3=numeric(), tree_status_obs=character(), notes=character())
write.csv(tree_measurements, "data/clean/tree_measurements.csv", row.names=FALSE, na="")

# 4.6 soil_cores
soil_cores <- data.frame(core_id=character(), campaign_id=character(), core_depth_cm=numeric(), core_diam_cm=numeric(), method=character(), waterlogged=character(), notes=character())
write.csv(soil_cores, "data/clean/soil_cores.csv", row.names=FALSE, na="")

# 4.7 soil_layers
soil_layers <- data.frame(core_id=character(), z_top_cm=numeric(), z_bottom_cm=numeric(), bulk_density_g_cm3=numeric(), c_org_g_kg=numeric(), coarse_frac=numeric(), salinity_psu=numeric(), redox_mv=numeric(), ph=numeric(), n_total_g_kg=numeric(), notes=character())
write.csv(soil_layers, "data/clean/soil_layers.csv", row.names=FALSE, na="")

# 4.8 species_ref
species_ref <- data.frame(species=character(), wood_density_g_cm3=numeric(), species_common_name=character(), reference=character(), notes=character())
write.csv(species_ref, "data/dict/species_ref.csv", row.names=FALSE, na="")

# 4.9 methods_alometries
methods_alometries <- data.frame(model_id=character(), component=character(), species_scope=character(), equation=character(), carbon_fraction=numeric(), dbh_min_cm=numeric(), dbh_max_cm=numeric(), reference=character(), notes=character())
write.csv(methods_alometries, "data/dict/methods_alometries.csv", row.names=FALSE, na="")

# 4.10 litter
litter <- data.frame(litter_id=character(), campaign_id=character(), subplot_id=character(), area_m2=numeric(), dry_mass_g=numeric(), c_fraction=numeric(), notes=character())
write.csv(litter, "data/clean/litter.csv", row.names=FALSE, na="")

# 4.11 deadwood
deadwood <- data.frame(deadwood_id=character(), campaign_id=character(), plot_id=character(), decomp_class=character(), volume_m3=numeric(), density_g_cm3=numeric(), c_fraction=numeric(), method=character(), notes=character())
write.csv(deadwood, "data/clean/deadwood.csv", row.names=FALSE, na="")

cat("Done!\n")
