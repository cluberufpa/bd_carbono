# Pipeline de Integracao - Banco de Dados de Carbono em Manguezais

cat("=== Iniciando Pipeline ===\n")

cat("\n[1] Validando dados originais...\n")
source("R/validate_data.R")

cat("\n[2] Calculando carbono arboreo (Acima do Solo)...\n")
source("R/calc_tree_carbon.R")

cat("\n[3] Calculando carbono do solo (SOC)...\n")
source("R/calc_soil_soc.R")

cat("\n[4] Sintetizando e agregando resultados (Parcela e Sitio)...\n")
source("R/summarise_results.R")

cat("\n=== Pipeline Concluido! Verifique a pasta 'outputs/' ===\n")
