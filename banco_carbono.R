
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#
#                                                                             #
#                                                                             #
#                                                                             #
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx#


library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

#1. Conexão com o banco de dados -----------------------------------------------

con <- dbConnect(RSQLite::SQLite(), "tracuateua_ensino_pt.sqlite")


# dbDisconnect(con)

# 1.1 Tabelas como tibbles -----------------------------------------------------

parcelas <- tbl(con, "parcelas")
arvores <- tbl(con, "arvores")
medidas_fuste <- tbl(con, "medidas_fuste")
especies <- tbl(con, "especies")
condicoes <- tbl(con, "condicoes_individuo")
dados_brutos <- tbl(con, "dados_brutos")


# 2. Resumo banco --------------------------------------------------------------

resumo_banco <- tibble(
  tabela = c("parcelas", "arvores", "medidas_fuste", "especies", "condicoes"),
  n = c(
    parcelas %>% tally() %>% pull(n),
    arvores %>% tally() %>% pull(n),
    medidas_fuste %>% tally() %>% pull(n),
    especies %>% tally() %>% pull(n),
    condicoes %>% tally() %>% pull(n)
  )
)

resumo_banco


# 3. Amostra de árvores por parcela --------------------------------------------

arvores_por_parcela <- 
  arvores %>%
  inner_join(parcelas, by = "id_parcela") %>%
  count(codigo_parcela, name = "n_arvores") %>%
  arrange(desc(n_arvores)) %>%
  collect()

arvores_por_parcela

# 4. Abundância de espécies ----------------------------------------------------

abundancia_especies <- arvores %>%
  inner_join(especies, by = "id_especie") %>%
  count(codigo_especie, nome_cientifico, nome_comum, name = "n_individuos") %>%
  arrange(desc(n_individuos)) %>%
  collect()

abundancia_especies

abundancia_especies %>%
  ggplot(aes(x = reorder(nome_comum, n_individuos), y = n_individuos)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Abundância de indivíduos por espécie",
    x = "Espécie",
    y = "Número de indivíduos"
  )

# 5. Altura media por espécie --------------------------------------------------

altura_por_especie <- arvores %>%
  inner_join(especies, by = "id_especie") %>%
  filter(!is.na(altura_m)) %>%
  group_by(nome_comum, nome_cientifico) %>%
  summarise(
    n = n(),
    altura_media = mean(altura_m, na.rm = TRUE),
    altura_sd = sd(altura_m, na.rm = TRUE),
    altura_min = min(altura_m, na.rm = TRUE),
    altura_max = max(altura_m, na.rm = TRUE)
  ) %>%
  arrange(desc(altura_media)) %>%
  collect()

altura_por_especie

# 6. Distribuição da altura por especie ----------------------------------------

dados_altura <- arvores %>%
  inner_join(especies, by = "id_especie") %>%
  filter(!is.na(altura_m)) %>%
  select(nome_comum, altura_m) %>%
  collect()

ggplot(dados_altura, aes(x = nome_comum, y = altura_m)) +
  geom_boxplot() +
  labs(
    title = "Distribuição da altura por espécie",
    x = "Espécie",
    y = "Altura (m)"
  )

# 7. Calculo DAP por especie ---------------------------------------------------

dap_por_especie <- medidas_fuste %>%
  filter(!is.na(cap_cm)) %>%
  mutate(dap_cm = cap_cm / pi) %>%
  inner_join(arvores, by = "id_arvore") %>%
  inner_join(especies, by = "id_especie") %>%
  group_by(nome_comum, nome_cientifico) %>%
  summarise(
    n_fustes = n(),
    dap_medio = mean(dap_cm, na.rm = TRUE),
    dap_sd = sd(dap_cm, na.rm = TRUE),
    dap_min = min(dap_cm, na.rm = TRUE),
    dap_max = max(dap_cm, na.rm = TRUE)
  ) %>%
  arrange(desc(dap_medio)) %>%
  collect()

dap_por_especie

# 8. Calcular DAP a partir do CAP ----------------------------------------------

fustes_com_dap <- medidas_fuste %>%
  filter(!is.na(cap_cm)) %>%
  mutate(
    dap_cm = cap_cm / pi
  )

fustes_com_dap

# 9. Estatísticas do DAP por espécie -------------------------------------------
dap_por_especie <- medidas_fuste %>%
filter(!is.na(cap_cm)) %>%
  mutate(dap_cm = cap_cm / pi) %>%
  inner_join(arvores, by = "id_arvore") %>%
  inner_join(especies, by = "id_especie") %>%
  group_by(nome_comum, nome_cientifico) %>%
  summarise(
    n_fustes = n(),
    dap_medio = mean(dap_cm, na.rm = TRUE),
    dap_sd = sd(dap_cm, na.rm = TRUE),
    dap_min = min(dap_cm, na.rm = TRUE),
    dap_max = max(dap_cm, na.rm = TRUE)
  ) %>%
  arrange(desc(dap_medio)) %>%
  collect()

dap_por_especie

# 10. Distribuições de classes diamétricas -------------------------------------

classes_dap <- medidas_fuste %>%
  filter(!is.na(cap_cm)) %>%
  mutate(dap_cm = cap_cm / pi) %>%
  mutate(
    classe_dap = case_when(
      dap_cm < 5 ~ "<5",
      dap_cm >= 5 & dap_cm < 10 ~ "5-10",
      dap_cm >= 10 & dap_cm < 15 ~ "10-15",
      dap_cm >= 15 & dap_cm < 20 ~ "15-20",
      dap_cm >= 20 ~ "20+",
      TRUE ~ NA_character_
    )
  ) %>%
  count(classe_dap) %>%
  collect()

classes_dap

classes_dap %>%
  ggplot(aes(x = classe_dap, y = n)) +
  geom_col() +
  labs(
    title = "Distribuição em classes de DAP",
    x = "Classe de DAP (cm)",
    y = "Número de fustes"
  )

# 11. Árvores multifuste -------------------------------------------------------

n_fustes_por_arvore <- medidas_fuste %>%
  group_by(id_arvore) %>%
  summarise(n_fustes = n()) %>%
  collect()

n_fustes_por_arvore %>%
  count(n_fustes)

# 12. Proporção de árvores com 1 fuste vs multifuste ---------------------------
n_fustes_por_arvore %>%
  mutate(tipo = if_else(n_fustes == 1, "um_fuste", "multifuste")) %>%
  count(tipo) %>%
  mutate(prop = n / sum(n))


multifuste_especie <- medidas_fuste %>%
  group_by(id_arvore) %>%
  summarise(n_fustes = n()) %>%
  inner_join(arvores, by = "id_arvore") %>%
  inner_join(especies, by = "id_especie") %>%
  mutate(multifuste = n_fustes > 1) %>%
  group_by(nome_comum) %>%
  summarise(
    n_arvores = n(),
    n_multifuste = sum(multifuste, na.rm = TRUE),
    prop_multifuste = mean(multifuste, na.rm = TRUE)
  ) %>%
  arrange(desc(prop_multifuste)) %>%
  collect()

multifuste_especie

# 13. Espécies com mais indivíduos multifuste ----------------------------------

multifuste_especie <- medidas_fuste %>%
  group_by(id_arvore) %>%
  summarise(n_fustes = n()) %>%
  inner_join(arvores, by = "id_arvore") %>%
  inner_join(especies, by = "id_especie") %>%
  mutate(multifuste = n_fustes > 1) %>%
  group_by(nome_comum) %>%
  summarise(
    n_arvores = n(),
    n_multifuste = sum(multifuste, na.rm = TRUE),
    prop_multifuste = mean(multifuste, na.rm = TRUE)
  ) %>%
  arrange(desc(prop_multifuste)) %>%
  collect()

multifuste_especie

# 14. Condição dos indivíduos ------------------------------------------------------

condicao_individuos <- arvores %>%
  left_join(condicoes, by = "id_condicao") %>%
  mutate(
    condicao_padronizada = coalesce(condicao_padronizada, "sem_registro")
  ) %>%
  count(condicao_padronizada, name = "n_individuos") %>%
  arrange(desc(n_individuos)) %>%
  collect()

condicao_individuos

# 15. Condição por espécie -----------------------------------------------------

condicao_por_especie <- arvores %>%
  left_join(condicoes, by = "id_condicao") %>%
  left_join(especies, by = "id_especie") %>%
  mutate(
    condicao_padronizada = coalesce(condicao_padronizada, "sem_registro")
  ) %>%
  count(nome_comum, condicao_padronizada) %>%
  collect()

condicao_por_especie

ggplot(condicao_por_especie, aes(x = nome_comum, y = n, fill = condicao_padronizada)) +
  geom_col() +
  labs(
    title = "Condição dos indivíduos por espécie",
    x = "Espécie",
    y = "Número de indivíduos",
    fill = "Condição"
  )

# 16. Altura media por parcela -------------------------------------------------

altura_por_parcela <- arvores %>%
  inner_join(parcelas, by = "id_parcela") %>%
  filter(!is.na(altura_m)) %>%
  group_by(codigo_parcela) %>%
  summarise(
    n = n(),
    altura_media = mean(altura_m, na.rm = TRUE),
    altura_sd = sd(altura_m, na.rm = TRUE)
  ) %>%
  arrange(desc(altura_media)) %>%
  collect()

altura_por_parcela

# 17. Area Basal por fuste -----------------------------------------------------

area_basal_especie <- medidas_fuste %>%
  filter(!is.na(cap_cm)) %>%
  mutate(
    dap_cm = cap_cm / pi,
    area_basal_cm2 = pi * (dap_cm / 2)^2,
    area_basal_m2 = area_basal_cm2 / 10000
  ) %>%
  inner_join(arvores, by = "id_arvore") %>%
  inner_join(especies, by = "id_especie") %>%
  group_by(nome_comum) %>%
  summarise(
    n_fustes = n(),
    area_basal_total_m2 = sum(area_basal_m2, na.rm = TRUE),
    area_basal_media_m2 = mean(area_basal_m2, na.rm = TRUE)
  ) %>%
  arrange(desc(area_basal_total_m2)) %>%
  collect()

area_basal_especie

# 18. Area basal por parcela ---------------------------------------------------
area_basal_parcela <- medidas_fuste %>%
  filter(!is.na(cap_cm)) %>%
  mutate(
    dap_cm = cap_cm / pi,
    area_basal_cm2 = pi * (dap_cm / 2)^2,
    area_basal_m2 = area_basal_cm2 / 10000
  ) %>%
  inner_join(arvores, by = "id_arvore") %>%
  inner_join(parcelas, by = "id_parcela") %>%
  group_by(codigo_parcela) %>%
  summarise(
    area_basal_total_m2 = sum(area_basal_m2, na.rm = TRUE),
    n_fustes = n()
  ) %>%
  arrange(desc(area_basal_total_m2)) %>%
  collect()

area_basal_parcela

# 19. Tabela integrada para analises -------------------------------------------
base_analitica <- medidas_fuste %>%
  filter(!is.na(cap_cm)) %>%
  mutate(
    dap_cm = cap_cm / pi,
    area_basal_cm2 = pi * (dap_cm / 2)^2,
    area_basal_m2 = area_basal_cm2 / 10000
  ) %>%
  inner_join(arvores, by = "id_arvore") %>%
  left_join(parcelas, by = "id_parcela") %>%
  left_join(especies, by = "id_especie") %>%
  left_join(condicoes, by = "id_condicao") %>%
  collect()


# 20. Exemplo relatório --------------------------------------------------------
glimpse(base_analitica)

base_analitica <- medidas_fuste %>%
  filter(!is.na(cap_cm)) %>%
  mutate(
    dap_cm = cap_cm / pi,
    area_basal_cm2 = pi * (dap_cm / 2)^2,
    area_basal_m2 = area_basal_cm2 / 10000
  ) %>%
  inner_join(arvores, by = "id_arvore") %>%
  left_join(parcelas, by = "id_parcela") %>%
  left_join(especies, by = "id_especie") %>%
  left_join(condicoes, by = "id_condicao") %>%
  collect()

glimpse(base_analitica)

