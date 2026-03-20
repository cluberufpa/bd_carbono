# Banco de Dados de Carbono em Manguezais

Este repositório contém a infraestrutura científica para integração de informações ecológicas, geográficas, químicas e biofísicas de estudos de carbono em manguezais. O banco de dados foi estruturado de forma relacional permitindo que a análise seja automatizada, auditável e tecnicamente sólida através do software R.

## Estrutura de Pastas
- `data/raw/`: Reservado para eventuais dados base extraídos do campo sem formatação
- `data/clean/`: Tabelas primárias estruturadas com esquemas definidos
- `data/dict/`: Tabelas auxiliares e dicionários para cruzamentos e junções
- `R/`: Scripts para validação e execução dos cálculos de estoque de carbono
- `outputs/`: Diretório de destino contendo os arquivos derivados e resultados consolidados
- `reports/`: Relatórios técnicos interativos elaborados a partir dos pacotes knitr/quarto

## Como Executar a Pipeline
Basta iniciar o R e rodar o arquivo principal na raiz do projeto:

```r
source("R/run_pipeline.R")
```

A rotina irá ler, validar restrições e integridades relacional, calcular biomassas usando as alometrias especificadas, somar carbono das diferentes camadas dos testemunhos do solo, sintetizar a área por parcela/estuário e exportar tabelas analíticas para a pasta `outputs/`.
