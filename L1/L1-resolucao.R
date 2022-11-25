# Questão 1
## A
dir.create('dados')
options(timeout=600)
if (!require("pacman")) install.packages("pacman")

library(rvest)
library(stringr)
library(dplyr)

ufs_regex = "AC|AL|AM|AP"
ufs = c("AC", "AL", "AM", "AP")
parts = 1:3 

links = read_html("https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao/resource/5093679f-12c3-4d6b-b7bd-07694de54173?inner_span=True") %>%
        html_elements("li") %>% 
        html_elements("a") %>% 
        html_attr("href")

download_links = links[grepl(ufs_regex, links)]
paths = do.call(paste0, expand.grid(ufs, parts) %>% arrange(Var1, Var2))

for (i in seq_along(download_links)) {
  
  download.file(url = download_links[i],
                destfile = paste0('dados/', paths[i], '.csv'))
  
}

## B
p_load(vroom)
ac1 = vroom(file = "dados/AC1.csv", 
            locale = locale("br", encoding = "latin1"),
            num_threads = 3)
head(ac1)
summary(ac1)
summary(ac1$vacina_nome %>% as.factor())

## C
files = do.call(paste0, expand.grid('dados/', list.files('dados/')[-8]))
(dados_folder_size = sum(sapply(files, file.size)))
(ac1_memory_R = object.size(ac1))
(ac1_hd = 282860808)

## D
teste = ac1 %>% filter(vacina_nome=='COVID-19 JANSSEN - Ad26.COV2.S')
# variaveis_interesse = c("ds_servico_tipo_linha", "hr_partida_real", "dt_partida_real",
#                          "sg_iata_origem", "nm_aerodromo_origem", "sg_iata_destino",
#                          "nm_aerodromo_destino", "nr_passag_pagos", "nr_passag_gratis")

janssen = vroom(file = pipe("findstr JANSSEN dados\\AC1.csv"),
                delim = ";",
                locale = locale("br", encoding = "UTF-8"))

janssen_memory_R = object.size(janssen)
(ac1_memory_R - janssen_memory_R)

## E
janssen_all = vroom(file = pipe("findstr JANSSEN dados\\*.csv"),
                    delim = ";",
                    locale = locale("br", encoding = "UTF-8"))

# 2
## A
library(data.table)
library(geobr)
files = list.files(path='dados/', full.names = TRUE)
COLS = c('estabelecimento_uf', 'vacina_descricao_dose', 'estabelecimento_municipio_codigo')
covid_subset = rbindlist(lapply(files, fread, select = COLS))

health_region = read_health_region()

municipal_code = read.csv("Tabela_codigos.csv")
colnames(municipal_code) = c('x', 'uf', 'municipio', 'cod_IBGE', 'cod_regiao_saude', 'nome_regiao_saude')

## B
covid_subset_add = merge(covid_subset, municipal_code,
                     by.x = 'estabelecimento_municipio_codigo',
                     by.y = 'cod_IBGE',
                     all.x = TRUE)

agg_region_vaccinated = covid_subset_add[, .N, by = nome_regiao_saude]
vaccination_median = median(agg_region_vaccinated$N)
agg_region_vaccinated = agg_region_vaccinated[, classification := ifelse (N <= vaccination_median, "Baixa", "Alta")]
agg_region_vaccinated = agg_region_vaccinated[order(N), ]

agg_region_vaccinated[classification == "Baixa"][1:5, ]
agg_region_vaccinated[classification == "Alta"][1:5, ]

## C
library(dplyr)
library(dtplyr)
lazy_covid_subset = lazy_dt(covid_subset)
lazy_agg_region_vaccinated = lazy_covid_subset %>% 
                              left_join(municipal_code, by = c("estabelecimento_municipio_codigo" = "cod_IBGE")) %>% 
                              group_by(nome_regiao_saude) %>% 
                              summarise(n = n()) %>% 
                              mutate(classification = if_else(n <= median(n), "Baixa", "Alta")) %>% 
                              arrange(n)

## D

