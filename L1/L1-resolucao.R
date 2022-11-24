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
                locale = locale("br", encoding = "latin1"))

janssen_memory_R = object.size(janssen)
(ac1_memory_R - janssen_memory_R)

## E
janssen_all = vroom(file = pipe("findstr JANSSEN dados\\*.csv"), 
                    locale = locale("br", encoding = "latin1"))

# 2
library(data.table)
library(geobr)
