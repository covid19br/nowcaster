# Lendo os dados

library(vroom)
library(tidyverse)
library(lubridate)

# Dados baixados de:
# https://github.com/covid19br/central_covid/tree/master/dados/SIVEP-Gripe

# Deve dar pra fazer isso automatico, baixei e li com vroom
# Lembrando que sao dois arquivos, um pra 2020 e outro pra 2021
aaa <- vroom("~/Downloads/SRAGHospitalizado_2022_04_04.csv.xz")
bbb <- vroom("~/Downloads/SRAGHospitalizado_2022_04_04.csv.21.xz")
ccc <- vroom("~/Downloads/SRAGHospitalizado_2022_04_04.csv.22.xz")


dados <- aaa %>% 
  select(NU_IDADE_N, TP_IDADE, DT_SIN_PRI, 
         DT_DIGITA, CLASSI_FIN, EVOLUCAO, CO_MUN_RES) %>% 
  bind_rows(
    bbb %>% 
      select(NU_IDADE_N, TP_IDADE, DT_SIN_PRI, 
             DT_DIGITA, CLASSI_FIN, EVOLUCAO, CO_MUN_RES)
  ) %>% 
  bind_rows(
    ccc %>% 
      select(NU_IDADE_N, TP_IDADE, DT_SIN_PRI, 
             DT_DIGITA, CLASSI_FIN, EVOLUCAO, CO_MUN_RES)
  )



fx.txt <- c("0 - 4", "5 - 11", "12 - 17", "18 - 29",
            "30 - 49", "50 - 59", "60 - 69", "70 - 79", 
            "80 +")

dados <- dados %>% 
  filter(TP_IDADE < 4) %>% 
  mutate(
    Idade = ifelse(TP_IDADE == 3, NU_IDADE_N, 0),
    fx_etaria = cut(Idade, 
                    breaks = c(0,5,12,18,30,seq(50,80,by=10),200), 
                    labels = fx.txt,
                    right = F),
    DT_SIN_PRI = dmy(DT_SIN_PRI),
    DT_DIGITA = dmy(DT_DIGITA)
  )

# BH
dadosBH <- dados %>% 
  filter(CO_MUN_RES == "310620") %>% 
  select(-TP_IDADE, -NU_IDADE_N)

#save(dadosBH, file = "Data/dadosBH.RData")
