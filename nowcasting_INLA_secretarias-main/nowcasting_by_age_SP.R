# Nowcasting SRAG by age group
# Leo Bastos

library(tidyverse)
library(lubridate)
library(vroom)
library(INLA)
# library(geobr) # For maps
#library(ggthemes) # For colorblindness pallete
library(ggrepel) # Para labels 

# Sys.setlocale("LC_TIME", "pt_BR.UTF-8")

# Fixando o diretório de trabalho
setwd("G:/CCD/CVE/RESPIRATORIAS/COVID_R/")
getwd()

source("Nowcasting/nowcasting_INLA_secretarias-main/nowcasting_fun.r")

## Loading a base do dia
load("boletim.rData")

## Nivel de agregação, padrão usado de DRS
agregado <- unique(boletim$`17DRS`)

# AnÃ¡lise Sao Paulo capital -----------------------------------------

MSP.list <- vector(mode = "list", length = 2)
names(MSP.list) <- c("total", "idade")

dados <- boletim %>% 
  mutate(IDADE = NU_IDADE_N) %>% 
  select(DT_DIGITA, DT_SIN_PRI, IDADE) %>% 
  drop_na(DT_DIGITA) 


## Data da ultima digitacao
trim.data <-  2 ## finais da sÃ©rie para descartar. Por algumas
               ## tentativas com o dado do muncÃ­pio sugerimos no
               ## mÃ­nimo trÃªs dias. Caso o resultado pareÃ§a estranho,
               ## verifique se muda muito se aumentar mais 1-2 dias.

## Data mÃ¡xima de digitaÃ§Ã£o a considerar
DT_max <- max(dados$DT_DIGITA  - trim.data, na.rm = T)

# Dia da semana da ultima digitacao
DT_max_diadasemana <- as.integer(format(DT_max, "%w"))
weekdays(DT_max)

# # Ultima digitacao no sÃ¡bado?
# DT_max <- DT_max - ifelse( DT_max_diadasemana < 6, 
#                            DT_max_diadasemana + 1,
#                            0)

# SRAG
dados.w <- dados %>% 
    ##filter(DT_DIGITA <= DT_max, epiyear(DT_SIN_PRI) == 2021) %>%
    filter(DT_DIGITA <= DT_max, epiyear(DT_SIN_PRI) >= 2021) %>%
  #filter(DT_SIN_PRI > ymd("2021-08-31"), NU_IDADE_N >=0, NU_IDADE_N <120) %>% 
  drop_na(IDADE) %>% 
  mutate(
    # Alterando a data para o primeiro dia da semana 
    # Ex. se ultimo dado for de um domingo, entao a semana 
    # comeca na 2a anterior, se termina 5a, entao comeÃ§a 6a
    DT_SIN_PRI = DT_SIN_PRI - 
      as.integer(format(DT_SIN_PRI, "%w")) - 
      (6-DT_max_diadasemana),
    #DT_DIGITA = DT_DIGITA2 - as.integer(format(DT_DIGITA2, "%w")),
    DT_DIGITA = DT_DIGITA - 
      as.integer(format(DT_DIGITA, "%w")) - 
      (6-DT_max_diadasemana),
    Delay = as.numeric(DT_DIGITA - DT_SIN_PRI) / 7,
    fx_etaria = cut(IDADE, 
                    breaks = c(0,4,11,18,30,seq(50,80,by=10),200), 
                    labels = 1:9,
                    right = F)
    # fx_etaria = cut(NU_IDADE_N, breaks = c(seq(0,80,by=10),200), 
    #                 labels =  1:9,
    #                 right = F)
  ) %>%
    drop_na(fx_etaria) %>%
    filter(Delay >= 0)

# dados.w %>% group_by(DT_SIN_PRI) %>% tally() %>%
#   ggplot(aes(x = DT_SIN_PRI, y = n)) +
#   geom_line() +
#   theme_bw()

# dados.w %>% group_by(DT_SIN_PRI,fx_etaria) %>% tally() %>%
#   ggplot(aes(x = DT_SIN_PRI, y = n)) +
#   geom_line() +
#   theme_bw() +
#   facet_wrap(~fx_etaria)


Dmax <- 15
wdw <- 30
Tmax = max(dados.w$DT_SIN_PRI)

fx.txt <- c("0 - 4", "5 - 11", "12 - 17", "18 - 29",
            "30 - 49", "50 - 59", "60 - 69", 
            "70 - 79", "80 +")
#fx.txt <- c(paste0((0:7)*10, " - ",(0:7)*10+9), "80+")

# max(dmy(dados.SES$`Data dos primeiros sintomas`))

dados.inla <- dados.w %>% 
  filter(DT_SIN_PRI >= Tmax - 7 * wdw, Delay <= Dmax) %>% 
  group_by(DT_SIN_PRI, delay = Delay, fx_etaria) %>% 
  tally(name = "Y") 

tbl.date.aux <- tibble(
  DT_SIN_PRI = unique(dados.inla$DT_SIN_PRI)
) %>% 
  rowid_to_column(var = "Time")

dados.inla <- dados.inla %>% left_join(tbl.date.aux) 


Tmax.id <- max(dados.inla$Time)

# By age
tbl.NA <- expand.grid(Time = 1:Tmax.id,
                      delay = 0:Dmax,
                      fx_etaria = unique(dados.inla$fx_etaria)
) %>% left_join(tbl.date.aux, by = "Time")

dados.inla <- dados.inla %>% full_join(tbl.NA) %>%  #View()
  mutate(
    Y = ifelse(Time + delay > Tmax.id, as.numeric(NA), Y),
    Y = ifelse(is.na(Y) & Time + delay <= Tmax.id, 0, Y ),
  ) %>% arrange(Time, delay, fx_etaria)


sample.now <- nowcasting_age(dados.age = dados.inla %>% 
                               rename(dt_event = DT_SIN_PRI)
)


MSP.summy <- nowcasting.summary(sample.now, age = T)

MSP.list$total <- dados.w %>% group_by(DT_SIN_PRI) %>% tally() %>% 
  ggplot(aes(x = DT_SIN_PRI, y = n)) +
  geom_line(aes()) + 
  geom_line(data = MSP.summy$total, 
            mapping = aes(x = dt_event, y = Median),
            color = "red") +
  geom_ribbon(data = MSP.summy$total, 
              mapping = aes(x = dt_event, y = Median,
                            ymin = LI, ymax = LS), 
              fill = "red", alpha = 0.3) +
  theme_bw(base_size = 14) 
  #facet_wrap(~`faixa etaria`, nrow = 3, scales = "free_y") +
  



## Graficos

MSP.list$idade <- dados.w %>% 
  group_by(DT_SIN_PRI,fx_etaria) %>% 
  tally() %>% 
  mutate(`faixa etaria` = factor(x = fx_etaria, 
                                 levels = as.character(1:9), 
                                 labels = fx.txt)
  ) %>%
  ggplot(aes(x = DT_SIN_PRI, y = n)) + 
  geom_line(show.legend = F) +
  geom_ribbon(data = MSP.summy$age %>% 
                mutate(
                  `faixa etaria` = factor(x = fx_etaria, 
                                          levels = as.character(1:9), 
                                          labels = fx.txt)
                ),
              mapping = aes(x = dt_event, y = Median, ymin = LI, ymax = LS), fill = "red", 
              color = NA, alpha = 0.2, show.legend = F) +
  geom_line(data = MSP.summy$age %>%   
              mutate(`faixa etaria` = factor(x = fx_etaria, 
                                             levels = as.character(1:9), 
                                             labels = fx.txt),
              ),
            aes(y = Median, x = dt_event), color = "red", show.legend = F) +
  theme_bw(base_size = 14) +
  facet_wrap(~`faixa etaria`, nrow = 3, scales = "free_y") 

## Para nowcasting de SRAG, use estes plots
p1 <-
    gridExtra::grid.arrange(
                   MSP.list$total +
                   labs(
                       x = "Semana de primeiros sintomas",
                       y = "Hospitalizações por SRAG",
                       title = paste0("Estado de São Paulo"),
                       subtitle = paste0("casos notificados até ",
                                      format(DT_max, "%d/%m/%Y"))) + 
                   scale_x_date(date_breaks = "5 weeks",date_labels = "%V"),
                   MSP.list$idade +
                   labs(
                       x = "Semana de primeiros sintomas",
                       y = "Hospitalizações por SRAG",
                       title = paste0("Estado de São Paulo"),
                       subtitle = paste0("casos notificados até ",
                                      format(DT_max, "%d/%m/%Y"))) +
                   scale_x_date(date_breaks = "10 weeks", date_labels = "%V"),
                   nrow = 1) 

ggsave(paste0("Nowcasting/Outputs/Plots/SRAG_est_SP_",format(dmy(DT_max),"%d-%m-%Y"),".png"),
       p1, width = 18, height =9)

# ## Caso o nowcasting seja de SG, use estes plots:
# p2 <-
#     gridExtra::grid.arrange(
#                    MSP.list$total +
#                    labs(
#                        x = "Semana de primeiros sintomas",
#                        y = "Casos de sÃ­ndrome gripal",
#                        title = paste0("MunicÃ­pio de SÃ£o Paulo, casos notificados atÃ© ",
#                                       format(DT_max, "%d/%m/%Y"))) + 
#                    scale_x_date(date_breaks = "5 weeks",date_labels = "%V"),
#                    MSP.list$idade +
#                    labs(
#                        x = "Semana de primeiros sintomas",
#                        y = "Casos de sÃ­ndrome gripal",
#                        title = paste0("MunicÃ­pio de SÃ£o Paulo, casos notificados atÃ© ",
#                                       format(DT_max, "%d/%m/%Y"))) +
#                    scale_x_date(date_breaks = "10 weeks", date_labels = "%V"),
#                    nrow = 1) 
# 
# ggsave(paste0("outputs/SG_mun_SP_",format(dmy(data.dados),"%d-%m-%Y"),".png"),
#        p2, width = 18, height =9)
