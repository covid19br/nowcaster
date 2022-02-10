# Nowcasting SRAG by age group
# Leo Bastos

library(tidyverse)
library(lubridate)
library(vroom)
library(INLA)
library(geobr) # For maps
#library(ggthemes) # For colorblindness pallete
library(ggrepel) # Para labels 

Sys.setlocale("LC_TIME", "pt_BR.UTF-8")

source("../nowcasting_code/R/nowcasting_fun.r")


# Lendo casos de SRAG -----------------------------------------

# dados.MRJ <- vroom("Data/srag_anon.csv.gz")
# dados <- vroom("~/Downloads/dados-srag-sivep-sao-paulo-sp.zip")
# dados2 <- vroom("~/Downloads/dados-sivep-esus-sao-paulo-sp-set-dez-2021.zip")

# dado32 <- dados2 %>% #filter(!is.na(DT_INTERNA)) %>% 
#   mutate(
#     NU_IDADE_N = IDADE
#   )
#max(dados$DT_SIN_PRI)


#dados.SES <- vroom("~/Downloads/sivep_gripe_2021.csv", locale = vroom::locale(encoding = "latin1"))
#dados.SES <- vroom("~/Downloads/sivep_gripe_2021(1).csv", locale = vroom::locale(encoding = "latin1"))
## dados.SES.21 <- vroom("~/Downloads/sivep_gripe_2021(1).csv", locale = vroom::locale(encoding = "latin1"))
## dados.SES.22 <- vroom("~/Downloads/sivep_gripe_2022.csv", locale = vroom::locale(encoding = "latin1"), )

## Leitura direto do site da SES RJ
dados.SES.21 <- vroom("http://sistemas.saude.rj.gov.br/tabnetbd/sivep_gripe/sivep_gripe_2021.csv",
                      locale = vroom::locale(encoding = "latin1"))
dados.SES.22 <- vroom("http://sistemas.saude.rj.gov.br/tabnetbd/sivep_gripe/sivep_gripe_2022.csv",
                      locale = vroom::locale(encoding = "latin1"), )



# Análise Estado -----------------------------------------

ERJ.list <- vector(mode = "list", length = 2)
names(ERJ.list) <- c("total", "idade")

dados <- dados.SES.21 %>% 
  # filter( `Município de residência` == "RIO DE JANEIRO",
  #         #`Classificação final do caso` == "5 - COVID-19"
  # ) %>% 
  transmute(
    DT_DIGITA = dmy(`Data da digitação no sistema`),
    DT_SIN_PRI = dmy(`Data dos primeiros sintomas`),
    MUN = `Município de residência`,
    MUN_COD = `Código do município de residência`,
    IDADE = ifelse(`Unidade da idade` == "3 - Anos", Idade, 0)
  ) %>% drop_na(DT_DIGITA) 


dados <- dados %>% bind_rows(
  dados.SES.22 %>% 
    # filter( `Município de residência` == "RIO DE JANEIRO",
    #         #`Classificação final do caso` == "5 - COVID-19"
    # ) %>% 
    transmute(
      DT_DIGITA = dmy(`Data da digitação no sistema`),
      DT_SIN_PRI = dmy(`Data dos primeiros sintomas`),
      MUN = `Município de residência`,
      MUN_COD = `Código do município de residência`,
      IDADE = ifelse(`Unidade da idade` == "3 - Anos", Idade, 0)
    ) %>% drop_na(DT_DIGITA) 
)


# Data da ultima digitacao
DT_max <- max(dados$DT_DIGITA, na.rm = T)

# Dia da semana da ultima digitacao
DT_max_diadasemana <- as.integer(format(DT_max, "%w"))
weekdays(DT_max)

# # Ultima digitacao no sábado?
# DT_max <- DT_max - ifelse( DT_max_diadasemana < 6, 
#                            DT_max_diadasemana + 1, 
#                            0)

# SRAG
dados.w <- dados %>% 
  filter(DT_DIGITA <= DT_max) %>% 
  #filter(DT_SIN_PRI > ymd("2021-08-31"), NU_IDADE_N >=0, NU_IDADE_N <120) %>% 
  drop_na(IDADE) %>% 
  mutate(
    # Alterando a data para o primeiro dia da semana 
    # Ex. se ultimo dado for de um domingo, entao a semana 
    # comeca na 2a anterior, se termina 5a, entao começa 6a
    DT_SIN_PRI = DT_SIN_PRI - 
      as.integer(format(DT_SIN_PRI, "%w")) - 
      (6-DT_max_diadasemana),
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
  ) %>% filter(Delay >= 0)

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


Rio.summy <- nowcasting.summary(sample.now, age = T)

ERJ.list$total <- dados.w %>% group_by(DT_SIN_PRI) %>% tally() %>% 
  ggplot(aes(x = DT_SIN_PRI, y = n)) +
  geom_line(aes()) + 
  geom_line(data = Rio.summy$total, 
            mapping = aes(x = dt_event, y = Median),
            color = "red") +
  geom_ribbon(data = Rio.summy$total, 
              mapping = aes(x = dt_event, y = Median,
                            ymin = LI, ymax = LS), 
              fill = "red", alpha = 0.3) +
  theme_bw(base_size = 14) +
  #facet_wrap(~`faixa etaria`, nrow = 3, scales = "free_y") +
  labs(
    x = "Semana de primeiros sintomas",
    y = "Hospitalizaçoes por SRAG",
    #color = "Faixa Etária", 
    title = "Estado do Rio de Janeiro") 




ERJ.list$idade <- dados.w %>% group_by(DT_SIN_PRI,fx_etaria) %>% 
  tally() %>% 
  mutate(`faixa etaria` = factor(x = fx_etaria, 
                                 levels = as.character(1:9), 
                                 labels = fx.txt)
  ) %>%
  ggplot(aes(x = DT_SIN_PRI, y = n)) + 
  geom_line(show.legend = F) +
  geom_ribbon(data = Rio.summy$age %>% 
                mutate(
                  `faixa etaria` = factor(x = fx_etaria, 
                                          levels = as.character(1:9), 
                                          labels = fx.txt)
                ),
              mapping = aes(x = dt_event, y = Median, ymin = LI, ymax = LS), fill = "red", 
              color = NA, alpha = 0.2, show.legend = F) +
  geom_line(data = Rio.summy$age %>%   
              mutate(`faixa etaria` = factor(x = fx_etaria, 
                                             levels = as.character(1:9), 
                                             labels = fx.txt),
              ),
            aes(y = Median, x = dt_event), color = "red", show.legend = F) +
  theme_bw(base_size = 14) +
  facet_wrap(~`faixa etaria`, nrow = 3, scales = "free_y") +
  labs(
    x = "Semana de primeiros sintomas",
    y = "Hospitalizaçoes por SRAG",
    color = "Faixa Etária", 
    title = "Estado do Rio de Janeiro")

gridExtra::grid.arrange(
  ERJ.list$total + 
    scale_x_date(date_breaks = "5 weeks",date_labels = "%V"),
  ERJ.list$idade + 
    scale_x_date(date_breaks = "10 weeks", date_labels = "%V"),
  nrow = 1) 


# Análise Capital -----------------------------------------

MRJ.list <- vector(mode = "list", length = 2)
names(MRJ.list) <- c("total", "idade")

# SRAG
dados.w <- dados %>% 
  filter(DT_DIGITA <= DT_max, 
         MUN == "RIO DE JANEIRO") %>% 
  drop_na(IDADE) %>% 
  mutate(
    # Alterando a data para o primeiro dia da semana 
    # Ex. se ultimo dado for de um domingo, entao a semana 
    # comeca na 2a anterior, se termina 5a, entao começa 6a
    DT_SIN_PRI = DT_SIN_PRI - 
      as.integer(format(DT_SIN_PRI, "%w")) - 
      (6-DT_max_diadasemana),
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
  ) %>% filter(Delay >= 0)

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


Rio.summy <- nowcasting.summary(sample.now, age = T)

MRJ.list$total <- dados.w %>% group_by(DT_SIN_PRI) %>% tally() %>% 
  ggplot(aes(x = DT_SIN_PRI, y = n)) +
  geom_line(aes()) + 
  geom_line(data = Rio.summy$total, 
            mapping = aes(x = dt_event, y = Median),
            color = "red") +
  geom_ribbon(data = Rio.summy$total, 
              mapping = aes(x = dt_event, y = Median,
                            ymin = LI, ymax = LS), 
              fill = "red", alpha = 0.3) +
  theme_bw(base_size = 14) +
  #facet_wrap(~`faixa etaria`, nrow = 3, scales = "free_y") +
  labs(
    x = "Semana de primeiros sintomas",
    y = "Hospitalizaçoes por SRAG",
    #color = "Faixa Etária", 
    title = "Município do Rio de Janeiro") 




MRJ.list$idade <- dados.w %>% group_by(DT_SIN_PRI,fx_etaria) %>% 
  tally() %>% 
  mutate(`faixa etaria` = factor(x = fx_etaria, 
                                 levels = as.character(1:9), 
                                 labels = fx.txt)
  ) %>%
  ggplot(aes(x = DT_SIN_PRI, y = n)) + 
  geom_line(show.legend = F) +
  geom_ribbon(data = Rio.summy$age %>% 
                mutate(
                  `faixa etaria` = factor(x = fx_etaria, 
                                          levels = as.character(1:9), 
                                          labels = fx.txt)
                ),
              mapping = aes(x = dt_event, y = Median, ymin = LI, ymax = LS), fill = "red", 
              color = NA, alpha = 0.2, show.legend = F) +
  geom_line(data = Rio.summy$age %>%   
              mutate(`faixa etaria` = factor(x = fx_etaria, 
                                             levels = as.character(1:9), 
                                             labels = fx.txt),
              ),
            aes(y = Median, x = dt_event), color = "red", show.legend = F) +
  theme_bw(base_size = 14) +
  facet_wrap(~`faixa etaria`, nrow = 3, scales = "free_y") +
  labs(
    x = "Semana de primeiros sintomas",
    y = "Hospitalizaçoes por SRAG",
    color = "Faixa Etária", 
    title = "Município do Rio de Janeiro")


gridExtra::grid.arrange(
  MRJ.list$total + 
    scale_x_date(date_breaks = "5 weeks",date_labels = "%V"),
  MRJ.list$idade + 
    scale_x_date(date_breaks = "10 weeks", date_labels = "%V"),
  nrow = 1) 

# Lendo regionais de saude + mapa -----------------------------------------

# Lendo mapas das macroregionais de saude
hr <- read_health_region( year=2013, macro = T)

# saveRDS(object = hr, file = "Data/hr_BR.rds")
# hr <- readRDS(file = "Data/hr_BR.rds")

RJ.hr <- hr %>% filter(abbrev_state == "RJ")

hr.cod <- as.character(RJ.hr$code_health_marcroregion)
hr.name <- sort(as.character(RJ.hr$name_health_macroregion))

# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())



# Plot all regions in RJ state 
hr.map <- ggplot(data=RJ.hr) +
  geom_sf(aes(fill = name_health_macroregion), show.legend = FALSE) + 
  geom_label_repel(aes(label = name_health_macroregion, 
                       geometry = geom),
                   stat = "sf_coordinates", size = 5, ) +
  #stat_sf_coordinates() +
  theme_minimal(base_size = 14) +
  labs(title = "Macrorregionais de Saúde do Estado do Rio de Janeiro") +
  no_axis

hr.map

# Lendo casos de SRAG -----------------------------------------

tbl.regsaude <- read_csv("Data/rl_municip_macsaud.csv")

dados <- dados %>% 
  mutate(
    MUNCOD = as.character(MUN_COD)
  ) %>% 
  left_join(tbl.regsaude, by = c("MUNCOD"="CO_MUNICIP")) %>% 
  mutate(
    CO_MACSAUD2 = paste0(CO_MACSAUD, "8")
  )

hr.cod
hr.name

hr.list <- vector(mode = "list", length = length(hr.cod))

for(k in 1:length(hr.cod)){
  out.list <- vector(mode = "list", length = 2)
  names(out.list) <- c("total", "idade")
  
  
  # dados <- dados.SES %>% 
  #   filter(
  #     CO_MACSAUD2 == hr.cod[k]
  #   ) %>% 
  #   transmute(
  #     DT_DIGITA = dmy(`Data da digitação no sistema`),
  #     DT_SIN_PRI = dmy(`Data dos primeiros sintomas`),
  #     NU_IDADE_N = Idade
  #   )
  
  
  # SRAG
  dados.w <- dados %>% 
    filter(DT_DIGITA <= DT_max, 
           CO_MACSAUD2 == hr.cod[k]) %>% 
    drop_na(IDADE) %>% 
    #filter(DT_SIN_PRI > ymd("2021-08-31"), NU_IDADE_N >=0, NU_IDADE_N <120) %>% 
    mutate(
      DT_SIN_PRI = DT_SIN_PRI - as.integer(format(DT_SIN_PRI, "%w")),
      DT_DIGITA = DT_DIGITA - as.integer(format(DT_DIGITA, "%w")),
      Delay = as.numeric(DT_DIGITA - DT_SIN_PRI) / 7,
      fx_etaria = cut(IDADE, 
                      breaks = c(0,4,11,18,30,seq(50,80,by=10),200), 
                      labels = 1:9,
                      right = F)
    ) %>% filter(Delay >= 0)
  
  Dmax <- 10
  wdw <- 30
  Tmax = max(dados.w$DT_SIN_PRI)
  fx.txt <- c("0 - 4", "5 - 11", "12 - 17", "18 - 29",
              "30 - 49", "50 - 59", "60 - 69", 
              "70 - 79", "80 +")

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
  
  
  Rio.summy <- nowcasting.summary(sample.now, age = T)
  
  out.list$total <- dados.w %>% group_by(DT_SIN_PRI) %>% tally() %>% 
    ggplot(aes(x = DT_SIN_PRI, y = n)) +
    geom_line(aes()) + 
    geom_line(data = Rio.summy$total, 
              mapping = aes(x = dt_event, y = Median),
              color = "red") +
    geom_ribbon(data = Rio.summy$total, 
                mapping = aes(x = dt_event, y = Median,
                              ymin = LI, ymax = LS), 
                fill = "red", alpha = 0.3) +
    theme_bw(base_size = 14) +
    #facet_wrap(~`faixa etaria`, nrow = 3, scales = "free_y") +
    labs(
      x = "Semana de primeiros sintomas",
      y = "Hospitalizações por SRAG",
      #color = "Faixa Etária", 
      title = paste("Regional de saúde:", hr.name[k]) 
    )

  out.list$idade  <- dados.w %>% group_by(DT_SIN_PRI,fx_etaria) %>% 
    tally() %>% 
    mutate(`faixa etaria` = factor(x = fx_etaria, 
                                   levels = as.character(1:9), 
                                   labels = fx.txt)
    ) %>%
    ggplot(aes(x = DT_SIN_PRI, y = n)) + 
    geom_line(show.legend = F) +
    geom_ribbon(data = Rio.summy$age %>% 
                  mutate(
                    `faixa etaria` = factor(x = fx_etaria, 
                                            levels = as.character(1:9), 
                                            labels = fx.txt)
                  ),
                mapping = aes(x = dt_event, y = Median, ymin = LI, ymax = LS), fill = "red", 
                color = NA, alpha = 0.2, show.legend = F) +
    geom_line(data = Rio.summy$age %>%   
                mutate(`faixa etaria` = factor(x = fx_etaria, 
                                               levels = as.character(1:9), 
                                               labels = fx.txt),
                ),
              aes(y = Median, x = dt_event), color = "red", show.legend = F) +
    theme_bw(base_size = 14) +
    facet_wrap(~`faixa etaria`, nrow = 3, scales = "free_y") +
    labs(
      x = "Semana de primeiros sintomas",
      y = "Hospitalizações por SRAG",
      color = "Faixa Etária", 
      title = paste("Regional de saúde:", hr.name[k]) 
    )
  
  hr.list[[k]] <- out.list
  
}


hr.list





# Plots -------------------------------------------------------------------


gridExtra::grid.arrange(
  ERJ.list$total + 
    scale_x_date(date_breaks = "5 weeks",date_labels = "%V"),
  ERJ.list$idade + 
    scale_x_date(date_breaks = "10 weeks", date_labels = "%V"),
  nrow = 1) 




gridExtra::grid.arrange(
  MRJ.list$total + 
    scale_x_date(date_breaks = "5 weeks",date_labels = "%V"),
  MRJ.list$idade + 
    scale_x_date(date_breaks = "10 weeks", date_labels = "%V"),
  nrow = 1) 


gridExtra::grid.arrange(
  hr.list[[1]]$total + 
    scale_x_date(date_breaks = "5 weeks",date_labels = "%V"),
  hr.list[[1]]$idade + 
    scale_x_date(date_breaks = "10 weeks", date_labels = "%V"),
  nrow = 1)

gridExtra::grid.arrange(
  hr.list[[2]]$total + 
    scale_x_date(date_breaks = "5 weeks",date_labels = "%V"),
  hr.list[[2]]$idade + 
    scale_x_date(date_breaks = "10 weeks", date_labels = "%V"),
  nrow = 1)

gridExtra::grid.arrange(
  hr.list[[3]]$total + 
    scale_x_date(date_breaks = "5 weeks",date_labels = "%V"),
  hr.list[[3]]$idade + 
    scale_x_date(date_breaks = "10 weeks", date_labels = "%V"),
  nrow = 1) 


hr.map

