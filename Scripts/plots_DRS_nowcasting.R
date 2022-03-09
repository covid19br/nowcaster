#!/usr/bin/env Rscript
rm(list = ls())
gc()

if(!require ('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if(!require ('data.table')) {install.packages('data.table')};library('data.table')
# if(!require ('NobBS')) {install.packages('NobBS')};library('NobBS')
if(!require ('scales')) {install.packages('scales')};library('scales')
# if(!require ('forecast')) {install.packages('forecast')};library('forecast')
if(!require ('purrr')) {install.packages('purrr')};library('purrr')
if(!require ('readr')) {install.packages('readr')};library('readr')
if(!require ('vroom')) {install.packages('vroom')};library('vroom')
if(!require ('patchwork')) {install.packages('patchwork')};library('patchwork')

setwd(gsub("RESPIRATORIAS.*","RESPIRATORIAS/COVID_R/",getwd()))
getwd()

## Criar diretório do dia
if(!dir.exists(paste0("nowcastr/Outputs/Plots/", Sys.Date()))){
  dir.create(path = paste0("nowcastr/Outputs/Plots/", Sys.Date()))
}

DRS_name<- c("ARAÇATUBA","ARARAQUARA","BAIXADA SANTISTA","BARRETOS","BAURU","CAMPINAS","FRANCA","GRANDE SÃO PAULO","MARÍLIA",
             "PIRACICABA","PRESIDENTE PRUDENTE","REGISTRO","RIBEIRÃO PRETO","SÃO JOÃO DA BOA VISTA","SÃO JOSÉ DO RIO PRETO","SOROCABA",
             "TAUBATÉ")

date<-Sys.Date() - 1

## Loading arquivos de Nowcasting
dados_w.plot_list_total<-list.files(path = paste0("nowcastr/Outputs/Tables/", date), 
                              pattern = paste0("dados_w_plot_total"), full.names = T)
if(length(dados_w.plot_list_total) != 17){
  warning("There are no 17 files, corresponding to 17 DRS, maybe some units didn't ran!")
}
dados_w.plot_total<-lapply(dados_w.plot_list_total, function(x){
  x<-vroom(x)
})
names(dados_w.plot_total)<-DRS_name
dados_w.plot_total<-dados_w.plot_total %>% 
  bind_rows(.id = "DRS")

dados_w.plot_list_age<-list.files(path = paste0("nowcastr/Outputs/Tables/", date), 
                                    pattern = paste0("dados_w_plot_age"), full.names = T)
if(length(dados_w.plot_list_age) != 17){
  warning("There no 17 files, corresponding to 17 DRS, maybe some units didn't ran!")
}
dados_w.plot_age<-lapply(dados_w.plot_list_age, function(x){
  x<-vroom(x)
})
names(dados_w.plot_age)<-DRS_name
dados_w.plot_age<-dados_w.plot_age %>% 
  bind_rows(.id = "DRS")

nowcast_list_total<-list.files(path = paste0("nowcastr/Outputs/Tables/", date),
                               pattern = paste0("nowcasting_total_"), full.names = T)
if(length(nowcast_list_total) != 17){
  warning("There no 17 files, corresponding to 17 DRS, maybe some units didn't ran!")
}
nowcast_list_total<-lapply(nowcast_list_total, function(x){
  x<-vroom(x)
})
names(nowcast_list_total)<-DRS_name
nowcast_list_total<-nowcast_list_total %>% 
  bind_rows(.id = "DRS")


nowcast_list_age<-list.files(path = paste0("Nowcasting/Outputs/Tables/", date), 
                             pattern = paste0("nowcasting_age_"), full.names = T)
if(length(nowcast_list_age) != 17){
  warning("There no 17 files, corresponding to 17 DRS, maybe some units didn't ran!")
}
nowcast_list_age<-lapply(nowcast_list_age, function(x){
  x<-vroom(x)
})
names(nowcast_list_age)<-DRS_name
nowcast_list_age<-nowcast_list_age %>% 
  bind_rows(.id = "DRS")

## Objetos de armazenamento de plots e a estimativa de nowcasting
plot_now<-vector("list", length(DRS_name))
names(plot_now)<-DRS_name
# 
fx.txt <- c("0 - 4", "5 - 11", "12 - 17", "18 - 29",
            "30 - 49", "50 - 59", "60 - 69",
            "70 - 79", "80 +")

## Objetos de plots 
for (i in 1:length(DRS_name)) {
  
  ## Cortar uma semana
  trim.data <-  1 ## quanto cortar da estimativa
  ## Data Máxima
  DT_max <- max(dados_w.plot_total$DT_SIN_PRI  - trim.data)
  
  ## Filtrando os dados para a DRS de plot
  dados_w.plot_drs_total<-dados_w.plot_total %>% 
    filter(DRS == DRS_name[i], DT_SIN_PRI <= DT_max) %>% 
    rename(dt_event = DT_SIN_PRI)
  
  ## Filtrando os dados para a DRS de plot
  dados_w.plot_drs_age<-dados_w.plot_age %>% 
    filter(DRS == DRS_name[i], DT_SIN_PRI <= DT_max) %>%   
    mutate(`faixa etaria` = factor(x = fx_etaria, 
                                   levels = as.character(1:9), 
                                   labels = fx.txt),
    ) %>% 
    rename(dt_event = DT_SIN_PRI)
  
  ## Filtrando nowcasting para a DRS de interesse
  nowcast_list_total_drs<-nowcast_list_total %>% 
    filter(DRS == DRS_name[i], dt_event <= DT_max) 
  
  # n_max_total<-2*max(dados_w.plot_drs_total$n)

  nowcast_list_total_drs<-nowcast_list_total_drs %>%
    left_join(dados_w.plot_drs_total, 
              by = c("DRS", "dt_event"))%>% 
    mutate(LS_new = if_else(LSb > 3*Median, 3*Median, LSb), 
           LI_new = if_else(LIb < n, n, LIb))
  
  plot_now[[i]]$total<- dados_w.plot_drs_total %>% 
    ggplot(aes(x = dt_event, y = n)) +
    geom_line(aes(), size = .5) + 
    geom_line(data = nowcast_list_total_drs, 
              mapping = aes(x = dt_event, y = Median),
              color = "red", size = .3) +
    geom_ribbon(data = nowcast_list_total_drs,
                mapping = aes(x = dt_event, y = Median,
                              ymin = LI_new, ymax = LS_new),
                fill = "red", alpha = 0.3) +
    theme_bw(base_size = 14) +
    labs(
      x = "Semana de primeiros sintomas",
      y = "Hospitalizações por SRAG-COVID",
      # title = paste0("DRS ", DRS_name[i]),
      # subtitle = paste0("casos notificados até ",
      #                   format(DT_max, "%d/%m/%Y"))
      ) + 
    scale_x_date(date_breaks = "5 weeks",date_labels = "%V")
  # plot_now[[1]]$total
  
  ## Filtrando nowcasting para a DRS de interesse
  nowcast_list_age_drs<-nowcast_list_age %>% 
    filter(DRS == DRS_name[i], dt_event <= DT_max) %>%   
    mutate(`faixa etaria` = factor(x = fx_etaria, 
                                   levels = as.character(1:9), 
                                   labels = fx.txt),
    ) 
  
  nowcast_list_age_drs<-nowcast_list_age_drs %>%
    left_join(dados_w.plot_drs_age, 
              by = c("DRS", "dt_event", "faixa etaria", "fx_etaria"))%>% 
    mutate(LS_new = if_else(LSb > 3*Median, 3*Median, LSb), 
           LI_new = if_else(LIb < n, n, LIb))
  
  plot_now[[i]]$age <- dados_w.plot_drs_age %>% 
    ggplot(aes(x = dt_event, y = n)) + 
    geom_line(show.legend = F) +
    geom_ribbon(data = nowcast_list_age_drs,
                mapping = aes(x = dt_event, y = Median, ymin = LI_new, ymax = LS_new), fill = "firebrick1",
                color = NA, alpha = 0.2, show.legend = F) +
    geom_line(data = nowcast_list_age_drs,
              aes(y = Median, x = dt_event), color = "firebrick1", show.legend = F) +
    theme_bw(base_size = 14) +
    facet_wrap(~`faixa etaria`, nrow = 3, scales = "free_y") +
    labs(
      x = "Semana de primeiros sintomas",
      y = "Hospitalizações por SRAG-COVID",
      # title = paste0("DRS ", DRS_name[i]),
      # subtitle = paste0("casos notificados até ",
      #                   format(DT_max, "%d/%m/%Y"))
      ) +
    scale_x_date(date_breaks = "10 weeks", date_labels = "%V")
  # plot_now[[1]]$age
  
  p1 <- (plot_now[[i]]$total | plot_now[[i]]$age)+
    plot_annotation(title = paste0("DRS ", DRS_name[i]),
                    subtitle = paste0("casos notificados até ",
                                      format(DT_max, "%d/%m/%Y"))
      )
  # p1
  # p1
  # Salvando o plot
  ggsave(plot = p1, filename = paste0('nowcastr/Outputs/Plots/', 
                                      Sys.Date(), 
                                      '/NC_',
                                      date,"_",
                                      DRS_name[i],
                                      '.png'),
         width = 9, height = 7, dpi = 300)
}

#


