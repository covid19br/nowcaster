#!/usr/bin/env Rscript
if(!require ('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if(!require ('data.table')) {install.packages('data.table')};library('data.table')
if(!require ('NobBS')) {install.packages('NobBS')};library('NobBS')
if(!require ('scales')) {install.packages('scales')};library('scales')
if(!require ('forecast')) {install.packages('forecast')};library('forecast')
if(!require ('purrr')) {install.packages('purrr')};library('purrr')
if(!require ('readr')) {install.packages('readr')};library('readr')
if(!require ('vroom')) {install.packages('vroom')};library('vroom')

# Fixando o diretório de trabalho
setwd("G:/CCD/CVE/RESPIRATORIAS/COVID_R/")
getwd()

source("Nowcasting/nowcasting_INLA_secretarias-main/nowcasting_fun.r")
source("Nowcasting/Scripts/nowcasting_inla.R")

## Loading a base do dia
load("boletim.rData")

## Nivel de agregação, padrão usado de GVEs
agregado <- unique(boletim$`17DRS`)

## Objetos de armazenamento de plots e a estimativa de nowcasting
plot_now<-vector("list", length(agregado))
# names(plot_now)<-agregado
nowcast_list<-vector("list", length(agregado))
# names(nowcast_list)<-agregado

## Looping para rodar o nowcasting para cada nível
for (i in seq(1:length(agregado))) {
  print(paste0("Started Nowcasting for ", agregado[i], sep = " "))
  
  ## Filtrando no nível de agregação
  dados_covid <- boletim %>% 
    filter(classi == 'COVID-19')  %>% 
    filter(`17DRS` == agregado[i]) %>% 
    select(DT_SIN_PRI, DT_NOTIFIC) %>% 
    mutate(DT_SIN_PRI = as.Date(DT_SIN_PRI)) %>%
    mutate(DT_NOTIFIC = as.Date(DT_NOTIFIC)) 
  
  ## Função de Nowcasting
  nowcast_list[[i]] <- NobBS(data = dados_covid, 
                             now = Sys.Date(),
                             units="1 day",
                             onset_date="DT_SIN_PRI", 
                             report_date="DT_NOTIFIC", 
                             # moving_window = 30, 
                             max_D = 30, cutoff_D = T)
  
  ## Cálculo de arimética, 10 dias
  nowcast_list[[i]]$estimates$ma <- ma(nowcast_list[[i]]$estimates$estimate, order = 10)
  ## Agregação do dado real
  nowcast_list[[i]]$estimates$n.reported[is.na(nowcast_list[[i]]$estimates$n.reported)] <- 0
  
  ## Objetos de plots 
  plot_now[[i]]<-nowcast_list[[i]]$estimates %>% 
    filter(onset_date > '2021-01-01') %>%
    ggplot() +
    geom_point(mapping = aes(x = onset_date, y = estimate, color = 'red'), alpha = 0.6) +
    geom_line(mapping = aes(x = onset_date, y = estimate, color = 'red'), linetype = "dashed", size=0.1, alpha = 0.6) +
    geom_point(mapping = aes(x = onset_date, y = ma, color = 'navy'), alpha = 0.6) +
    geom_line(mapping = aes(x = onset_date, y = ma, color = 'navy'), alpha = 0.6) +
    geom_point(mapping = aes(x = onset_date, y = n.reported, color = 'darkgreen'), alpha = 0.6) +
    geom_line(mapping = aes(x = onset_date, y = n.reported, color = 'darkgreen'), alpha = 0.6) +
    geom_ribbon(mapping = aes(x = onset_date, ymin=lower, ymax=upper), alpha=0.1) +
    scale_color_identity(name = "Legenda",
                         breaks = c("red", "navy", "darkgreen"),
                         labels = c("Estimado (nowcasting)", "MÃ©dia Movel (10 dias)", "Dado real"),
                         guide = "legend") +
    labs(x = "Data", y = "Casos") +
    ggtitle(paste0(agregado[i], " - SP"))
  
  ## Salvando o plot
  ggsave(filename = paste0('Nowcasting/Outputs/Plots/NC-', 
                           Sys.Date(), 
                           agregado[i], 
                           '.png',
                           sep = "_"),
         width = 9, height = 7, dpi = 300)
  
  vroom_write(nowcast_list[[i]], )
  
  print(paste0("Finished Nowcasting for ", agregado[i], sep = " "))
}
