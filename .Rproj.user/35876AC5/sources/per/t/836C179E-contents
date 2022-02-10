#!/usr/bin/env Rscript
if(!require ('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if(!require ('data.table')) {install.packages('data.table')};library('data.table')
if(!require ('scales')) {install.packages('scales')};library('scales')
if(!require ('purrr')) {install.packages('purrr')};library('purrr')
if(!require ('readr')) {install.packages('readr')};library('readr')
if(!require ('vroom')) {install.packages('vroom')};library('vroom')
if(!require ('INLA')) {install.packages('INLA')};library('INLA')

# Fixando o diretório de trabalho

setwd(gsub("RESPIRATORIAS.*","RESPIRATORIAS/COVID_R/",getwd()))
getwd()

source("Nowcasting/nowcasting_INLA_secretarias-main/nowcasting_fun.r")
source("Nowcasting/Scripts/nowcasting_inla.R")
source("Nowcasting/Scripts/dados_w.R")

## Loading a base do dia
load("boletim.rData")

## Nivel de agregação
agregado <- unique(boletim$`17DRS`)
agregado<-sort(agregado)

## Objetos de armazenamento de plots e a estimativa de nowcasting
nowcast_list_total<-vector("list", length(agregado))
nowcast_list_age<-vector("list", length(agregado))

## Cria o diretório do run com a data do dia
if(!dir.exists(paste0("Nowcasting/Outputs/Tables/", Sys.Date()))){
  dir.create(path = paste0("Nowcasting/Outputs/Tables/", Sys.Date()))
}

## Looping para rodar o nowcasting para cada nível
for (i in seq(1:length(agregado))) {
  print(paste0("Started Nowcasting for ", agregado[i], sep = " "))
  
  ## Filtrando no nível de agregação
  dados_covid <- boletim %>% 
    filter(classi == 'COVID-19')  %>% 
    filter(`17DRS` == agregado[i])
  
  ## Função de Nowcasting
  nowcast_list <- nowcasting_inla(boletim = dados_covid, data.by.week = T)
  
  nowcast_list_total[[i]]<-nowcast_list$total
  nowcast_list_age[[i]]<-nowcast_list$age
  
  ## Dando stamp do nível de agregação
  nowcast_list_total[[i]]$DRS<-agregado[i]
  nowcast_list_age[[i]]$DRS<-agregado[i]
  
  ## Salvando Nowcasting Total
  vroom_write(nowcast_list_total[[i]], 
              file = paste0("Nowcasting/Outputs/Tables/", 
                            Sys.Date(), 
                            "/nowcasting_total_", 
                            Sys.Date(), 
                            "_",
                            agregado[i], 
                            ".csv.xz"))
  
  ## Salvando Nowcasting por idade
  vroom_write(nowcast_list_age[[i]], 
              file = paste0("Nowcasting/Outputs/Tables/", 
                            Sys.Date(),
                            "/nowcasting_age_", 
                            Sys.Date(), 
                            "_",
                            agregado[i], 
                            ".csv.xz"))
  
  # ## Summary para entrada no Nowcasting
  dados_w.plot_total<-nowcast_list[[3]] %>%
    group_by(DT_SIN_PRI) %>%
    tally()
  
  # Salvando curvas
  vroom_write(dados_w.plot_total,
              file = paste0("Nowcasting/Outputs/Tables/",
                            Sys.Date(),
                            "/dados_w_plot_total_",
                            Sys.Date(),
                            "-",
                            agregado[i],
                            ".csv.xz"))
  
  ## Summary para entrada no Nowcasting por idade
  dados_w.plot_age<-nowcast_list[[3]] %>%
    group_by(DT_SIN_PRI, fx_etaria) %>%
    tally()
  
  ## Salvando curvas por idade
  vroom_write(dados_w.plot_age,
              file = paste0("Nowcasting/Outputs/Tables/",
                            Sys.Date(),
                            "/dados_w_plot_age_",
                            Sys.Date(),
                            "-",
                            agregado[i],
                            ".csv.xz"))
  
  print(paste0("Finished Nowcasting for ", agregado[i], sep = " "))
  
  gc()
}


## Salvando Nowcasting total
## Total
nowcast_total<-nowcast_list_total %>% 
  bind_rows(.id = "DRS")

vroom_write(nowcast_total, 
            file = paste0("Nowcasting/Outputs/Tables/", 
                          Sys.Date(), 
                          "_",
                          "nowcasting_total_all_DRS.csv.xz"))
## Age
nowcast_age<-nowcast_list_age %>% 
  bind_rows(.id = "DRS")

vroom_write(nowcast_list, 
            file = paste0("Nowcasting/Outputs/Tables/", 
                          Sys.Date(), 
                          "_",
                          "nowcasting_age_all_DRS.csv.xz"))


