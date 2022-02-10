#!/usr/bin/env Rscript
if(!require ('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if(!require ('data.table')) {install.packages('data.table')};library('data.table')
if(!require ('scales')) {install.packages('scales')};library('scales')
if(!require ('purrr')) {install.packages('purrr')};library('purrr')
if(!require ('readr')) {install.packages('readr')};library('readr')
if(!require ('vroom')) {install.packages('vroom')};library('vroom')
if(!require ('EpiEstim')) {install.packages('EpiEstim')};library('EpiEstim')
if(!require ('bimets')) {install.packages('bimets')};library('bimets')
if(!require ('reshape2')) {install.packages('reshape2')};library('reshape2')

# Fixando o diretório de trabalho

setwd(gsub("RESPIRATORIAS.*","RESPIRATORIAS/COVID_R/",getwd()))
getwd()

source("Nowcasting/Scripts/gde_sp2.R")

## Loading a base do dia
load("boletim.rData")

## Nivel de agregação, padrão usado de DRs
agregado <- unique(boletim$`17DRS`)

## Cria o diretório do run com a data do dia
if(!dir.exists(paste0("Nowcasting/Outputs/Tables/", Sys.Date()))){
  dir.create(path = paste0("Nowcasting/Outputs/Tables/", Sys.Date()))
}

###########################################################
######      SELECIONE O NÚMERO MÍNIMO DE DIAS        ######
minimo_dias <- 10
hoje <- Sys.Date()-120
ate <- Sys.Date()-15
dados <- boletim %>% 
  filter(DT_SIN_PRI>=hoje, DT_SIN_PRI<=ate) %>% 
  rename(DRS = `17DRS`, 
         macro = macroregiao)

## Nível de agregação
agregado <- unique(dados$DRS)

## Objeto de armazenamento
res_list<-vector("list", length(agregado))
names(res_list)<-agregado

f <- paste0('DT_SIN_PRI + DRS ~ classi')
dados_an <- do.call("dcast", list(as.formula(f), data=as.name("dados")))
dados_an$COVID_PREV <- round(dados_an$`COVID-19` + 
                               dados_an$`Em investigacao`*(dados_an$`COVID-19`/(dados_an$`COVID-19`+ 
                                                                                  dados_an$Influenza +
                                                                                  dados_an$`Outros agentes` +
                                                                                  dados_an$`Outros virus` +
                                                                                  dados_an$`SRAG nao especificado`)))
dados_an$COVID_PREV[is.na(dados_an$COVID_PREV)] <- 0
dados_covid <- dados_an %>%
  select(DRS, DT_SIN_PRI, COVID_PREV, `COVID-19`, `Em investigacao`) %>%
  rename(dates = DT_SIN_PRI, I = COVID_PREV, COVID_NOT = `COVID-19`, INVESTIG = `Em investigacao`) %>%
  mutate(dates = as.Date(dates))

## Looping para Reff por nível agregado
for (j in seq(1:length(agregado))) {
  
  ## Filtro nos dados conforme parametros
  nivel_agregado <- dados_covid %>%
    filter(DRS == agregado[j]) %>%
    complete(dates = seq.Date(min(dates), max(dates), by = "day")) %>%
    replace_na(list(I = 0, COVID_NOT = 0, INVESTIG = 0))
  
  ## Data.frame reff ready
  nivel_agregado1 <- nivel_agregado %>%
    select(dates,I)
  
  if (count(nivel_agregado1 %>% 
            filter(nivel_agregado1$I>0))>minimo_dias) {
    
    ## Estimativa de Reff
    res <- estimate_R(incid = nivel_agregado1,
                      method = "parametric_si",
                      config = make_config(list(mean_si = 4.7, std_si = 2.9)))
    
    ## Ajuste de datas no reff
    datas<-res$dates[length(res$dates) - (99:0)]
    names(res$R) <- gsub("\\(R\\)", ".R", names(res$R))
    res$R$data.inicio <- datas[res$R$t_start]
    res$R$data.fim <- datas[res$R$t_end]
    
    ## Salvando reff para a DRS
    vroom_write(res$R, 
                file = paste0("Nowcasting/Outputs/Tables/", 
                              Sys.Date(), 
                              "/reff_estimates_DRS_", 
                              agregado[j], 
                              "_",
                              Sys.Date(), 
                              ".csv.xz"))
    
    res_list[[j]]<-res$R
    res_list[[j]]$DRS<-agregado[j]
  }
}

## Salvando reff para todas as DRS
res_list<-res_list %>% 
  bind_rows(.id = "DRS")

vroom_write(res_list, 
            file = paste0("Nowcasting/Outputs/Tables/", 
                          Sys.Date(), 
                          "reff_estimates_all_DRS.csv.xz"))

#


