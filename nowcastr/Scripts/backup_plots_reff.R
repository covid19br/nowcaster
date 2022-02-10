#!/usr/bin/env Rscript
if(!require ('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if(!require ('data.table')) {install.packages('data.table')};library('data.table')
if(!require ('scales')) {install.packages('scales')};library('scales')
if(!require ('purrr')) {install.packages('purrr')};library('purrr')
if(!require ('readr')) {install.packages('readr')};library('readr')
if(!require ('vroom')) {install.packages('vroom')};library('vroom')
if(!require ('EpiEstim')) {install.packages('EpiEstim')};library('EpiEstim')
if(!require ('bimets')) {install.packages('bimets')};library('bimets')

# Fixando o diretório de trabalho
setwd("G:/CCD/CVE/RESPIRATORIAS/COVID_R/")
getwd()

source("Nowcasting/Scripts/gde_sp2.R")

## Loading a base do dia
load("boletim.rData")

## Nivel de agregação, padrão usado de DRs
agregado <- unique(boletim$`17DRS`)

##Objetos para armazenar
## DRS
drs_list<-vector("list", length(agregado))
# ## Plot
# plot_list<-vector("list", length(agregado))

## looping para os Reff por DRS
for (i in seq(1:length(agregado))) {
  
  dados_covid <- boletim %>% 
    filter(classi == 'COVID-19')  %>% 
    select(`17DRS`, DT_SIN_PRI) %>% 
    rename(dates = DT_SIN_PRI) %>%
    mutate(dates = as.Date(dates)) %>%
    group_by(`17DRS`, dates) %>%          
    summarize( I = n())
  
  drs_list[[i]] <- dados_covid %>%
    filter(`17DRS` == agregado[i]) %>%
    complete(dates = seq.Date(min(dates), max(dates), by = "day")) %>%
    replace_na(list(I = 0))
  drs_list[[i]]$I <- cumsum(drs_list[[i]]$I)
  
  plot_list[[i]]<-ggplot(drs_list[[i]], aes(x = dates, y = I)) +
    geom_point() +
    labs(x = "Data", y = "Casos") +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    labs( subtitle = "(SE06-SE48, 2020)",
          x = "Data do início dos sintomas",
          y = "Casos de SRAG COVID-19 (log10)")+
    annotation_logticks() +
    ggtitle(paste0(agregado[i],"_","DRS",sep = "")) +
    theme_bw()
  
  ggsave(filename = paste0("Nowcasting/Outputs/Plots/LOG_cum-",agregado[i],"_","DRS",".png",sep = ""),
         width = 12, height = 8, dpi = 300)
}

dados <- boletim %>%
  rename(DRS = `17DRS`)

###########################################################
######      SELECIONE O N?MERO M?NIMO DE DIAS        ######
minimo_dias <- 10
hoje <- Sys.Date()-120
ate <- Sys.Date()-15
dados <- boletim %>% 
  filter(DT_SIN_PRI>=hoje, DT_SIN_PRI<=ate) %>% 
  rename(DRS = `17DRS`, 
         macro = macroregiao)

agregado <- unique(dados$DRS)

# reff_list<-vector("list", )

for (i in seq(1:length(agregado))) {
  
  f <- paste0('DT_SIN_PRI + ', agregado[i],' ~ classi')
  dados_an <- do.call("dcast", list(as.formula(f), data=as.name("dados")))
  dados_an$COVID_PREV <- round(dados_an$`COVID-19` + 
                                 dados_an$`Em investigacao`*(dados_an$`COVID-19`/(dados_an$`COVID-19`+ 
                                                                                    dados_an$Influenza +
                                                                                    dados_an$`Outros agentes` +
                                                                                    dados_an$`Outros virus` +
                                                                                    dados_an$`SRAG nao especificado`)))
  dados_an$COVID_PREV[is.na(dados_an$COVID_PREV)] <- 0
  dados_covid <- dados_an %>%
    select(.data[[agregado[i]]], DT_SIN_PRI, COVID_PREV, `COVID-19`, `Em investigacao`) %>%
    rename(dates = DT_SIN_PRI, I = COVID_PREV, COVID_NOT = `COVID-19`, INVESTIG = `Em investigacao`) %>%
    mutate(dates = as.Date(dates))
  
  ids_agregados <- distinct(dados_covid, .data[[agregado[i]]])
  # res_list<-vector("list", length(ids_agregados))
  # names(res_list)<-ids_agregados
  for (j in seq(1:lengths(ids_agregados))) {
    nivel_agregado <- dados_covid %>%
      filter(.data[[agregado[i]]] == ids_agregados[[1]][j]) %>%
      complete(dates = seq.Date(min(dates), max(dates), by = "day")) %>%
      replace_na(list(I = 0, COVID_NOT = 0, INVESTIG = 0))
    
    ggplot(nivel_agregado) +
      geom_point(mapping = aes(x = dates, y = I, color = 'red'), alpha = 0.6) +
      geom_line(mapping = aes(x = dates, y = I, color = 'red'), linetype = "dashed", size=0.1, alpha = 0.6) +
      geom_point(mapping = aes(x = dates, y = COVID_NOT, color = 'navy'), alpha = 0.6) +
      geom_line(mapping = aes(x = dates, y = COVID_NOT, color = 'navy'), alpha = 0.6) +
      geom_point(mapping = aes(x = dates, y = INVESTIG, color = 'darkgreen'), alpha = 0.6) +
      geom_line(mapping = aes(x = dates, y = INVESTIG, color = 'darkgreen'), alpha = 0.6) +
      scale_color_identity(name = "Legenda",
                           breaks = c("red", "navy", "darkgreen"),
                           labels = c("Estimado", "Observado", "Em investigação"),
                           guide = "legend") +
      labs(x = "Data", y = "Casos") +
      ggtitle(paste(agregado[i],"-",ids_agregados[[1]][j],sep = ""))+
      theme_bw()+
      theme(legend.position = "bottom")
    
    ggsave(filename = paste("Nowcasting/Outputs/Plots/Inc_Corrig-", agregado[i], "-", ids_agregados[[1]][j], ".png", sep = ""),
           width = 12, height = 8, dpi = 300)
    nivel_agregado1 <- nivel_agregado %>%
      select(dates,I)
    
  }