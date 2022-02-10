#!/usr/bin/env Rscript
if(!require ('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if(!require ('data.table')) {install.packages('data.table')};library('data.table')
# if(!require ('NobBS')) {install.packages('NobBS')};library('NobBS')
if(!require ('scales')) {install.packages('scales')};library('scales')
# if(!require ('forecast')) {install.packages('forecast')};library('forecast')
if(!require ('purrr')) {install.packages('purrr')};library('purrr')
if(!require ('readr')) {install.packages('readr')};library('readr')
if(!require ('vroom')) {install.packages('vroom')};library('vroom')

setwd("G:/CCD/CVE/RESPIRATORIAS/COVID_R/")
getwd()


DRS_name<- c("ARAÇATUBA","ARARAQUARA","BAIXADA SANTISTA","BARRETOS","BAURU","CAMPINAS","FRANCA","GRANDE SÃO PAULO","MARÍLIA",
             "PIRACICABA","PRESIDENTE PRUDENTE","REGISTRO","RIBEIRÃO PRETO","SÃO JOÃO DA BOA VISTA","SÃO JOSÉ DO RIO PRETO","SOROCABA",
             "TAUBATÉ") 

## Loading arquivos de Nowcasting
dados_w.plot_list_total<-list.files(path = "Nowcasting/Outputs/Tables/", 
                              pattern = paste0("dados_w_plot_total", Sys.Date()), full.names = T)
dados_w.plot_total<-lapply(dados_w.plot_list_total, function(x){
  x<-vroom(x)
})
names(dados_w.plot_total)<-DRS_name
dados_w.plot_total<-dados_w.plot_total %>% 
  bind_rows(.id = "DRS")

dados_w.plot_list_age<-list.files(path = "Nowcasting/Outputs/Tables/", 
                                    pattern = paste0("dados_w_plot_age", Sys.Date()), full.names = T)
dados_w.plot_age<-lapply(dados_w.plot_list_age, function(x){
  x<-vroom(x)
})
names(dados_w.plot_age)<-DRS_name
dados_w.plot_age<-dados_w.plot_age %>% 
  bind_rows(.id = "DRS")

nowcast_list_total<-list.files(path = "Nowcasting/Outputs/Tables/", 
                               pattern = paste0("nowcasting_total_", Sys.Date()), full.names = T)
nowcast_list_total<-lapply(nowcast_list_total, function(x){
  x<-vroom(x)
})
names(nowcast_list_total)<-DRS_name
nowcast_list_total<-nowcast_list_total %>% 
  bind_rows(.id = "DRS")


nowcast_list_age<-list.files(path = "Nowcasting/Outputs/Tables/", 
                             pattern = paste0("nowcasting_age_", Sys.Date()), full.names = T)
nowcast_list_age<-lapply(nowcast_list_age, function(x){
  x<-vroom(x)
})
names(nowcast_list_age)<-DRS_name
nowcast_list_age<-nowcast_list_age %>% 
  bind_rows(.id = "DRS")

## Objetos de armazenamento de plots e a estimativa de nowcasting
plot_now<-vector("list", length(agregado))
names(plot_now)<-DRS_name
# 
fx.txt <- c("0 - 4", "5 - 11", "12 - 17", "18 - 29",
            "30 - 49", "50 - 59", "60 - 69",
            "70 - 79", "80 +")

## Objetos de plots 
for (i in 1:length(DRS_name)) {
  dados_w.plot_drs_total<-dados_w.plot_total %>% 
    filter(DRS == DRS_name[i])
  
  trim.data <-  1 ## quanto cortar da estimativa
  DT_max <- max(dados_w.plot_drs_total$DT_SIN_PRI  - trim.data)
  
  dados_w.plot_drs_age<-dados_w.plot_age %>% 
    filter(DRS == DRS_name[i]) %>%   
    mutate(`faixa etaria` = factor(x = fx_etaria, 
                                   levels = as.character(1:9), 
                                   labels = fx.txt),
    )
  
  
  nowcast_list_total_drs<-nowcast_list_total %>% 
    filter(DRS == DRS_name[i]) 
  
  # n_max_total<-max(dados_w.plot_drs_total$n)
  # 
  # nowcast_list_total_drs<-nowcast_list_age_drs %>% 
  #   mutate(LS_new = if_else(LSb > n_max_total, 2*n_max_total, LSb))
  
  nowcast_list_age_drs<-nowcast_list_age %>% 
    filter(DRS == DRS_name[i]) %>%   
    mutate(`faixa etaria` = factor(x = fx_etaria, 
                                   levels = as.character(1:9), 
                                   labels = fx.txt),
    ) 
  # %>% 
  #   mutate(LS_new = if_else(LSb > n_max_total, 2*n_max_total, LSb))
  
  
  
  plot_now[[i]]$total<- dados_w.plot_drs_total %>% 
    # filter(DT_SIN_PRI <= min(nowcast_list_total_drs$dt_event)) %>% 
    ggplot(aes(x = DT_SIN_PRI, y = n)) +
    geom_line(aes(), size = .5) + 
    geom_line(data = nowcast_list_total_drs, 
              mapping = aes(x = dt_event, y = Median),
              color = "red", size = .3) +
    geom_ribbon(data = nowcast_list_total_drs,
                mapping = aes(x = dt_event, y = Median,
                              ymin = LI, ymax = LSb),
                fill = "red", alpha = 0.3) +
    theme_bw(base_size = 14)
  plot_now[[17]]$total
  
  plot_now[[i]]$age <- dados_w.plot_drs_age %>% 
    # filter(DT_SIN_PRI <= min(nowcast_list_total_drs$dt_event)) %>% 
    ggplot(aes(x = DT_SIN_PRI, y = n)) + 
    geom_line(show.legend = F) +
    geom_ribbon(data = nowcast_list_age_drs,
                mapping = aes(x = dt_event, y = Median, ymin = LIb, ymax = LSb), fill = "firebrick1",
                color = NA, alpha = 0.2, show.legend = F) +
    geom_line(data = nowcast_list_age_drs,
              aes(y = Median, x = dt_event), color = "firebrick1", show.legend = F) +
    theme_bw(base_size = 14) +
    facet_wrap(~`faixa etaria`, nrow = 3, scales = "free_y")
    
  plot_now[[17]]$age
  
  p1 <-
    gridExtra::grid.arrange(
      plot_now[[i]]$total +
        labs(
          x = "Semana de primeiros sintomas",
          y = "Hospitalizações por SRAG-COVID",
          title = paste0("DRS ", DRS_name[i]),
          subtitle = paste0("casos notificados até ",
                            format(DT_max, "%d/%m/%Y"))) + 
        scale_x_date(date_breaks = "5 weeks",date_labels = "%V"),
      plot_now[[i]]$age +
        labs(
          x = "Semana de primeiros sintomas",
          y = "Hospitalizações por SRAG-COVID",
          title = paste0("DRS ", DRS_name[i]),
          subtitle = paste0("casos notificados até ",
                            format(DT_max, "%d/%m/%Y"))) +
        scale_x_date(date_breaks = "10 weeks", date_labels = "%V"),
      nrow = 1) 
  
  # p1
  # Salvando o plot
  ggsave(plot = p1, filename = paste0('Nowcasting/Outputs/Plots/NC_new-',
                                      Sys.Date(),"_",
                                      agregado[i],
                                      '.png'),
         width = 9, height = 7, dpi = 300)
}

#


