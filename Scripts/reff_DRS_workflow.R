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

## Nivel de agregaÃ§Ã£o, padrÃ£o usado de GVEs
agregado <- unique(boletim$`17DRS`)

##Objetos para armazenar
## DRS
drs_list<-vector("list", length(agregado))
## Plot
plot_list<-vector("list", length(agregado))

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

dados$gde_sp2 [dados$ID_RG_RESI=="GVE I CAPITAL"] <- "MSP"
dados$gde_sp2 [dados$ID_RG_RESI=="GVE X OSASCO"] <- "Gde Sao Paulo"
dados$gde_sp2 [dados$ID_RG_RESI=="GVE IX FRANCO DA ROCHA"] <- "Gde Sao Paulo"
dados$gde_sp2 [dados$ID_RG_RESI=="GVE VIII MOGI DAS CRUZES"] <- "Gde Sao Paulo"
dados$gde_sp2 [dados$ID_RG_RESI=="GVE VII SANTO ANDRE"] <- "Gde Sao Paulo"
dados$gde_sp2 [is.na(dados$gde_sp2)] <- "Interior"
###########################################################

# agregado <- c('SG_UF', 'DRS', 'ID_RG_RESI', 'ID_MN_RESI')
# agregado<-c('SG_UF', 'DRS')
agregado <- c('SG_UF', 'DRS', 'macro')

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
    
    if (count(nivel_agregado1 %>% filter(nivel_agregado1$I>0))>minimo_dias) {
      res <- estimate_R(incid = nivel_agregado1,
                        method = "parametric_si",
                        config = make_config(list(mean_si = 4.7, std_si = 2.9)))
      
      datas<-res$dates[length(res$dates) - (99:0)]
      names(res$R) <- gsub("\\(R\\)", ".R", names(res$R))
      res$R$data.inicio <- datas[res$R$t_start]
      res$R$data.fim <- datas[res$R$t_end]
      
      vroom_write(res$R, 
                  file = paste0("Nowcasting/Outputs/Tables/reff_estimtes_", agregado[i], "_", ids_agregados[[1]][j], ".csv.xz"))
      
      # res_list[[i]]<-res$R
      
      reff_plot<-res$R %>%  
        # mutate(data = as.Date(data)) %>%
        ggplot(aes(x = data.fim, y = Mean.R)) +
        geom_ribbon(aes(ymin = Quantile.0.025.R, ymax = Quantile.0.975.R), fill = "lightgrey") +
        geom_line(size = 1.25, colour = RColorBrewer::brewer.pal(4, "Dark2")[3]) +
        scale_x_date( date_labels = "%w/%b", name = "") +
        ylim(min(c(0.8, min(res$R$Quantile.0.025.R))), max(res$R$Quantile.0.975.R)) +
        geom_hline(yintercept = 1, linetype = "dashed", col = "red", size = 1) +
        labs(y = "Número de reprodução da epidemia",
             x = "Data")+
        ggtitle(paste(agregado[i],"-",ids_agregados[[1]][j],sep = ""))+
        theme_bw()+
        theme(legend.position = "bottom")
      
      ggsave(plot = reff_plot,
             file = paste0("Nowcasting/Outputs/Plots/Rt_Corr-", agregado[i], "-", ids_agregados[[1]][j], ".png", sep = ""),
             width = 9,
             height = 7,
             dpi = 300
      )
      # plot(res)
      # dev.off()
    }
  }
}


## Gráfico

msp_reff<-vroom("Nowcasting/Outputs/Tables/reff_estimtes_gde_sp2_MSP.csv.xz")
gdesp_reff<-vroom("Nowcasting/Outputs/Tables/reff_estimtes_gde_sp2_Gde Sao Paulo.csv.xz")
interior_reff<-vroom("Nowcasting/Outputs/Tables/reff_estimtes_gde_sp2_Interior.csv.xz")
sg_uf_ref<-vroom("Nowcasting/Outputs/Tables/reff_estimtes_SG_UF_SP.csv.xz")

ggplot() +
  # geom_line(data = sg_uf_ref, 
  #           aes(x = data.fim, y = Mean.R, col = "Estado"), size = 1.25)+
  # geom_ribbon(data = sg_uf_ref,
  #             aes(x = data.fim, y = Mean.R,
  #                 ymin = Quantile.0.025.R, ymax = Quantile.0.975.R, fill = "Estado"), alpha = 0.1) +
  geom_line(data = gdesp_reff, 
            aes(x = data.fim, y = Mean.R, col = "Gde. SP"), size = 1.25) +
  # geom_ribbon(data = gdesp_reff,
  #             aes(x = data.fim, y = Mean.R,
  #                 ymin = Quantile.0.025.R, ymax = Quantile.0.975.R, fill = "Gde. SP"), alpha = 0.1) +
  geom_line(data = msp_reff,
            aes(x = data.fim, y = Mean.R, col = "Capital"), size = 1.25)+
  # geom_ribbon(data = msp_reff,
  #             aes(x = data.fim, y = Mean.R,
  #                 ymin = Quantile.0.025.R, ymax = Quantile.0.975.R, fill = "Capital"), alpha = 0.1) +
  geom_line(data = interior_reff, 
            aes(x = data.fim, y = Mean.R, col = "Interior"), size = 1.25)+
  # geom_ribbon(data = interior_reff,
  #             aes(x = data.fim, y = Mean.R,
  #                 ymin = Quantile.0.025.R, ymax = Quantile.0.975.R, fill = "Interior"), alpha = 0.1) +
  scale_x_date( date_labels = "%d/%b/%y", name = "Data") +
  ylim(min(c(0.8, min(res$R$Quantile.0.025.R))), max(res$R$Quantile.0.975.R)) +
  geom_hline(yintercept = 1, linetype = "dashed", col = "red", size = 1) +
  scale_color_manual(name = "", values = c("firebrick3", "darkgreen", "cyan3"))+
  # scale_color_viridis_d(name='', option = "viridis")+
  labs(y = "Número de reprodução da epidemia",
       x = "Data", 
       title = "Gde. SP, Capital e Interior")+
  # ggtitle(paste(agregado[i],"-",ids_agregados[[1]][j],sep = ""))+
  theme_bw()+
  theme(legend.position = "bottom")


macro_list<-list.files(path = "Nowcasting/Outputs/Tables/", pattern = "macro", full.names = T)
macro_list<-lapply(macro_list, function(x){
  x<-vroom(x)
})
names(macro_list)<-c("CENTRO_LESTE", "CENTRO_OESTE", "NORDESTE", "NOROESTE", "SUL_SUDESTE")
macro_list<-macro_list %>% 
  bind_rows(.id="macro")
  
macro_list %>% 
  ggplot() +
  geom_line(aes(x = data.fim, y = Mean.R, col = macro), size = 1.25)+
  # geom_ribbon(aes(x = data.fim, y = Mean.R,
  #                 ymin = Quantile.0.025.R, ymax = Quantile.0.975.R, fill = macro), alpha = 0.1) +
  scale_x_date( date_labels = "%w/%b", name = "Data") +
  ylim(min(c(0.8, min(res$R$Quantile.0.025.R))), max(res$R$Quantile.0.975.R)) +
  geom_hline(yintercept = 1, linetype = "dashed", col = "red", size = 1) +
  scale_color_viridis_d(name = "", option = "cividis", aesthetics = c("colour", "fill"))+
  labs(y = "Número de reprodução da epidemia",
       x = "Data", 
       title = "Macroregião")+
  # ggtitle(paste(agregado[i],"-",ids_agregados[[1]][j],sep = ""))+
  theme_bw()+
  theme(legend.position = "bottom")+
  facet_wrap(.~macro, ncol = 1)
  
