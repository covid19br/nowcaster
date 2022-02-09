#Antes de rodar, criar uma pasta "RT_log" na pasta do dia

if(!require ('tidyverse')) {install.packages('tidyverse')}
if(!require ('data.table')) {install.packages('data.table')}
if(!require ('bimets')) {install.packages('bimets')}
if(!require ('scales')) {install.packages('scales')}
if(!require('EpiEstim')) {install.packages('EpiEstim')}

library(EpiEstim)
library(tidyverse)
library(data.table)
library(bimets)
library(scales)
library(readr)
# seleciona a pasta a ser usada
# setwd("D:/CVE")

setwd("G:/CCD/CVE/RESPIRATORIAS/COVID_R/")
getwd()

## Pastas
d <- format(Sys.Date(), "%d")
m <- as.numeric(format(Sys.Date(), "%m"))
mes <- data.frame(m = 01:12, mes = c("Janeiro","Fevereiro","Marco","Abril","Maio","Junho"
                                     ,"Julho","Agosto","Setembro","Outubro","Novembro","Dezembro"))
dat <- paste(d,mes[m,2], sep="")
p4 <- paste(mes[m,2],"/",dat,"/", sep="")


p <- paste("G:/CCD/CVE/RESPIRATORIAS/CoronavÌrus/Bancos_SRAG/0_INFO_Obito/Banco_SRAG_boletim",sep="")
q <- paste("G:/CCD/CVE/RESPIRATORIAS/CoronavÌrus/Bancos_SRAG/", p4 ,"RT_log/",sep="")

dados <- read_delim("G:/CCD/CVE/RESPIRATORIAS/CoronavÌrus/Bancos_SRAG/0_INFO_Obito/Banco_SRAG_boletim/novo_base_srag_boletim.csv",
                           ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"),
                           trim_ws = TRUE)
dados <- boletim

hoje <- Sys.Date()-120
ate <- Sys.Date()-30
dados <- filter(dados, dados$DT_SIN_PRI>=hoje, dados$DT_SIN_PRI<=ate)

agregado <- c('SG_UF', '17DRS')

# L√™ banco
# dados <- fread(file = 'base_COVID2_06Jul.csv',
#                sep = ';',
#                quote = "\"",
#                encoding = 'Latin-1',
#                header = TRUE)
# agregado <- c('SG_UF', '17DRS', 'ID_RG_RESI', 'ID_MN_RESI')



for (i in seq(1:length(agregado))) {
  
  dados_covid <- dados %>% 
    filter(classi == 'COVID-19')  %>% 
    select(.data[[agregado[i]]], DT_SIN_PRI) %>% 
    rename(dates = DT_SIN_PRI) %>%
    mutate(dates = as.Date(dates)) %>%
    group_by(.data[[agregado[i]]], dates) %>%          
    summarize( I = n())
  
  ids_agregados <- distinct(dados_covid,.data[[agregado[i]]])
  for (j in seq(1:lengths(ids_agregados))) {
    nivel_agregado <- dados_covid %>%
      filter(.data[[agregado[i]]] == ids_agregados[[1]][j]) %>%
      complete(dates = seq.Date(min(dates), max(dates), by = "day")) %>%
      replace_na(list(I = 0))
    nivel_agregado$I <- cumsum(nivel_agregado$I)
    
    ggplot(nivel_agregado, aes(x = dates, y = I)) +
      geom_point() +
      labs(x = "Data", y = "Casos") +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      labs( subtitle = "(SE06-SE48, 2020)",
            x = "Data do in√≠cio dos sintomas",
            y = "Casos de SRAG COVID-19 (log10)")+
      annotation_logticks() +
      ggtitle(paste(agregado[i],"-",ids_agregados[[1]][j],sep = "")) +
      theme_bw()
    ggsave(filename = paste(q,'LOG_cum-',agregado[i],"-",ids_agregados[[1]][j],".png",sep = ""),
           width = 12, height = 8, dpi = 300)
  }
}

###########################################################
######      SELECIONE O N?MERO M?NIMO DE DIAS        ######
minimo_dias <- 10
###########################################################

dados <- dados %>%
  rename(DRS = `17DRS`)

# agregado <- c('SG_UF', 'DRS', 'ID_RG_RESI', 'ID_MN_RESI')
agregado <- c('SG_UF', 'DRS')

for (i in seq(1:length(agregado))) {
  
  f <- paste('DT_SIN_PRI + ', agregado[i],' ~ classi')
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
                           labels = c("Estimado", "Observado", "Em investiga√ß√£o"),
                           guide = "legend") +
      labs(x = "Data", y = "Casos") +
      ggtitle(paste(agregado[i],"-",ids_agregados[[1]][j],sep = "")) 
    ggsave(filename = paste0("Nowcasting/Outputs/Plots/Inc_Corrig-", agregado[i], "-", ids_agregados[[1]][j], ".png", sep = ""),
           width = 12, height = 8, dpi = 300)
    nivel_agregado1 <- nivel_agregado %>%
      select(dates,I)
    
    if (count(nivel_agregado1 %>% filter(nivel_agregado1$I>0))>minimo_dias) {
      res <- estimate_R(incid = nivel_agregado1,
                        method = "parametric_si",
                        config = make_config(list(mean_si = 4.7, std_si = 2.9)))
      png(
        file = paste("Nowcasting/Outputs/Plots/Rt_Corr-", agregado[i], "-", ids_agregados[[1]][j], ".png", sep = ""),
        width = 1800,
        height = 2200,
        res = 300
      )
      plot(res)
      dev.off()
    }
  }
}

