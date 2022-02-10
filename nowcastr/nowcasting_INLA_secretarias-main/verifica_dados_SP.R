## Criar diretório ./Data/SP para baixar os arquivos
system("cd Data/SP/; ../../baixa_dados_MSP.sh ; cd ../../")

tmp <- paste0("Data/SP/", dir.dados)

pmsp <- lapply(tmp, function(x){
    vroom(x)%>% 
        filter(ORIGEM == 2) %>% ## ORIGEM == 1 SIVEP, ORIGEM == 2 E-SUS, ver dicionario de dados
        ## filtro sragnofever do InfoGripe (tosse|gargata) & (algum sinal de desconf resp) & (hospital|obito)
        ## filter((TOSSE == 1 | GARGANTA==1) &
        ##        (DISPNEIA == 1 | DESC_RESP == 1) &
        ##        (HOSPITAL == 1  | EVOL_COMUM == 2)) %>% 
               select(-SEM_PRI, -CO_UN_INTE)
               ##select(DT_DIGITA, DT_SIN_PRI, IDADE)
        }) %>%
    bind_rows() %>% ungroup()

tab1 <-
    pmsp %>%
    filter(DT_DIGITA >= ymd("2020-01-01")) %>% 
    group_by(CLASSI_FIN_COVID) %>%
    summarise(n.classi.fin = n())

tab2 <-
    pmsp %>%
    filter(DT_DIGITA >= ymd("2020-01-01")) %>% 
    group_by(CLASSI_FIN_COVID_MOD) %>%
    summarise(n.classi.fin.mod = n()) %>%
    merge(tab1, by.x="CLASSI_FIN_COVID_MOD", by.y="CLASSI_FIN_COVID") 
            


tab2$criterio = c("Laboratorial", "Clinico Epidemiologico", "Imagem", "Clinico", "Descartado", "SG Não especif.", "NA")


names(tab2)[1] <- "Cod_criterio"

tab2 <- tab2[,c(1,4,3,2)]

tab2$variacao.percentual <- with(tab2, 100*(n.classi.fin.mod - n.classi.fin)/n.classi.fin)
    
apply(tab2[1:4,3:4], 2, sum)


apply(tab2[,3:4], 2, sum)

write.csv2(tab2, "outputs/MSP_esus_ncasos_criterios.csv", row.names=FALSE)
