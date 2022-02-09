## DADOS de SP ##
## planilhas csv "Casos, SRAG e obitos", com dados da SIVEp e E-sus, baixar de
## https://www.prefeitura.sp.gov.br/cidade/secretarias/saude/vigilancia_em_saude/doencas_e_agravos/coronavirus/index.php?p=313773
## Atualiza os csvs das planilhas e-sus + sivep da SMS SP
## Segundo colegas da SMS-SP as planilhas são atualizadas todos os dias, no fim da manhã
## E pode haver mudanças em planilhas antigas. Então a opção mai segura é baixar todas as planilhas a cada análise
## Além disso, novas planilahs são acrescentadas à medida que avança o ano
## Colsute planilhas csv "Casos, SRAG e obitos", com dados da SIVEp e E-sus em
## https://www.prefeitura.sp.gov.br/cidade/secretarias/saude/vigilancia_em_saude/doencas_e_agravos/coronavirus/index.php?p=313773
## e acrescente à lista os nomes dos novos arquivos

base.url <- "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/saude/arquivos/coronavirus/"
## Acrescente a esta lista nomes das novas planilhas
nomes <- c("dados-sivep-esus-sao-paulo-sp-jan-jul-2020.csv" ,
           "dados-sivep-esus-sao-paulo-sp-ago-nov-2020.csv" ,
           "dados-sivep-esus-sao-paulo-sp-dez-fev-2021.csv" ,
           "dados-sivep-esus-sao-paulo-sp-mar-abr-2021.csv" ,
           "dados-sivep-esus-sao-paulo-sp-mai-jun-2021.csv" ,
           "dados-sivep-esus-sao-paulo-sp-jul-ago-2021.csv" , 
           "dados-sivep-esus-sao-paulo-sp-set-dez-2021.csv",
           "dados-sivep-esus-sao-paulo-sp-jan-fev-2022.csv")

## Loop de leitura Arquivo : com rede lenta dá timeout
indices <- 1:length(nomes)
## Ultimos 5 arquivos
##indices <- (length(nomes)-5): length(nomes)
for(n in nomes[indices])
    ##cat(paste0("wget ",base.url, n),"\n \n")
    download.file(paste0(base.url, n),
                  destfile = paste0("Data/SP/",n),
                  method = "wget",
                  cachedOK = FALSE)

