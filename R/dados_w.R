dados.w<-function(dados){
  ## Data da ultima digitacao
  trim.data <-  2 ## finais da sÃ©rie para descartar. Por algumas
  ## tentativas com o dado do muncÃ­pio sugerimos no
  ## mÃ­nimo trÃªs dias. Caso o resultado pareÃ§a estranho,
  ## verifique se muda muito se aumentar mais 1-2 dias.
  
  ## Data mÃ¡xima de digitaÃ§Ã£o a considerar
  DT_max <- max(dados$DT_DIGITA  - trim.data, na.rm = T)
  
  # Dia da semana da ultima digitacao
  DT_max_diadasemana <- as.integer(format(DT_max, "%w"))
  weekdays(DT_max)
  
  # # Ultima digitacao no sÃ¡bado?
  # DT_max <- DT_max - ifelse( DT_max_diadasemana < 6, 
  #                            DT_max_diadasemana + 1,
  #                            0)
  
  
  dados.w <- dados %>% 
    mutate(IDADE = NU_IDADE_N) %>% 
    ##filter(DT_DIGITA <= DT_max, epiyear(DT_SIN_PRI) == 2021) %>%
    filter(DT_DIGITA <= DT_max, epiyear(DT_SIN_PRI) >= 2021) %>%
    #filter(DT_SIN_PRI > ymd("2021-08-31"), NU_IDADE_N >=0, NU_IDADE_N <120) %>% 
    drop_na(IDADE) %>% 
    mutate(
      # Alterando a data para o primeiro dia da semana 
      # Ex. se ultimo dado for de um domingo, entao a semana 
      # comeca na 2a anterior, se termina 5a, entao comeÃ§a 6a
      DT_SIN_PRI = DT_SIN_PRI - 
        as.integer(format(DT_SIN_PRI, "%w")) - 
        (6-DT_max_diadasemana),
      #DT_DIGITA = DT_DIGITA2 - as.integer(format(DT_DIGITA2, "%w")),
      DT_DIGITA = DT_DIGITA - 
        as.integer(format(DT_DIGITA, "%w")) - 
        (6-DT_max_diadasemana),
      Delay = as.numeric(DT_DIGITA - DT_SIN_PRI) / 7,
      fx_etaria = cut(IDADE, 
                      breaks = c(0,4,11,18,30,seq(50,80,by=10),200), 
                      labels = 1:9,
                      right = F)
      # fx_etaria = cut(NU_IDADE_N, breaks = c(seq(0,80,by=10),200), 
      #                 labels =  1:9,
      #                 right = F)
    ) %>%
    drop_na(fx_etaria) %>%
    filter(Delay >= 0)
}