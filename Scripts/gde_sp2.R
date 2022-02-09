gde_sp2<-function(base_nova){
  base_nova$gde_sp2 [base_nova$ID_RG_RESI=="GVE I CAPITAL"] <- "MSP"
  base_nova$gde_sp2 [base_nova$ID_RG_RESI=="GVE X OSASCO"] <- "Gde Sao Paulo"
  base_nova$gde_sp2 [base_nova$ID_RG_RESI=="GVE IX FRANCO DA ROCHA"] <- "Gde Sao Paulo"
  base_nova$gde_sp2 [base_nova$ID_RG_RESI=="GVE VIII MOGI DAS CRUZES"] <- "Gde Sao Paulo"
  base_nova$gde_sp2 [base_nova$ID_RG_RESI=="GVE VII SANTO ANDRE"] <- "Gde Sao Paulo"
  base_nova$gde_sp2 [is.na(base_nova$gde_sp2)] <- "Interior"
}

# base_nova<-dados
