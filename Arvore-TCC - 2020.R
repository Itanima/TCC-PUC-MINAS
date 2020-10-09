################################################################################
# SCRIPT PARA A GERAÇÃO DAS ÁRVORES DE CLASSIFICAÇÃO
################################################################################
############### PARA O LUCRO REAL/PRESUMIDO
################################################################################

LR_MOTIVO <- as.data.frame(unique(LR_2020$MOTIVO))
i <- row(LR_MOTIVO[1])
for (a in i){
  LR_MOTIVO$Desc[a] <- a
}

names(LR_MOTIVO) <- c("MOTIVO","COD_MOTIVO")
LR1 <- inner_join(LR_2020,LR_MOTIVO,by="MOTIVO")

################################################################################
############### TESTE PARA DIFERENTES HIPÓTESES DE ÁRVORES
################################################################################
arvore <- rpart(SIT_CADASTRAL ~ COD_MOTIVO + QTD ,
                    data = LR1, method = "class")

arvore <- rpart(SIT_CADASTRAL ~ TEMPO_FUNCIONAMENTO + QTD ,
                data = LR1, method = "class")

arvore <- rpart(SIT_CADASTRAL ~ QTD + TEMPO_FUNCIONAMENTO ,
                data = LR1, method = "class")

################################################################################
############### PARA O LUCRO REAL/PRESUMIDO
################################################################################

printcp(arvore)           # Exibe os resultados
plotcp(arvore)            # visualizar os resultados de cross-validation
summary(arvore)           # detalhamento da montagem da árvore)

rpart.plot(prune(arvore, cp = 0.035714),
           tweak = 1.1,
           type = 5,
           branch = .9,
           yesno = F,
           extra = 2, #under = T,
           legend.x = NA,
           gap = 0,space = 0,
#           shadow.col = "GRAY",
           main = "Empresas do Lucro/Presumido\nPeríodo: 2020\n")

rm(a,i,LR1,arvore,LR_MOTIVO,LR2)

############################################################################################
############### PARA O MEI
############################################################################################

MEI_MOTIVO <- as.data.frame(unique(MEI_2020$MOTIVO))
i <- row(MEI_MOTIVO[1])
for (a in i){
  MEI_MOTIVO$Desc[a] <- a
}

names(MEI_MOTIVO) <- c("MOTIVO","COD_MOTIVO")
MEI1 <- inner_join(MEI_2020,MEI_MOTIVO,by="MOTIVO")

arvore <- rpart(SIT_CADASTRAL ~ QTD + TEMPO_FUNCIONAMENTO ,
                data = MEI1, method = "class")


#arvore <- rpart(COD_MOTIVO ~ SIT_CADASTRAL + TEMPO_FUNCIONAMENTO + QTD,
#                data = MEI1, method = "class")

printcp(arvore)           # Exibe os resultados
plotcp(arvore)            # visualizar os resultados de cross-validation
summary(arvore)           # detalhamento da montagem da árvore

rpart.plot(prune(arvore, cp = 0.010000),
           tweak = 1.2,
           type = 5,
           branch = .9,
           yesno = F,
           extra = 2, under = T,
           legend.x = NA,
           gap = 1,space = .5,
           shadow.col = "GRAY",
           main = "Empresas do MEI\nPeríodo: 2020\n") 

rm(a,i,MEI1,arvore,MEI_MOTIVO)

############################################################################################
############### PARA O SN
############################################################################################

SN_MOTIVO <- as.data.frame(unique(SN_2020$MOTIVO))
i <- row(SN_MOTIVO[1])
for (a in i){
  SN_MOTIVO$Desc[a] <- a
}

names(SN_MOTIVO) <- c("MOTIVO","COD_MOTIVO")
SN1 <- inner_join(SN_2020,SN_MOTIVO,by="MOTIVO")

arvore <- rpart(SIT_CADASTRAL ~ QTD + TEMPO_FUNCIONAMENTO ,
                data = SN1, method = "class")

#arvore <- rpart(COD_MOTIVO ~ TEMPO_FUNCIONAMENTO + QTD,
#                data = SN1, method = "class")

printcp(arvore)           # Exibe os resultados
plotcp(arvore)            # visualizar os resultados de cross-validation
summary(arvore)           # detalhamento da montagem da árvore

rpart.plot(prune(arvore, cp = 0.01000000),
           tweak = 1.2,
           type = 5,
           branch = .9,
           yesno = F,
           extra = 2, under = T,
           legend.x = NA,
           gap = 1,space = .5,
           shadow.col = "GRAY",
           main = "Empresas do Simples Nacional\nPeríodo: 2020\n")


rm(a,i,SN1,arvore,SN_MOTIVO)

