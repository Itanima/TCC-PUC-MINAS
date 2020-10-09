
##Scrip de leitura e organizaçãodos dados
#################################################################################

MEI <- read_delim(paste0(base_path,"MEI.csv"), ",", escape_double = FALSE, 
                  col_types = cols(ANO_ABERTURA = col_integer(), 
                                   ANO_BAIXA = col_integer(), QTD = col_integer()), 
                  trim_ws = TRUE)
MEI <- as.data.frame(MEI)
summary(MEI)
MEI$MOTIVO[MEI$MOTIVO == "NÃO INFORMADO"] <- "ATIVA"
MEI$ANO_BAIXA[is.na(MEI$ANO_BAIXA)] <- 9999
summary(MEI)

#################################################################################
# Agrupar por UF
UF_QTD <-  MEI %>% 
  group_by(UF) %>% 
  summarise(QTD = sum(QTD))

barplot(UF_QTD$QTD, main = "Quantitativo de Empresas abertas do MEI",sub = "Período: 2005 - 2020", xlab = "Estados", ylab = "Quantidade", 
        names.arg = UF_QTD$UF, cex.names = 0.7, col = "orange")

#################################################################################

MEI_2020<- MEI[MEI$ANO_ABERTURA == 2020,] # Base de Treino 
MEI <- MEI[MEI$ANO_ABERTURA <= 2019,]     # Base de Teste

#################################################################################
#################################################################################
#################################################################################

LR <- read_delim(paste0(base_path,"LR.csv"), ",", escape_double = FALSE, 
                     col_types = cols(ANO_ABERTURA = col_integer(), 
                                      ANO_BAIXA = col_integer(), QTD = col_integer()), 
                     trim_ws = TRUE)
LR <- as.data.frame(LR)
summary(LR)
LR$MOTIVO[LR$MOTIVO == "NÃO INFORMADO"] <- "ATIVA"
LR$ANO_BAIXA[is.na(LR$ANO_BAIXA)] <- 9999
summary(LR)

#################################################################################
# Agrupar por UF
LR_QTD <-  LR %>% 
  group_by(UF) %>% 
  summarise(QTD = sum(QTD))

barplot(LR_QTD$QTD, main = "Quantitativo de Empresas abertas do Lucro Real/Presumido",sub = "Período: 2005 - 2020", xlab = "Estados", ylab = "Quantidade", 
        names.arg = LR_QTD$UF, cex.names = 0.7, col = "RED")

#################################################################################

LR_2020<- LR[LR$ANO_ABERTURA == 2020,] # Base de Treino
LR <- LR[LR$ANO_ABERTURA <= 2019,]     # Base de Teste

#################################################################################
#################################################################################
#################################################################################

SN <- read_delim(paste0(base_path,"SN.csv"), ",", escape_double = FALSE, 
                 col_types = cols(ANO_ABERTURA = col_integer(), 
                                  ANO_BAIXA = col_integer(), QTD = col_integer()), 
                 trim_ws = TRUE)
summary(SN)
SN$MOTIVO[SN$MOTIVO == "NÃO INFORMADO"] <- "ATIVA"
SN$ANO_BAIXA[is.na(SN$ANO_BAIXA)] <- 9999
summary(SN)

#################################################################################

# Agrupar por UF
SN_QTD <-  SN %>% 
  group_by(UF) %>% 
  summarise(QTD = sum(QTD))

barplot(SN_QTD$QTD, main = "Quantitativo de Empresas abertas do Simples Nacional",sub = "Período: 2005 - 2020", xlab = "Estados", ylab = "Quantidade", 
        names.arg = SN_QTD$UF, cex.names = 0.7, col = "blue")


SN_2020<- SN[SN$ANO_ABERTURA == 2020,]  # Base de Treino
SN <- SN[SN$ANO_ABERTURA <= 2019,]      # Base de Teste

#################################################################################
#################################################################################
#################################################################################

MEI$ANO_BAIXA[MEI$ANO_BAIXA == 9999] <- 2020
MEI$TEMPO_FUNCIONAMENTO <- MEI$ANO_BAIXA - MEI$ANO_ABERTURA
MEI$ANO_BAIXA[MEI$SIT_CADASTRAL == "Ativa"] <- 9999

#################################################################################

SN$ANO_BAIXA[SN$ANO_BAIXA == 9999] <- 2020
SN$TEMPO_FUNCIONAMENTO <- SN$ANO_BAIXA - SN$ANO_ABERTURA
SN$ANO_BAIXA[SN$SIT_CADASTRAL == "Ativa"] <- 9999

#################################################################################

LR$ANO_BAIXA[LR$ANO_BAIXA == 9999] <- 2020
LR$TEMPO_FUNCIONAMENTO <- LR$ANO_BAIXA - LR$ANO_ABERTURA
LR$ANO_BAIXA[LR$SIT_CADASTRAL == "Ativa"] <- 9999
LR <- LR[LR$TEMPO_FUNCIONAMENTO >= 0,]
#################################################################################
#################################################################################
#################################################################################

MEI_2020$ANO_BAIXA[MEI_2020$ANO_BAIXA == 9999] <- 2020
MEI_2020$TEMPO_FUNCIONAMENTO <- MEI_2020$ANO_BAIXA - MEI_2020$ANO_ABERTURA
MEI_2020$ANO_BAIXA[MEI_2020$SIT_CADASTRAL == "Ativa"] <- 9999

#################################################################################

SN_2020$ANO_BAIXA[SN_2020$ANO_BAIXA == 9999] <- 2020
SN_2020$TEMPO_FUNCIONAMENTO <- SN_2020$ANO_BAIXA - SN_2020$ANO_ABERTURA
SN_2020$ANO_BAIXA[SN_2020$SIT_CADASTRAL == "Ativa"] <- 9999

#################################################################################

LR_2020$ANO_BAIXA[LR_2020$ANO_BAIXA == 9999] <- 2020
LR_2020$TEMPO_FUNCIONAMENTO <- LR_2020$ANO_BAIXA - LR_2020$ANO_ABERTURA
LR_2020$ANO_BAIXA[LR_2020$SIT_CADASTRAL == "Ativa"] <- 9999

#################################################################################