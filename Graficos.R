###############################################################################
########## Script dos gráficos iniciais
###############################################################################

# Agrupar por UF
SN_AB <-  SN %>% 
  group_by(UF,ANO_ABERTURA) %>% 
  summarise(QTD = sum(QTD))

# Agrupar por UF
MEI_AB <-  MEI %>% 
  group_by(UF,ANO_ABERTURA) %>% 
  summarise(QTD = sum(QTD))

# Agrupar por UF
LR_AB <- LR %>% 
  group_by(UF,ANO_ABERTURA) %>% 
  summarise(QTD = sum(QTD))

###############################################################################
# Análise de dispersão
###############################################################################

TMP <- LR
TMP$UF <- NULL
TMP$MOTIVO <- NULL
TMP$SIT_CADASTRAL <- NULL
pairs(TMP)
rm(TMP)

TMP <- MEI
TMP$UF <- NULL
TMP$MOTIVO <- NULL
TMP$SIT_CADASTRAL <- NULL
pairs(TMP)
rm(TMP)

TMP <- SN
TMP$UF <- NULL
TMP$MOTIVO <- NULL
TMP$SIT_CADASTRAL <- NULL
pairs(TMP)
rm(TMP)

#################################################################################
## Gráficos de quantitativos
#################################################################################

p <- ggplot(SN_AB, aes(x=UF, y=QTD)) +
       geom_boxplot(aes(x = UF, y = QTD),color="blue", fill="blue", alpha=0.2)
p
p + labs(title = "Quantitativo de Empresas abertas do Simples Nacional", subtitle = "Período: 2005 - 2019")

p1 <- ggplot(MEI_AB, aes(x=UF, y=QTD)) + 
       geom_boxplot(aes(x = UF, y = QTD),color="orange", fill="orange", alpha=0.2)
p1
p1 + labs(title = "Quantitativo de Empresas abertas do MEI", subtitle = "Período: 2005 - 2019")


p2 <- ggplot(LR_AB, aes(x=UF, y=QTD)) + 
      geom_boxplot(aes(x = UF, y = QTD),color="red", fill="red", alpha=0.2)
p2
p2 + labs(title = "Quantitativo de Empresas abertas do Lucro Real/Presumido", subtitle = "Período: 2005 - 2019")

rm(p, p1, p2,cols,TOT_LR_SIT)

#################################################################################
## Gráficos da evolução histórica
#################################################################################

names(LR_AB) <- c("UF","ANO","QTD_LR")
names(SN_AB) <- c("UF","ANO","QTD_SN")
names(MEI_AB) <- c("UF","ANO","QTD_MEI")
full_join2 <- full_join(LR_AB,SN_AB)
full_join3 <- full_join(full_join2,MEI_AB)
full_join3$QTD_LR[is.na(full_join3$QTD_LR)] <- 0
full_join3$QTD_SN[is.na(full_join3$QTD_SN)] <- 0
full_join3$QTD_MEI[is.na(full_join3$QTD_MEI)] <- 0

dados <-  full_join3 %>% 
  group_by(ANO) %>% 
  summarise(QTD_LR = sum(QTD_LR), QTD_SN = sum(QTD_SN), QTD_MEI = sum(QTD_MEI))
dados <- melt(dados, id = c("ANO"))
names(dados) <- c("Ano","Tipo","Qtd")

rm(full_join2,full_join3)

cols <- c("QTD_LR" = "red", "QTD_SN" = "blue", "QTD_MEI" = "orange")
ggplot(data = dados) +
  geom_bar(aes(x = Ano, y = Qtd, fill = Tipo), stat = "identity") +
  labs(title="Evolução histórica das opções por tipo de Tributação", subtitle="Período: 2005 - 2019", 
       y="Quantidade de Estabelecimentos",x="Ano da Abetrura", caption="") +
  labs(fill = "Tributação") + 
  scale_color_manual(values = cols,
                     aesthetics = c("colour", "fill","teste"),                     
                     labels = c("Lucro Real", "Simples Nacional","MEI"))

ggplot(data = dados) +
  geom_bar(aes(x = Ano, y = Qtd,fill = Tipo),
           stat = "identity",position=position_dodge()) +
  labs(title="Evolução histórica das opções por tipo de Tributação", subtitle="Período: 2005 - 2019", 
       y="Quantidade de Estabelecimentos",x="Ano da Abetrura", caption="") +
  labs(fill = "Tributação") +
  scale_color_manual(values = cols,
                     aesthetics = c("colour", "fill","teste"),                     
                     labels = c("Lucro Real", "Simples Nacional","MEI"))  
  
rm(dados,cols)

#################################################################################
## Gráficos do tempo de funcionamento
#################################################################################

# Agrupar por Tempo de Funcionamento
#TOT_SN <- apply(SN[, 6:7], 2, sum)[1]
SN_TF <-  SN %>% 
  group_by(TEMPO_FUNCIONAMENTO) %>%
  filter(SIT_CADASTRAL == "Ativa") %>%
  summarise(Ativas = sum(QTD))
SN_TF1 <-  SN %>% 
  group_by(TEMPO_FUNCIONAMENTO) %>%
  filter(SIT_CADASTRAL != "Ativa") %>%
  summarise(Inativas = sum(QTD))
SN_TF <- full_join(SN_TF,SN_TF1)
rm(SN_TF1)

######################################################################################################

dados <- melt(SN_TF, id = c("TEMPO_FUNCIONAMENTO"))
names(dados) <- c("TEMPO_FUNCIONAMENTO","Tipo","Qtd")

ggplot(data = dados) +
  geom_bar(aes(x = TEMPO_FUNCIONAMENTO, y = Qtd, fill = Tipo), stat = "identity") +
  labs(title="Tempo de funciomamento", subtitle="Empresas do Simples Nacional",
       y="Quantidade de Estabelecimentos",x="Tempo de funcionamento", caption="Período: 2005 - 2019") +
  labs(fill = "Status Cadastral")

ggplot(data = dados) +
  geom_bar(aes(x = TEMPO_FUNCIONAMENTO, y = Qtd,fill = Tipo),
           stat = "identity",position=position_dodge()) +
  labs(title="Tempo de funciomamento", subtitle="Empresas do Simples Nacional", 
       y="Quantidade de Estabelecimentos",x="Tempo de funcionamento", caption="Período: 2005 - 2019") +
  labs(fill = "Status Cadastral")

rm(dados)

################################################################################
# Agrupar por Tempo de Funcionamento
################################################################################

MEI_TF <-  MEI %>% 
  group_by(TEMPO_FUNCIONAMENTO) %>%
  filter(SIT_CADASTRAL == "Ativa") %>%
  summarise(Ativas = sum(QTD))
MEI_TF1 <-  MEI %>% 
  group_by(TEMPO_FUNCIONAMENTO) %>%
  filter(SIT_CADASTRAL != "Ativa") %>%
  summarise(Inativas = sum(QTD))
MEI_TF <- full_join(MEI_TF,MEI_TF1)

rm(MEI_TF1)

######################################################################################################

dados <- melt(MEI_TF, id = c("TEMPO_FUNCIONAMENTO"))
names(dados) <- c("TEMPO_FUNCIONAMENTO","Tipo","Qtd")

ggplot(data = dados) +
  geom_bar(aes(x = TEMPO_FUNCIONAMENTO, y = Qtd, fill = Tipo), stat = "identity") +
  labs(title="Tempo de funciomamento", subtitle="Empresas do MEI", 
       y="Quantidade de Estabelecimentos",x="Tempo de funcionamento", caption="Período: 2005 - 2019") +
  labs(fill = "Status Cadastral")

ggplot(data = dados) +
  geom_bar(aes(x = TEMPO_FUNCIONAMENTO, y = Qtd,fill = Tipo),
           stat = "identity",position=position_dodge()) +
  labs(title="Tempo de funciomamento", subtitle="Empresas do MEI", 
       y="Quantidade de Estabelecimentos",x="Tempo de funcionamento", caption="Período: 2005 - 2019") +
  labs(fill = "Status Cadastral")

rm(MEI_TF,SN_TF,LR_TF)

##############################################################################
# Agrupar por Tempo de Funcionamento
##############################################################################

LR_TF <-  LR %>% 
  group_by(TEMPO_FUNCIONAMENTO) %>%
  filter(SIT_CADASTRAL == "Ativa") %>%
  summarise(Ativas = sum(QTD))
LR_TF1 <-  LR %>% 
  group_by(TEMPO_FUNCIONAMENTO) %>%
  filter(SIT_CADASTRAL != "Ativa") %>%
  summarise(Inativas = sum(QTD))
LR_TF <- full_join(LR_TF,LR_TF1)

rm(LR_TF1)

######################################################################################################

dados <- melt(LR_TF, id = c("TEMPO_FUNCIONAMENTO"))
names(dados) <- c("TEMPO_FUNCIONAMENTO","Tipo","Qtd")

ggplot(data = dados) +
  geom_bar(aes(x = TEMPO_FUNCIONAMENTO, y = Qtd, fill = Tipo), stat = "identity") +
  labs(title="Tempo de funciomamento", subtitle="Empresas do Lucro Real/Presumido", 
       y="Quantidade de Estabelecimentos",x="Tempo de funcionamento", caption="Período: 2005 - 2019") +
  labs(fill = "Status Cadastral")

ggplot(data = dados) +
  geom_bar(aes(x = TEMPO_FUNCIONAMENTO, y = Qtd,fill = Tipo),
           stat = "identity",position=position_dodge()) +
  labs(title="Tempo de funciomamento", subtitle="Empresas do Lucro Real/Presumido", 
       y="Quantidade de Estabelecimentos",x="Tempo de funcionamento", caption="Período: 2005 - 2019") +
  labs(fill = "Status Cadastral")

rm(dados)

################################################################################
# Separação das situações cadastrais e motivos das empresas LR
# Mapa de calor
################################################################################

p1 <-ggplot(SN_AB, aes(ANO,UF)) +
  geom_tile(aes(fill = QTD_SN), color = "white") +
  scale_fill_gradient(low = "white", high = "yellowgreen") +
  labs(title="Mapa de Calor - Empresas Abertas", subtitle="Empresas do Simples Nacional - Período: 2005 - 2019") +
  ylab("Estado") +
  xlab("Ano da Abertura") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Quantidade")
p1

p2 <-ggplot(LR_AB, aes(ANO,UF)) +
  geom_tile(aes(fill = QTD_LR), color = "white") +
  scale_fill_gradient(low = "white", high = "yellowgreen") +
  labs(title="Mapa de Calor - Empresas Abertas", subtitle="Empresas do Lucro Real/Presumido - Período: 2005 - 2019") +
  ylab("Estado") +
  xlab("Ano da Abertura") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Quantidade")
p2

p3 <-ggplot(MEI_AB, aes(ANO,UF )) +
  geom_tile(aes(fill = QTD_MEI), color = "white") +
  scale_fill_gradient(low = "white", high = "yellowgreen") +
  labs(title="Mapa de Calor - Empresas Abertas", subtitle="Empresas do MEI - Período: 2005 - 2019") +  
  ylab("Estado") +
  xlab("Ano da Abertura") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Quantidade")

p3

rm(p1,p2, p3,MEI_AB,LR_AB,SN_AB)