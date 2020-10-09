################################################################################
############## ANÁLISE DOS MOTIVOS DA EXTINÇÃO COM TEXT MINING
################################################################################

base_polaridade <- read.csv2(file=paste0(base_path,"base_polaridade.csv"),sep = ",",fileEncoding="UTF-8")
sw <- read.csv2(file=paste0(base_path,"sw.csv"),sep = ";",fileEncoding="UTF-8")

#################################################################################################
##############         TEXT MINING - LUCRO REAL/PRESUMIDO
#################################################################################################

analise_mensagem <- LR_2020 %>%
  unnest_tokens(palavra, MOTIVO,to_lower = TRUE) %>%
  group_by(ANO_ABERTURA , palavra) %>%
  summarise(n = sum(QTD))

total_palavras <- analise_mensagem %>%
  group_by(ANO_ABERTURA) %>%
  summarize(total=sum(n))

analise_mensagem <- inner_join(analise_mensagem, total_palavras)

analise_mensagem <- analise_mensagem %>%
  bind_tf_idf(palavra, ANO_ABERTURA, n) #%>%
  #filter(tf > 0.0001)

stop_words_grupo <- unique(c(unique(analise_mensagem$palavra[analise_mensagem$tf == 0.00]))) #, stopwords::stopwords("pt")))
stop_words_grupo <- c(stop_words_grupo,c("de","a","das","localizacao","pela","do","rfb","convenente","nao","pelo","na","2009","em","e","da","me","dado","2006","as","por","pedido"))

analise_mensagem <- analise_mensagem %>%
  anti_join(data_frame(palavra = stop_words_grupo))

p <- analise_mensagem %>%
#  anti_join(data_frame(palavra = stop_words_grupo1)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  filter(n >= 300) %>%
  ungroup() %>%
  mutate(palavra = reorder(palavra, n)) %>%
  ggplot(aes(x=palavra, y=n)) +
  geom_segment( aes(x=palavra, xend=palavra, y=0, yend=n ) ) +
  geom_point(color = "orange" ) +
  theme_light(base_size = 12, base_family = "") +
  coord_flip() +
  theme(
    legend.position="none",
    panel.grid.major.y = element_blank(),
    axis.ticks.length = unit(.99, "cm"),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

print(p)
p + labs(title = "Text-Minig - Lucro Real/Presumido\nPeríodo: 2020")  + xlab("Ocorrências") + ylab("contagem")

analise_mensagem %>%
#  anti_join(data_frame(palavra = stop_words_grupo1)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 15, colors=brewer.pal(6,"Dark2"),random.order=FALSE))

analise_mensagem %>%
  filter(n > 150) %>%
  mutate(n = ifelse(palavra == "negative", -n, n)) %>%
  mutate(word = reorder(ANO_ABERTURA, n)) %>%
  ggplot(aes(ANO_ABERTURA, n, fill = palavra)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribuição de cada ocorrência \n Empresas Lucro Real/Presumido - Período: 2020")

pal_tmp <- analise_mensagem[,1:3]
pal_tmp <- inner_join(pal_tmp,base_polaridade,"palavra")
pal_tmp <- pal_tmp[pal_tmp$n > 50000,]

tb_bars <- pal_tmp %>%
  mutate(n = polaridade * n) %>%
  group_by(polaridade) %>%
  ungroup()

p <- ggplot(data = tb_bars,
       mapping = aes(x = reorder(palavra, n),
                     y = n,
                     fill = n)) +
  geom_col(color = "black") +
  scale_fill_distiller(palette = "RdBu", direction = 1) +
  coord_flip() +
  theme_light() +
  #theme(legend.position = c(0.95, 0.5),
  #      legend.justification = c(1, 0.5)) +
  labs(y = "Frequência de ocorrência",
       x = "Motivo",
       fill = "Frequência de\nocorrência")

p
p + labs(title = "Análise do motivo de encerramento", subtitle = "Empresas do Lucro Real/Presumido - Período: 2020")

rm(p,analise_mensagem,stop_words_grupo,total_palavras,tb_bars,n_words,pal_tmp)

#################################################################################################
##############         TEXT MINING - SIMPLES NACIONAL
#################################################################################################

analise_mensagem <- SN_2020 %>%
  unnest_tokens(palavra, MOTIVO,to_lower = TRUE) %>%
  group_by(ANO_ABERTURA , palavra) %>%
  summarise(n = sum(QTD))

total_palavras <- analise_mensagem %>%
  group_by(ANO_ABERTURA) %>%
  summarize(total=sum(n))

analise_mensagem <- inner_join(analise_mensagem, total_palavras)

analise_mensagem <- analise_mensagem %>%
  bind_tf_idf(palavra, ANO_ABERTURA, n) #%>%
#  filter(tf > 0.01)

stop_words_grupo <- unique(c(unique(analise_mensagem$palavra[analise_mensagem$tf == 0.00]))) #, stopwords::stopwords("pt")))
stop_words_grupo <- c(stop_words_grupo,c("de","a","das","localizacao","pela","do","rfb","convenente","nao","pelo","na","2009","em","e","da","me","dado","2006","as","por","pedido"))

analise_mensagem <- analise_mensagem %>%
  anti_join(data_frame(palavra = stop_words_grupo))

p <- analise_mensagem %>%
#  anti_join(data_frame(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  filter(n >= 45) %>%
  ungroup() %>%
  mutate(palavra = reorder(palavra, n)) %>%
  ggplot(aes(x=palavra, y=n)) +
  geom_segment( aes(x=palavra, xend=palavra, y=0, yend=n ) ) +
  geom_point(color = "orange" ) +
  theme_light(base_size = 12, base_family = "") +
  coord_flip() +
  theme(
    legend.position="none",
    panel.grid.major.y = element_blank(),
    axis.ticks.length = unit(.99, "cm"),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

print(p)
p + labs(title = "Text-Minig - Simples Nacional\nPeríodo: 2020")  + xlab("Ocorrências") + ylab("contagem")

analise_mensagem %>%
#  anti_join(data_frame(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 15, colors=brewer.pal(6,"Dark2"),random.order=FALSE))

analise_mensagem %>%
  filter(n > 50) %>%
  mutate(n = ifelse(palavra == "negative", -n, n)) %>%
  mutate(word = reorder(ANO_ABERTURA, n)) %>%
  ggplot(aes(ANO_ABERTURA, n, fill = palavra)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribuição de cada ocorrência \n Empresas do Simples Nacional - Período: 2020")

pal_tmp <- analise_mensagem[,1:3]
pal_tmp <- inner_join(pal_tmp,base_polaridade,"palavra")
pal_tmp <- pal_tmp[pal_tmp$n > 50,]

tb_bars <- pal_tmp %>%
  mutate(n = polaridade * n) %>%
  group_by(polaridade) %>%
#  top_n(n, n = n_words) %>%
#  top_n(n, n = n_words) %>%
  ungroup()

p <- ggplot(data = tb_bars,
       mapping = aes(x = reorder(palavra, n),
                     y = n,
                     fill = n)) +
  geom_col(color = "black") +
  scale_fill_distiller(palette = "RdBu", direction = 1) +
  coord_flip() +
  theme_light() +
#  theme(legend.position = c(0.95, 0.5),
#        legend.justification = c(1, 0.5)) +
  labs(y = "Frequência de ocorrência",
       x = "Motivo",
       fill = "Frequência de\nocorrência")
p
p + labs(title = "Análise do motivo de encerramento", subtitle = "Empresas do Simples Nacional - Período: 2020")

rm(p,analise_mensagem,stop_words_grupo,total_palavras,tb_bars,n_words,pal_tmp)

#################################################################################################
##############         TEXT MINING - MEI
#################################################################################################

analise_mensagem <- MEI_2020 %>%
  unnest_tokens(palavra, MOTIVO,to_lower = TRUE) %>%
  group_by(ANO_ABERTURA , palavra) %>%
  summarise(n = sum(QTD))

total_palavras <- analise_mensagem %>%
  group_by(ANO_ABERTURA) %>%
  summarize(total=sum(n))

analise_mensagem <- inner_join(analise_mensagem, total_palavras)

analise_mensagem <- analise_mensagem %>%
  bind_tf_idf(palavra, ANO_ABERTURA, n) # %>%
#  filter(tf > 0.0001)

stop_words_grupo <- unique(c(unique(analise_mensagem$palavra[analise_mensagem$tf == 0.00]))) #, stopwords::stopwords("pt")))
stop_words_grupo <- c(stop_words_grupo,c("de","a","das","localizacao","pela","do","rfb","convenente","nao","pelo","na","2009","em","e","da","me","dado","2006","as","por","pedido"))

analise_mensagem <- analise_mensagem %>%
  anti_join(data_frame(palavra = stop_words_grupo))

p <- analise_mensagem %>%
#  anti_join(data_frame(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  filter(n >= 2) %>%
  ungroup() %>%
  mutate(palavra = reorder(palavra, n)) %>%
  ggplot(aes(x=palavra, y=n)) +
  geom_segment( aes(x=palavra, xend=palavra, y=0, yend=n ) ) +
  geom_point(color = "orange" ) +
  theme_light(base_size = 12, base_family = "") +
  coord_flip() +
  theme(
    legend.position="none",
    panel.grid.major.y = element_blank(),
    axis.ticks.length = unit(.99, "cm"),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

print(p)
p + labs(title = "Text-Minig - MEI\nPeríodo: 2020")  + xlab("Ocorrências") + ylab("contagem")

analise_mensagem %>%
#  anti_join(data_frame(palavra = stop_words_grupo)) %>%
  group_by(palavra)%>%
  summarise(
    n = sum(n)
  ) %>%
  with(wordcloud(palavra,n,max.words = 15, colors=brewer.pal(6,"Dark2"),random.order=FALSE))

analise_mensagem %>%
  filter(n > 1) %>%
  mutate(n = ifelse(palavra == "negative", -n, n)) %>%
  mutate(word = reorder(ANO_ABERTURA, n)) %>%
  ggplot(aes(ANO_ABERTURA, n, fill = palavra)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribuição de cada ocorrência \n Empresas do MEI - Período: 2020")

pal_tmp <- analise_mensagem[,1:3]
pal_tmp <- inner_join(pal_tmp,base_polaridade,"palavra")
pal_tmp <- pal_tmp[pal_tmp$n > 1,]

tb_bars <- pal_tmp %>%
  mutate(n = polaridade * n) %>%
  group_by(polaridade) %>%
  #  top_n(n, n = n_words) %>%
  #  top_n(n, n = n_words) %>%
  ungroup()

p <- ggplot(data = tb_bars,
            mapping = aes(x = reorder(palavra, n),
                          y = n,
                          fill = n)) +
  geom_col(color = "black") +
  scale_fill_distiller(palette = "RdBu", direction = 1) +
  coord_flip() +
  theme_light() +
  #  theme(legend.position = c(0.95, 0.5),
  #        legend.justification = c(1, 0.5)) +
  labs(y = "Frequência de ocorrência",
       x = "Motivo",
       fill = "Frequência de\nocorrência")
p
p + labs(title = "Análise do motivo de encerramento", subtitle = "Empresas do MEI - Período: 2020")

rm(p,analise_mensagem,stop_words_grupo,total_palavras,tb_bars,n_words,pal_tmp)
