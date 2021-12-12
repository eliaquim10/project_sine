
library(tidyverse)
library(nortest)
install.packages("nortest")
install.packages("RVAideMemoire")
library(RVAideMemoire)
install.packages("rstatix")
library(stats)
install.packages("DescTools")
library(DescTools)
################################
#dados_sine <- read.csv("vagas.csv", sep=";")
#dados_sine

#summary(dados_sine)

plot(dados_sine$VALOR_SALARIO)

salario <- subset(dados_sine,VALOR_SALARIO < 100000)
salario


salario1 <- subset(salario,VALOR_SALARIO > 0 )
plot(salario1$VALOR_SALARIO)

summary(salario1$VALOR_SALARIO)

salario_baixo <- subset(salario, VALOR_SALARIO == 2.59)
salario_baixo

plot(dados_sine$REQUERIDO_DEFICIENCIA)

###############################

 ## DATASET: VAGAS NO SINE:

vagas_sine <- read.csv("vagas_categorio.csv", sep=";",encoding = "UTF-8")

##ver em forma de tabela:

view(vagas_sine)

# TENDENCIA CENTRAL DOS VALORES DE SALARIOS EXIGIDOS:

resumo_salarios <- summary(vagas_sine$VALOR_SALARIO)


# FREQUENCIA DE DADOS DO TIPO DE CONTRATAÇÃO:

table(vagas_sine$TIPO_CONTRATACAO)

# FREQUENCIA DE DADOS DA QUANTIDADE DE VAGAS:

resumo_qtvagas <- summary(vagas_sine$QTD_VAGAS)
resumo_qtvagas

# LINHA DO TITULO DE OCUPAÇÃO EM QUE OFERECIDO MAIS VAGAS:

vagas_sine[vagas_sine$QTD_VAGAS=="500",]

# FREQUENCIA DE DADOS DO TIPO DE ESCOLARIDADE REQUERIDA PARA AS VAGAS:

table(vagas_sine$ESCOLARIDADE)

# FREQUENCIA DO TIPO DE EXPERIENCIA EXIGIDA:

resumo_expe <- summary(vagas_sine$QTD_EXPERIENCIA)
resumo_expe


# DISTRIBUIÇÃO DE FREQUENCIA DE ESCOLARIDADE:

escol_freq <- vagas_sine %>% group_by(ESCOLARIDADE) %>% 
  summarise(Freq.escolaridade = n())

escol_freq

# DISTRIBUIÇÃO FREQUENCIA RELATIVA:

escol_freq_rel <- vagas_sine %>% group_by(ESCOLARIDADE) %>% 
  summarise(Freq.escolaridade = n()) %>% mutate(freq.relative = Freq.escolaridade/sum(Freq.escolaridade))

escol_freq_rel

# criando o gráfico de barras da frequencia de escolaridade:


escol_freq_rel_gr <- vagas_sine %>% group_by(ESCOLARIDADE) %>% 
  summarise(Freq.escolaridade = n()) %>% mutate(freq.relative = Freq.escolaridade/sum(Freq.escolaridade)) %>% 
  ggplot() + geom_col(mapping = aes(x = ESCOLARIDADE, y= freq.relative, fill= ESCOLARIDADE )) + labs(x = " ") + theme_classic()

escol_freq_rel_gr


# CRIANDO UM GRAFICO DE PIZZA PARA A FREQUENCIA DE ESCOLARIDADE:


escol_freq_rel_gr_pie <- vagas_sine %>% group_by(ESCOLARIDADE) %>% 
  summarise(Freq.escolaridade = n()) %>% mutate(freq.relative = Freq.escolaridade/sum(Freq.escolaridade)) %>% 
  ggplot(aes(x = "", y=freq.relative,fill=ESCOLARIDADE)) + geom_col(width = 1, color = "white") + coord_polar("y", start = 0) + scale.default() + geom_text(aes(label=paste0(round(freq.relative*100),"%")),
                                                                                                              position = position_stack(vjust = 0.5),color = "white") + theme_void()

# HISTOGRAMA DA COLUNA SALARIOS DO DATASET:

# função bins para a formula sturges:

bins_st <- function(vec){
  ceiling(log(length(vec),2)) + 1
}

vagas_sine %>% ggplot(aes(x = VALOR_SALARIO)) + geom_histogram(bins = bins_st(vagas_sine$VALOR_SALARIO),colour=4, fill="white") + theme_classic()

# TESTE DE SHAPIRO-WILK: TESTE PARA SABER SE A COLUNA SALARIO SEGUE DISTRIBUIÇÃO NORMAL

shapiro.test(vagas_sine$VALOR_SALARIO) # o tamanho da amostra deve estar entre 3 e 5000

lillie.test(vagas_sine$VALOR_SALARIO)

#RESULTADO DO TESTE: COMO O PVALOR É MUITO BAIXO ENTAO REJEITA A HIPOTESE NULA DE QUE OS DADOS PROVEM DE U
# DISTRIBUIÇÃO NORMAL.
Lilliefors (Kolmogorov-Smirnov) normality test

data:  vagas_sine$VALOR_SALARIO
D = 0.20037, p-value < 2.2e-16


# APLICANDO O TESTE DA ANOVA

#TESTE 1: QUEREMOS SABER SE OS VALORES DOS SALARIOS INFORMADOS SÃO COMPATÍVEIS (SIGNIFICATIVOS)
# COM A ESCOLARIDADE EXIGIDA

anova1 <- aov(data = vagas_sine, VALOR_SALARIO ~ ESCOLARIDADE)
summary(anova1)

TukeyHSD(anova1)
plot(TukeyHSD(anova1))

# TESTE 2:

anova2 <- aov(data = vagas_sine, VALOR_SALARIO ~ TITULO_OCUPACAO)
summary(anova2)

TukeyHSD(anova2)

# VERIFICAÇÃO DA NORMALIDADE DOS DADOS POR GRUPO:

# construção do modelo: o valor do salario depende da escolaridade e experiencia.

modelo <- aov(VALOR_SALARIO ~ ESCOLARIDADE*QTD_EXPERIENCIA, vagas_sine)

#teste de normalidade no residuos:

lillie.test(modelo$residuals) # não seguem distribuição normal

#verificação da presença de outliers entre os residuos:

boxplot(modelo$residuals)

vagas_sine$Residuos <- modelo$residuals

View(vagas_sine)

vagas_sine %>% group_by(ESCOLARIDADE,QTD_EXPERIENCIA) %>% 
  identify_outliers(Residuos)

# REALIZAÇÃO DA ANOVA:

modelo <- aov(VALOR_SALARIO ~ ESCOLARIDADE*QTD_EXPERIENCIA, vagas_sine)
summary(modelo)

Anova(modelo, type = "III")

# GRAFICO DE INTERAÇÃO:

ggplot(vagas_sine, aes(x = QTD_EXPERIENCIA, y = VALOR_SALARIO, group= ESCOLARIDADE, color=ESCOLARIDADE)) + 
  geom_line(stat = "summary", fun.data="mean_se", size=0.6)+
  geom_point(stat = "summary", fun.y = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width=0.2)


####modelo 2:

modelo <- aov(VALOR_SALARIO ~ QTD_VAGAS*TIPO_CONTRATACAO, vagas_sine)
summary(modelo)

Anova(modelo, type = "III")

# GRAFICO DE INTERAÇÃO:

ggplot(vagas_sine, aes(x = QTD_VAGAS, y = VALOR_SALARIO, group= TIPO_CONTRATACAO , color=TIPO_CONTRATACAO)) + 
  geom_line(stat = "summary", fun.data="mean_se", size=0.6)+
  geom_point(stat = "summary", fun.y = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width=0.2)


####modelo 3:

modelo <- aov(VALOR_SALARIO ~ QTD_EXPERIENCIA, vagas_sine)
summary(modelo)



modelo2 <- aov(QTD_EXPERIENCIA ~ TIPO_CONTRATACAO, vagas_sine)

modelo3 <- aov(VALOR_SALARIO ~ TIPO_CONTRATACAO, vagas_sine)

modelo4 <- aov(VALOR_SALARIO ~ ESCOLARIDADE, vagas_sine)
summary(modelo4)
# ANALISE PST-HOC:
PostHocTest(modelo4, method = "hsd", conf.level = 0.95)

# GRAFICO DE INTERAÇÃO:

ggplot(vagas_sine, aes(x = QTD_VAGAS, y = VALOR_SALARIO, group= TIPO_CONTRATACAO , color=TIPO_CONTRATACAO)) + 
  geom_line(stat = "summary", fun.data="mean_se", size=0.6)+
  geom_point(stat = "summary", fun.y = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width=0.2)
