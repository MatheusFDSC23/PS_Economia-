
# Autor:
# Matheus Ferreira da Silva Costa
# Instituto de Economia - UFRJ
# Curso: Ciências Econômicas
# Email: matheus.costa@graduacao.ie.ufrj.br

# -------------------------
# Pacotes
# -------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, readxl, tinytex, janitor)

# -------------------------
# Questão 01
# -------------------------


# -------------------------
# a) Definir diretório de trabalho
setwd("C:/Users/matheus.scosta/Desktop/PS_economia")



# -------------------------
# b) Importar base de dados

df <- read.csv(
  "input/base_ipea 1.csv",
  sep = ";",
  fileEncoding = "Latin1",
  stringsAsFactors = FALSE
)

names(df) <- iconv(names(df), from = "Latin1", to = "UTF-8")
df <- clean_names(df)

# -------------------------
# c) Quantas linhas e colunas?

n_linhas <- nrow(df)
n_colunas <- ncol(df)

n_linhas
n_colunas

# -------------------------
# d) Frequência de pessoas por região do Brasil

Frequencia <- table(df$regi_a_o_onde_foi_realizada_a_entrevista)
Frequencia

# -------------------------
# e) Qual é a região mais frequente (moda)?

names(which.max(table(df$regiao_onde_foi_realizada_a_entrevista)))



# -------------------------
# f) Idade da pessoa mais nova e mais velha na amostra

age_min <- min(df$idade_qual_a_sua_idade_em_anos_completos, na.rm = TRUE)
age_max <- max(df$idade_qual_a_sua_idade_em_anos_completos, na.rm = TRUE)

#IDADE DA PESSOA MAIS NOVA: 
age_min

#IDADE DA PESSOA MAIS VELHA:
age_max


# -------------------------
# g) Média, mediana, moda e assimetria da idade


idade <- df$idade_qual_a_sua_idade_em_anos_completos

# Média: 
media_idade <- mean(idade, na.rm = TRUE)
media_idade


# Mediana:
mediana_idade <- median(idade, na.rm = TRUE)
mediana_idade


# Moda:
moda <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

moda_idade <- moda(idade)
moda_idade


# -------------------------
# h) Faixas etárias + classificação

df$faixa_etaria <- cut(
  idade,
  breaks = c(0, 29, 59, Inf),
  labels = c("Jovem (≤29)", "Adulto (30-59)", "Idoso (60+)"),
  right = TRUE
)

table(df$faixa_etaria)

# Mais jovens, Adultos ou Idosos?

names(which.max(table(df$faixa_etaria)))



# -------------------------
# i) Calcule a média, a mediana, o primeiro quartil, o terceiro quartil e os 
# valores máximo e mínimo para a variável “renda total de todos os moradores, 
# parentes e agregados no último mês”. Comente os resultados. 

renda <- df$renda_total_de_todos_os_moradores_parentes_e_agregados_no_aoltimo_m_aas

# Média da Renda 
media_renda <- mean(renda, na.rm = TRUE)
media_renda

# Mediana da Renda:
mediana_renda <- median(renda, na.rm = TRUE)
mediana_renda

# 1° quartil
q1 <- quantile(renda, 0.25, na.rm = TRUE)
q1

# 3° quartil
q3 <- quantile(renda, 0.75, na.rm = TRUE)
q3

# Mínimo da Renda 
min_renda <- min(renda, na.rm = TRUE)
min_renda

# Máximo da Renda 
max_renda <- max(renda, na.rm = TRUE)
max_renda


# A renda total dos moradores apresentou média de aproximadamente 
# R$ 1.801,48 e mediana de R$ 1.400,00. Como a média é superior à mediana, 
# observa-se indício de assimetria positiva na distribuição da renda, 
# sugerindo a presença de valores elevados que aumentam a média da distribuição.

# O primeiro quartil (Q1 = R$ 800) indica que 25% dos indivíduos possuem renda 
# familiar total mensal de até esse valor. Já o terceiro quartil (Q3 = R$ 2.150) 
# mostra que 75% da amostra possuem renda de até R$ 2.150.

# Além disso, a diferença entre o valor mínimo (R$ 150) e o valor máximo (R$ 19.000) 
# evidencia elevada dispersão dos rendimentos na amostra, refletindo heterogeneidade 
# e desigualdade na distribuição da renda.




# j) Interprete o primeiro e o terceiro quartis encontrados no item anterior.

# O primeiro quartil (Q1 = R$ 800) representa o valor abaixo do qual se 
# encontram 25% das observações da distribuição de renda, indicando que 
# um quarto dos indivíduos da amostra possui renda familiar total mensal 
# de até esse valor.

# O terceiro quartil (Q3 = R$ 2150), por sua vez, representa o valor abaixo 
# do qual se encontram 75% das observações, evidenciando que três quartos da 
# amostra possuem renda familiar total mensal de até R$ 2150.

# Assim, o intervalo interquartil, compreendido entre Q1 e Q3, concentra os 50% 
# centrais da distribuição da renda, sendo uma medida importante para avaliar a 
# dispersão dos dados e reduzir a influência de valores extremos.




# l) Coeficiente de variação da idade e da renda

coef_variacao <- function(x) {
  
  desvio_padrao <- sd(x, na.rm = TRUE)
  media <- mean(x, na.rm = TRUE)
  
  cv <- (desvio_padrao / media) * 100
  
  return(cv)
}



# Coeficiente de variação da idade
cv_idade <- coef_variacao(idade)

# Coeficiente de variação da renda
cv_renda <- coef_variacao(renda)

cv_idade
cv_renda


# -------------------------
# m) Calcule o desvio-padrão para a renda de acordo com cada região do Brasil.
# Qual é a região que possui um comportamento mais homogêneo em relação à renda?

desvio_regiao <- df %>%
  group_by(regi_a_o_onde_foi_realizada_a_entrevista) %>%
  summarise(
    desvio_padrao_renda = sd(
      as.numeric(
        renda_total_de_todos_os_moradores_parentes_e_agregados_no_aoltimo_m_aas
      ),
      na.rm = TRUE
    )
  )

desvio_regiao


# Região mais homogênea (menor desvio-padrão)

regiao_homogenea <- desvio_regiao %>%
  arrange(desvio_padrao_renda) %>%
  slice(1)

regiao_homogenea

# A região Nordeste apresentou o menor desvio-padrão da renda, 
# aproximadamente 1190.Dessa forma, conclui-se que essa região 
# possui o comportamento mais homogêneo em relação à renda, 
# indicando menor dispersão dos rendimentos em torno da média 
# regional quando comparada às demais regiões da amostra.
