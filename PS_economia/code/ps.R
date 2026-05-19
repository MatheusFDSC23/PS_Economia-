
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
df <- read.csv("input/base_ipea 1.csv",
               sep = ";",
               fileEncoding = "Latin1",
               stringsAsFactors = FALSE)

df <- clean_names(df)



# -------------------------
# c) Quantas linhas e colunas?

n_linhas <- nrow(df)
print(n_linhas)



# -------------------------
# d) Frequência de pessoas por região do Brasil

table(df$regiao_onde_foi_realizada_a_entrevista)



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
# Renda 
renda <- df$renda_total_de_todos_os_moradores_parentes_e_agregados_no_ultimo_mes


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




