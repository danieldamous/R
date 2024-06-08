# Análise de dados - Vertificação de normalidade, Análise de variância e Geração de gráficos.
# Daniel Damous, maio de 2024.

########################## Checar, instalar e ativar bibliotecas necessárias ########################## 
# tidyverse
if(!require(tidyverse)){install.packages("tidyverse")
  library(tidyverse)}
# dunn.test
if(!require(dunn.test)){install.packages("dunn.test")
  library(dunn.test)}
# rstatix
if(!require(rstatix)){install.packages("rstatix")
  library(rstatix)}
# xlsx
if(!require(xlsx)){install.packages("xlsx")
  library(xlsx)}
# ggprism
if(!require(ggprism)){install.packages("ggprism")
  library(ggprism)}
########################### Importar arquivos ##########################
df <- read.csv("24h_595nn.csv", header=T)

########################## Reordenar  a posição das colunas ##########################
df <- select(df, controle , X10, X5, X2.5, X1.25, X625, X312, mortas)
#
########################## Verificação de normalidade. ##########################
# Teste de Shapiro-Wilk
lapply(df, shapiro.test)
#
# Teste de Levene
df_longo <- pivot_longer(data = df, cols = everything(), names_to = "absorbancia", values_to = "valores")
levene_test(data = df_longo, valores ~ absorbancia, center=mean)

# Gráfico de caixa (Boxplot)
boxplot(df)

# Identificação de outliers
df_longo %>% group_by(absorbancia) %>% identify_outliers(valores)

# Análise de variância não paramétrica (Kruskal-Wallis);
print(kw <- as.matrix(kruskal.test(df) %>% format(scientific = F)))
#
# Teste post-hoc de Dunn com correção de Bonferroni;
dunn_result <- as.data.frame(dunn.test(df, altp=T, method="bonferroni", alpha = 0.05, list =T))
dunn_result$significância <- ifelse(dunn_result$altP.adjusted < 0.0005, "*** (extremamente)",
                             ifelse(dunn_result$altP.adjusted < 0.005, "** (muito)",
                             ifelse(dunn_result$altP.adjusted < 0.05, "* (significativo)", "ns"))) # ns = não significativo.
print(dunn_result <- format(dunn_result, digits=4, scientific=F))

# Exportar a tabela de resultados do teste post-hoc de Dunn com correção de Bonferroni;
write.xlsx(dunn_result, "dunn_24h_595nn.xlsx")

########################## Gráficos de Boxplot com distribuição dos valores em pontos ##########################
df_longo$absorbancia <- factor(df_longo$absorbancia, levels = c("controle","X10","X5","X2.5","X1.25","X625","X312","mortas"))
grafico <- ggplot(df_longo, aes(x=absorbancia, y=valores, fill=absorbancia)) +
  geom_boxplot(outlier.shape = NA, lwd = 0.9, color = "black", width=NULL) +
  facet_grid(~ absorbancia, scales = "free") +
  geom_point(position = position_jitter(width = 0.1), shape = 21, size = 2.5, color = "black", stroke = 1.1) +
  labs(title = "", x = "Absorbância (%)", y = "ng/mL") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_prism(base_size = 13)
grafico + theme(strip.background = element_blank(), strip.text = element_blank())
#################################################### Fim ####################################################
