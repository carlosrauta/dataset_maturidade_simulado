# ==============================================================================
# SCRIPT: MATURAÇÃO, ESTRUTURA E L50 (MODULAR)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PACOTES NECESSÁRIOS
# ------------------------------------------------------------------------------
library(dplyr)         # Manipulação de dados
library(ggplot2)       # Gráficos e visualização de dados
library(introdataviz)  # Gráfico geom_split_violin
library(DHARMa)        # Diagnóstico de modelos de regressão e GLMM
library(boot)          # Técnicas de bootstrap e reamostragem
library(ggeffects)     # Efeitos preditivos de modelos para visualização

# ==============================================================================
# 2. CONFIGURAÇÕES DO USUÁRIO (Mude apenas aqui para novos trabalhos!)
# ==============================================================================
arquivo_dados <- "https://raw.githubusercontent.com/carlosrauta/dataset_maturidade_simulado/refs/heads/main/dataset_maturidade_peixes_800.csv"

# Nomes exatos das colunas do seu arquivo:
nome_col_comprimento <- "Comprimento_Total_cm"
nome_col_maturidade  <- "Maturidade_Binaria" # 0 (Imaturo), 1 (Maduro)
nome_col_sexo        <- "Sexo"               # "F" e "M"
nome_col_estagio     <- "Estagio_Gonadal"    # Para o Violin Plot

# Configurações Visuais
tamanho_da_classe <- 2 
eixo_x_titulo     <- "Comprimento Total (cm)"
cores_grafico     <- c("F" = "#7F7F7F", "M" = "#000000") # Cinza e Preto
cores_claras      <- c("F" = "#CCCCCC", "M" = "#4D4D4D") # Versão clara para o Violino

# Tema universal para os gráficos não repetirem código
tema_padrao <- theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), # <- CENTRALIZA O TÍTULO AQUI
        legend.position = "right", panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11, color = "black"),
        axis.title = element_text(size = 12))

# ==============================================================================
# 3. LEITURA E PADRONIZAÇÃO AUTOMÁTICA (Não precisa alterar)
# ==============================================================================
dados <- read.csv(arquivo_dados) %>%
  rename(
    L       = !!sym(nome_col_comprimento),
    Mat     = !!sym(nome_col_maturidade),
    Sex     = !!sym(nome_col_sexo),
    Estagio = !!sym(nome_col_estagio)
  ) %>%
  mutate(Sex = as.factor(Sex)) %>%
  filter(!is.na(L), !is.na(Mat), !is.na(Sex), !is.na(Estagio))

# Calculando dinamicamente o topo do gráfico para os rótulos
topo_grafico <- max(dados$L) * 1.05 

# ------------------------------------------------------------------------------
# 4. GRÁFICO 1: ESTRUTURA POPULACIONAL (VIOLIN SPLIT)
# ------------------------------------------------------------------------------
contagens_n <- dados %>%
  group_by(Estagio, Sex) %>%
  summarise(n_amostral = n(), .groups = "drop") %>%
  mutate(rotulo = paste0("(", n_amostral, ")"))

plot_violin <- ggplot(dados, aes(x = Estagio, y = L, fill = Sex)) +
  geom_split_violin(trim = FALSE, alpha = 0.9, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = cores_claras, name = "Sexo") +
  geom_text(data = contagens_n, aes(y = topo_grafico, label = rotulo),
            color = "black", position = position_dodge(width = 0.3),
            size = 4, fontface = "bold", show.legend = FALSE) +
  scale_y_continuous(limits = c(0, max(dados$L) * 1.1)) +
  labs(x = "Estágio Gonadal", y = eixo_x_titulo) +
  tema_padrao + theme(legend.position = "top") # Violino fica melhor com legenda no topo

cat("\n--- EXIBINDO GRÁFICO DA ESTRUTURA POPULACIONAL (VIOLINO) ---\n")
print(plot_violin)

# ------------------------------------------------------------------------------
# 5. GRÁFICO 2: PROPORÇÃO EMPÍRICA EXPLORATÓRIA
# ------------------------------------------------------------------------------
dados_prop <- dados %>%
  mutate(centro = floor(L / tamanho_da_classe) * tamanho_da_classe + (tamanho_da_classe/2)) %>%
  group_by(Sex, centro) %>%
  summarise(n_ind = n(), prop = sum(Mat == 1) / n_ind, .groups = "drop")

plot_empirico <- ggplot(dados_prop, aes(x = centro, y = prop, color = Sex)) +
  geom_line(alpha = 0.4, linewidth = 0.8) + 
  geom_point(aes(size = n_ind), alpha = 0.8) +
  scale_color_manual(values = cores_grafico, name = "Sexo") +
  labs(x = eixo_x_titulo, y = "Proporção Observada de Maduros", size = "n") +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  tema_padrao

print(plot_empirico)

# ------------------------------------------------------------------------------
# 6. MODELAGEM, SELEÇÃO (ANOVA) E VALIDAÇÃO (DHARMa)
# ------------------------------------------------------------------------------
glm_nulo <- glm(Mat ~ L, family = binomial(link = "logit"), data = dados)
glm_alt  <- glm(Mat ~ L * Sex, family = binomial(link = "logit"), data = dados)

cat("\n--- SELEÇÃO DE MODELO (ANOVA) ---\n")
print(anova(glm_nulo, glm_alt, test = "Chisq"))

# Comando para gerar os plots do DHARMa
plot(simulateResiduals(glm_alt, n = 250))

# ------------------------------------------------------------------------------
# 7. MATEMÁTICA DO L50 E BOOTSTRAP (IC 95%)
# ------------------------------------------------------------------------------
cf <- coef(glm_alt)
L50_F <- unname(-cf[1] / cf[2])
L50_M <- unname(-(cf[1] + cf[3]) / (cf[2] + cf[4]))

cat(sprintf("O comprimento L50 estimado é L50_F = %.1f cm para fêmeas e L50_M = %.1f cm para machos.\n", 
            L50_F, L50_M))

# Função para o Bootstrap com Interação
calcula_l50_boot <- function(data, ind) {
  cf <- coef(glm(Mat ~ L * Sex, family = binomial, data = data[ind, ]))
  return(c(unname(-cf[1] / cf[2]), unname(-(cf[1] + cf[3]) / (cf[2] + cf[4]))))
}

# Rodando Bootstrap (1000 iteracoes)
set.seed(123)
boot_comp <- boot(dados, calcula_l50_boot, R = 1000, strata = dados$Sex)
ic_F <- boot.ci(boot_comp, type = "bca", index = 1)$bca[4:5]
ic_M <- boot.ci(boot_comp, type = "bca", index = 2)$bca[4:5]

cat(sprintf("\n--- RESULTADOS FINAIS ---\nL50 Fêmeas: %.1f cm [%.1f - %.1f]\nL50 Machos: %.1f cm [%.1f - %.1f]\n", 
            L50_F, ic_F[1], ic_F[2], L50_M, ic_M[1], ic_M[2]))

# ------------------------------------------------------------------------------
# 8. GRÁFICO 3: OGIVA LOGÍSTICA PREDITIVA (SEM PONTOS EMPÍRICOS)
# ------------------------------------------------------------------------------
curva_pred <- ggpredict(glm_alt, terms = c("L [all]", "Sex"))

plot_ogiva <- ggplot() +
  # Faixa de Confiança e Linha Preditiva
  geom_ribbon(data = curva_pred, aes(x = x, ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.3, show.legend = FALSE) +
  geom_line(data = curva_pred, aes(x = x, y = predicted, color = group), linewidth = 1) +
  
  # === PONTOS EMPÍRICOS (Desativados) ===
  # Remova o "#" da linha abaixo se quiser trazer as bolinhas de volta:
  # geom_point(data = dados_prop, aes(x = centro, y = prop, color = Sex, size = n_ind), alpha = 0.8) +
  # ======================================

# Linhas de Referência L50
annotate("segment", x = c(min(curva_pred$x), L50_F, L50_M), xend = c(L50_F, L50_F, L50_M),
         y = c(0.5, 0, 0), yend = c(0.5, 0.5, 0.5), linetype = "dashed", color = "grey30") +
  
  # Rótulos de Texto
  annotate("text", x = c(L50_F, L50_M), y = c(0.55, 0.65), hjust = c(-0.2, 1.1),
           label = c(sprintf("L[50]*' F = %.1f'", L50_F), sprintf("L[50]*' M = %.1f'", L50_M)),
           color = c(cores_grafico["F"], cores_grafico["M"]), size = 4, parse = TRUE) +
  
  # Estética e Tema
  scale_color_manual(values = cores_grafico, name = "Sexo") +
  scale_fill_manual(values = cores_grafico, guide = "none") +
  labs(title = "Ogiva de Maturação Sexual", 
       x = eixo_x_titulo, 
       y = "Probabilidade de Maturação", 
       size = "n") +
  scale_y_continuous(breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  tema_padrao

print(plot_ogiva)
