#Librería más fácil
setwd("C:/Users/sebas/OneDrive/Escritorio/Octavo Semestre/OctavoSemestre/Diseños y Análisis de Experimentos/Trabajo Final")
install.packages("easypackages")        # Libreria especial para hacer carga automática de librerias
library("easypackages")
# Listado de librerias requeridas por el script
lib_req<-c("Matrix","plotly","tidyverse","desplot","broom","emmeans", "multcomp",
           "multcompView","ggrepel","ggpubr","readxl","performance","see","tidyverse")
easypackages::packages(lib_req) 
source("Script R - source.R")
# dataframe con los datos
# dataframe con los datos corregido
data <- data.frame(
  mote = factor(c(1:30)),
  agua = factor(rep(c("4ml", "8ml"), each = 15)),
  edad = factor(rep(rep(c("sem1", "sem3", "sem6", "sem9", "sem12"), each = 3), times = 2)),
  Rep = rep(paste0("R", 1:3), each = 1),
  produccion = c(11, 9, 6, 7, 16, 17, 9, 19, 35, 13, 35, 28, 20, 37, 45, 8, 3, 3, 1, 7, 3, 5, 9, 9, 1, 10, 9, 11, 15, 25)
)

# Realizar el análisis de varianza (ANOVA)
modelo <- lm(produccion ~ agua * edad, data = data)
tabla_anova <- anova(modelo)

# Imprimir la tabla ANOVA
print(tabla_anova)

# -------------------------------------------------------------------------
# Analysis ----------------------------------------------------------------
# -------------------------------------------------------------------------

datos <- data %>%
  mutate(
    agua = as.factor(agua),
    edad = as.factor(edad),
    Rep = as.factor(Rep)
  )

xtabs(formula = ~ agua + edad, data = datos)

t <- datos$agua %>% nlevels()
b <- datos$edad %>% nlevels()
r <- datos$Rep %>% nlevels()
n <- t * b * r

SST <- var(datos$produccion) * (n - 1)
SST

# -------------------------------------------------------------------------
# Chequeo de la variable de respuesta
# -------------------------------------------------------------------------

# summary
raw_m <- datos %>%
  group_by(agua, edad) %>%
  summarize(
    mean = mean(produccion, na.rm = T),
    std.dev = sd(produccion, na.rm = T),
    cv = std.dev / mean
  ) %>%
  print(n = Inf)
raw_m

datos %>%
  group_by(agua) %>%
  summarise(mean = mean(produccion, na.rm = T))

datos %>%
  group_by(edad) %>%
  summarise(mean = mean(produccion, na.rm = T))

# interaction plot edad on X axis
plot_a <- raw_m %>%
  ggplot(
    aes(
      x = edad,
      y = mean,
      group = agua,
      color = agua
    )
  ) +
  geom_point(size = 4) +
  geom_line() +
  theme_bw(base_size = 12) +
  theme(legend.position = "top") +
  labs(title = "Interaction Plot", subtitle = "edad sobre eje X")
plot_a

# interaction plot Color by edad
plot_b <- raw_m %>%
  ggplot(
    aes(
      x = agua,
      y = mean,
      group = edad,
      color = edad
    )
  ) +
  geom_point(size = 4) +
  geom_line() +
  theme_bw(base_size = 12) +
  theme(legend.position = "top") +
  labs(title = "Interaction Plot", subtitle = "agua sobre eje X")
plot_b

ggarrange(plot_a, plot_b)

# Marginal Means
datos %>%
  group_by(agua) %>%
  summarize(
    mean = mean(produccion),
    std.dev = sd(produccion)
  ) %>%
  arrange(desc(mean)) %>% # sort
  print(n = Inf) # print full table


datos %>%
  group_by(edad) %>%
  summarize(
    mean = mean(produccion),
    std.dev = sd(produccion)
  ) %>%
  arrange(desc(mean)) %>% # sort
  print(n = Inf) # print full table


ggplot(
  data = datos,
  aes(
    y = produccion,
    x = edad,
    color = edad
  )
) +
  facet_grid(~agua) + # facette per edad level
  geom_point(size = 4) + # scatter plot observed
  theme_bw(base_size = 12) + # clearer plot format
  theme(legend.position = "top") # legend on top


# -------------------------------------------------------------------------
# Matrices ----------------------------------------------------------------
# -------------------------------------------------------------------------

# Number of aguas
t <- datos$agua %>% nlevels()

# Number of levels of edad
b <- datos$edad %>% nlevels()

# Number of reps
r <- datos$Rep %>% nlevels()

# Total of obs
n <- t * b * r

# design matrix
X0 <- model.matrix(~1, datos)
X1 <- model.matrix(~ -1 + agua, datos)
X2 <- model.matrix(~ -1 + edad, datos)
X3 <- model.matrix(~ -1 + agua:edad, datos)

X <- cbind(X0, X1, X2, X3)
X

Matrix(X, sparse = TRUE)

# rank X
qr(X)$rank
dim(X)

# X'X
XTX <- t(X) %*% X
XTX
Matrix(XTX, sparse = TRUE)

# rank X'X
qr(X)$rank

# Y
Y <- as.matrix(datos$produccion, nrow = n, ncol = 1)
Y

# X'Y
XTY <- t(X) %*% Y
XTY

# Solution

# overall mean
mu <- mean(datos$produccion)

# tau effects
tau_i <- datos %>%
  group_by(agua) %>%
  summarise(mean = mean(produccion) - mu) %>%
  pull()

# alpha effects
alpha_j <- datos %>%
  group_by(edad) %>%
  summarise(mean = mean(produccion) - mu) %>%
  pull()

# tau x alpha effects
tau_alpha_ij <- datos %>%
  group_by(agua) %>%
  mutate(mean_A = mean(produccion)) %>%
  group_by(edad) %>%
  mutate(mean_B = mean(produccion)) %>%
  group_by(agua, edad) %>%
  summarise(
    mean = mean(produccion),
    mean_A = mean(mean_A),
    mean_B = mean(mean_B),
    .groups = "drop"
  ) %>%
  mutate(tau_alpha = mean - mean_A - mean_B + mu) %>%
  arrange(edad) %>%
  pull(tau_alpha)

beta <- c(
  mu,
  tau_i,
  alpha_j,
  tau_alpha_ij
)
beta

# R(beta) = beta'X'Y
R_beta <- t(beta) %*% XTY

R_mu <- beta[1] %*% t(X0) %*% Y
R_tau <- beta[2:3] %*% t(X1) %*% Y
R_alpha <- beta[4:5] %*% t(X2) %*% Y
R_tau_alpha <- beta[6:9] %*% t(X3) %*% Y

R_mu + R_tau + R_alpha + R_tau_alpha

# Sum of squares of factor A
SStau <- R_beta - R_mu - R_alpha - R_tau_alpha
SStau

CMtau <- SStau / (t - 1)
CMtau

# Sum of squares of factor B
SSalpha <- R_beta - R_mu - R_tau - R_tau_alpha
SSalpha

CMalpha <- SSalpha / (b - 1)
CMalpha

# Sum of squares of factor AB
SStau_alpha <- R_beta - R_mu - R_tau - R_alpha
SStau_alpha

CMtau_alpha <- SStau_alpha / ((t - 1) * (b - 1))
CMtau_alpha

# Sum of squares Total
SST <- t(Y) %*% Y - R_mu
SST

CMT <- SST / (n - 1)
CMT

# Sum of squares Error
SSE <- t(Y - X %*% beta) %*% (Y - X %*% beta)
SSE

CME <- SSE / (t * b * (r - 1))
CME

# Sigma^2
sigma_2 <- t(Y - X %*% beta) %*% (Y - X %*% beta) / (t * b * (r - 1))
sigma_2

# ANOVA
ANOVA <- data.frame(
  Fuente = c("agua", "edad", "agua:edad", "Residuals", "Total"),
  DF = c(t - 1, b - 1, (t - 1) * (b - 1), t * b * (r - 1), n - 1),
  Sum_Sq = c(SStau, SSalpha, SStau_alpha, SSE, SST),
  Mean_Sq = c(CMtau, CMalpha, CMtau_alpha, CME, CMT),
  F_value = c(CMtau / CME, CMalpha / CME, CMtau_alpha / CME, NA, NA),
  `Pr(>F)` = c(
    pf(CMtau / CME, t - 1, t * b * (r - 1), lower.tail = F),
    pf(CMalpha / CME, b - 1, t * b * (r - 1), lower.tail = F),
    pf(CMtau_alpha / CME, (t - 1) * (b - 1), t * b * (r - 1), lower.tail = F),
    NA, NA
  ),
  check.names = F
)
ANOVA

f_crtitical <- qf(0.95, df1 = t - 1, df2 = t * b * (r - 1))

# Plot ANOVA
ANOVA %>%
  mutate(
    source = c(rep("ANOVA", nrow(.) - 1), "TOTAL"),
    percentage = paste0(round(Sum_Sq / c(SST) * 100, 2), "%")
  ) %>%
  ggplot(aes(x = source, y = Sum_Sq, fill = Fuente, label = percentage)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "") +
  coord_flip()

# -------------------------------------------------------------------------
# Modelling ---------------------------------------------------------------
# -------------------------------------------------------------------------

# Comparison
m1 <- lm(produccion ~ agua, data = datos)
m2 <- lm(produccion ~ edad, data = datos)
m3 <- lm(produccion ~ agua + edad, data = datos)
m4 <- lm(produccion ~ agua + edad + agua:edad, data = datos)

compare_performance(m1, m2, m3, m4)
plot(compare_performance(m1, m2, m3, m4))

# Best Model
mod <- lm(produccion ~ agua + edad + agua:edad, data = datos)
ANOVA <- mod %>%
  anova() %>%
  tidy()

# ANOVA -------------------------------------------------------------------

TOTAL <- data.frame(term = "Total", df = n - 1, sumsq = SST)
ANOVA <- bind_rows(ANOVA, TOTAL)

ANOVA %>%
  mutate(
    source = c(rep("ANOVA", nrow(.) - 1), "TOTAL"),
    percentage = paste0(round(sumsq / c(SST) * 100, 2), "%")
  ) %>%
  ggplot(aes(x = source, y = sumsq, fill = term, label = percentage)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "") +
  coord_flip()

# -------------------------------------------------------------------------
# Plots -------------------------------------------------------------------
# -------------------------------------------------------------------------

all_mean_comparisons <- mod %>%
  emmeans(pairwise ~ edad:agua,
    adjust = "tukey",
    lmer.df = "kenward-roger"
  ) %>%
  pluck("emmeans") %>%
  cld(details = TRUE, Letters = letters) # add letter display

all_mean_comparisons$emmeans # adjusted means

within_type_mean_comparisons <- mod %>%
  emmeans(pairwise ~ edad | agua,
    adjust = "tukey",
    lmer.df = "kenward-roger"
  ) %>%
  pluck("emmeans") %>%
  cld(details = TRUE, Letters = letters) # add letter display

within_type_mean_comparisons$emmeans # adjusted means

formatted_emmeans <- within_type_mean_comparisons$emmeans %>%
  as_tibble()

g1 <- ggplot() +
  facet_grid(~agua) +
  # black dots representing the raw data
  geom_point(
    data = datos,
    aes(y = produccion, x = edad)
  ) +
  # red dots representing the adjusted means
  geom_point(
    data = formatted_emmeans,
    aes(y = emmean, x = edad),
    color = "red",
    position = position_nudge(x = 0.1)
  ) +
  # red error bars representing the confidence limits of the adjusted means
  geom_errorbar(
    data = formatted_emmeans,
    aes(ymin = lower.CL, ymax = upper.CL, x = edad),
    color = "red",
    width = 0.1,
    position = position_nudge(x = 0.1)
  ) +
  # red letters
  geom_text(
    data = formatted_emmeans,
    aes(y = emmean, x = edad, label = .group),
    color = "red",
    position = position_nudge(x = 0.2)
  ) +
  ylim(13, NA) + # force y-axis to start at 0
  ylab("produccion") + # label y-axis
  xlab("edad Level") + # label x-axis
  labs(caption = "The two facettes represent aguas Classic and Reggaeton
       Black dots represent raw data
       Red dots and error bars represent adjusted mean with 95% confidence limits per agua-edad level combination
       Separately per agua, means followed by a common letter are not significantly different according to the Tukey-test") +
  theme_bw() + # clearer plot format
  theme() # rotate x-axis label
g1

