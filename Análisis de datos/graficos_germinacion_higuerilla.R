# ============================================================
#  GRÁFICOS - ESCARIFICACIÓN DE HIGUERILLA (Ricinus communis)
#  Experimento: Grupo 5 - Chachapoyas, Amazonas
#  4 Tratamientos: Control, Lija, Bisturí, Térmica
#  4 Repeticiones | 25 semillas por unidad | DAI 0 - 13
# ============================================================

# --- 1. INSTALAR Y CARGAR LIBRERÍAS -------------------------
if (!require("ggplot2"))   install.packages("ggplot2")
if (!require("dplyr"))     install.packages("dplyr")
if (!require("tidyr"))     install.packages("tidyr")
if (!require("ggpubr"))    install.packages("ggpubr")

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)

# --- 2. INGRESAR DATOS --------------------------------------
# Germinación diaria por unidad experimental (semillas germinadas ese día)
fb <- data.frame(
  tratamiento = c("Control","Térmica","Térmica","Térmica","Térmica",
                  "Control","Lija","Lija","Bisturí","Bisturí",
                  "Control","Bisturí","Control","Lija","Lija","Bisturí"),
  rep         = c(3, 4, 1, 2, 3, 4, 1, 4, 2, 3, 2, 4, 1, 3, 2, 1),
  semillas    = 25,
  dia_0  = 0, dia_1  = 0, dia_2  = 0, dia_3  = 0,
  dia_4  = 0, dia_5  = 0, dia_6  = 0, dia_7  = 0,
  dia_8  = 0,
  dia_9  = c(0,0,0,1,0,0,2,1,2,2,0,0,0,3,1,2),
  dia_10 = c(0,1,0,3,0,0,3,2,1,1,0,2,0,2,2,1),
  dia_11 = c(0,0,1,0,0,0,2,2,2,3,0,1,0,3,3,2),
  dia_12 = c(1,1,0,2,1,0,4,4,3,2,0,2,0,4,2,2),
  dia_13 = c(0,0,0,1,0,0,1,4,3,2,0,1,1,4,3,3)
)

# Definir orden de tratamientos
fb$tratamiento <- factor(fb$tratamiento,
                         levels = c("Control","Térmica","Bisturí","Lija"))

# Paleta de colores
colores <- c("Control" = "#7f8c8d",
             "Térmica" = "#e74c3c",
             "Bisturí" = "#3498db",
             "Lija"    = "#2ecc71")

# --- 3. CALCULAR VARIABLES ----------------------------------
dias <- paste0("dia_", 0:13)

# Germinación acumulada por unidad
for (i in seq_along(dias)) {
  col_acum <- paste0("cum_", dias[i])
  fb[[col_acum]] <- rowSums(fb[, dias[1:i]])
}

# Porcentaje de germinación final
fb$pct_ger <- (rowSums(fb[, dias]) / fb$semillas) * 100

# Índice de Velocidad de Germinación (IVG = Σ ni/ti, t > 0)
dias_num <- 1:13
cols_ivg <- paste0("dia_", dias_num)
fb$IVG <- rowSums(
  mapply(function(col, t) fb[[col]] / t,
         cols_ivg, dias_num)
)

# --- 4. RESUMEN POR TRATAMIENTO -----------------------------
resumen <- fb %>%
  group_by(tratamiento) %>%
  summarise(
    media_pct  = mean(pct_ger),
    se_pct     = sd(pct_ger) / sqrt(n()),
    sd_pct     = sd(pct_ger),
    media_IVG  = mean(IVG),
    se_IVG     = sd(IVG) / sqrt(n()),
    .groups = "drop"
  )

# --- 5. GRÁFICO 1: PORCENTAJE FINAL DE GERMINACIÓN ----------
# (Barras + Error estándar — Objetivo 1 y 2)
g1 <- ggplot(resumen, aes(x = tratamiento, y = media_pct,
                           fill = tratamiento)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", linewidth = 0.4) +
  geom_errorbar(aes(ymin = media_pct - se_pct,
                    ymax = media_pct + se_pct),
                width = 0.2, linewidth = 0.7) +
  geom_text(aes(label = paste0(round(media_pct, 1), "%")),
            vjust = -1.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = colores) +
  scale_y_continuous(limits = c(0, 75),
                     breaks = seq(0, 75, 15)) +
  labs(
    title    = "Porcentaje de Germinación Final por Tratamiento",
    subtitle = "Higuerilla (Ricinus communis) — Chachapoyas 2026",
    x        = "Método de Escarificación",
    y        = "Germinación (%)",
    caption  = "Barras de error = Error Estándar (n = 4)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5, color = "grey40"),
    axis.text        = element_text(color = "black"),
    plot.caption     = element_text(color = "grey50")
  )

# --- 6. GRÁFICO 2: CURVAS DE GERMINACIÓN ACUMULADA ----------
# (Objetivo 1: efecto a lo largo del tiempo)

# Preparar datos en formato largo
cum_cols <- paste0("cum_dia_", 0:13)
curvas <- fb %>%
  select(tratamiento, rep, all_of(cum_cols)) %>%
  pivot_longer(cols = all_of(cum_cols),
               names_to  = "dia",
               values_to = "acumulado") %>%
  mutate(
    dia = as.numeric(gsub("cum_dia_", "", dia)),
    pct_acum = (acumulado / 25) * 100
  ) %>%
  group_by(tratamiento, dia) %>%
  summarise(media = mean(pct_acum),
            se    = sd(pct_acum) / sqrt(n()),
            .groups = "drop")

g2 <- ggplot(curvas, aes(x = dia, y = media,
                          color = tratamiento,
                          fill  = tratamiento)) +
  geom_ribbon(aes(ymin = media - se, ymax = media + se),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) +
  scale_x_continuous(breaks = 0:13) +
  scale_y_continuous(limits = c(0, 75),
                     breaks = seq(0, 75, 15)) +
  labs(
    title    = "Curva de Germinación Acumulada por Tratamiento",
    subtitle = "Higuerilla (Ricinus communis) — Chachapoyas 2026",
    x        = "Días después de la siembra (DAI)",
    y        = "Germinación acumulada (%)",
    color    = "Tratamiento",
    fill     = "Tratamiento",
    caption  = "Banda sombreada = ± Error Estándar (n = 4)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    axis.text     = element_text(color = "black"),
    legend.position = c(0.15, 0.75),
    legend.background = element_rect(fill = "white", color = "grey80"),
    plot.caption  = element_text(color = "grey50")
  )

# --- 7. GRÁFICO 3: BOXPLOT ----------------------------------
# (Variabilidad entre repeticiones — Objetivo 1)
g3 <- ggplot(fb, aes(x = tratamiento, y = pct_ger,
                      fill = tratamiento)) +
  geom_boxplot(width = 0.5, outlier.shape = 21,
               outlier.size = 2.5, color = "black", linewidth = 0.5) +
  geom_jitter(width = 0.1, size = 2.5, alpha = 0.7,
              aes(color = tratamiento)) +
  scale_fill_manual(values  = colores) +
  scale_color_manual(values = colores) +
  scale_y_continuous(limits = c(0, 80),
                     breaks = seq(0, 80, 20)) +
  labs(
    title    = "Distribución del Porcentaje de Germinación",
    subtitle = "Higuerilla (Ricinus communis) — Chachapoyas 2026",
    x        = "Método de Escarificación",
    y        = "Germinación (%)",
    caption  = "Puntos = repeticiones individuales (n = 4)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5, color = "grey40"),
    axis.text        = element_text(color = "black"),
    plot.caption     = element_text(color = "grey50")
  )

# --- 8. GRÁFICO 4: ÍNDICE DE VELOCIDAD DE GERMINACIÓN -------
# (Objetivo 2: identificar el método más eficiente)
g4 <- ggplot(resumen, aes(x = tratamiento, y = media_IVG,
                           fill = tratamiento)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", linewidth = 0.4) +
  geom_errorbar(aes(ymin = media_IVG - se_IVG,
                    ymax = media_IVG + se_IVG),
                width = 0.2, linewidth = 0.7) +
  geom_text(aes(label = round(media_IVG, 2)),
            vjust = -1.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = colores) +
  scale_y_continuous(limits = c(0, 1.6),
                     breaks = seq(0, 1.6, 0.4)) +
  labs(
    title    = "Índice de Velocidad de Germinación (IVG)",
    subtitle = "Higuerilla (Ricinus communis) — Chachapoyas 2026",
    x        = "Método de Escarificación",
    y        = "IVG (semillas·día⁻¹)",
    caption  = "IVG = Σ(nᵢ/tᵢ) | Barras de error = Error Estándar (n = 4)"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5, color = "grey40"),
    axis.text        = element_text(color = "black"),
    plot.caption     = element_text(color = "grey50")
  )

# --- 9. GUARDAR GRÁFICOS ------------------------------------
ggsave("G1_porcentaje_germinacion.png", g1,
       width = 7, height = 5.5, dpi = 300)

ggsave("G2_curvas_acumuladas.png", g2,
       width = 8, height = 5.5, dpi = 300)

ggsave("G3_boxplot_germinacion.png", g3,
       width = 7, height = 5.5, dpi = 300)

ggsave("G4_IVG.png", g4,
       width = 7, height = 5.5, dpi = 300)

# Panel combinado (los 4 juntos)
panel <- ggarrange(g1, g2, g3, g4,
                   ncol = 2, nrow = 2,
                   labels = c("A","B","C","D"))

ggsave("Panel_todos_graficos.png", panel,
       width = 14, height = 11, dpi = 300)

message("✅ ¡Gráficos guardados correctamente!")

# --- 10. MOSTRAR RESUMEN EN CONSOLA -------------------------
cat("\n======= RESUMEN POR TRATAMIENTO =======\n")
print(resumen %>%
        mutate(across(where(is.numeric), ~round(.x, 2))) %>%
        rename(Tratamiento   = tratamiento,
               `% Ger. Media` = media_pct,
               `SE (%)`       = se_pct,
               `SD (%)`       = sd_pct,
               `IVG Media`    = media_IVG,
               `SE (IVG)`     = se_IVG))
