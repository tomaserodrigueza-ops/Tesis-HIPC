# =============================================================================
# FASE 1 — HIPÓTESIS H1: SOSTENIBILIDAD Y EFECTO CAUSAL DE LA INICIATIVA HIPC
# Tesis doctoral: Iniciativa HIPC y sostenibilidad de la deuda externa
# =============================================================================
# Métodos:
#   §1.1  Preparación: reconstruir variables de tratamiento
#   §1.2  Estadísticos descriptivos pre/post
#   §1.3  DiD base (TWFE): efecto de culminación sobre dxp y gdpg
#   §1.4  Event-study: dinámica temporal del efecto
#   §1.5  DiD con heterogeneidad: subgrupos por gobernanza y región
#   §1.6  Pruebas de robustez: tendencias paralelas, placebo, anticipación
# =============================================================================

setwd("C:/Users/tomas/OneDrive/Documentos/Doctorado/TESIS/Datos")

library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(ggplot2)
library(openxlsx)

cat("✅ Paquetes cargados\n\n")

load("panel_HIPC.RData")
cat(sprintf("Panel: %d filas × %d columnas | %d países | %d–%d\n\n",
            nrow(panel_complete), ncol(panel_complete),
            n_distinct(panel_complete$iso3),
            min(panel_complete$year), max(panel_complete$year)))

dir.create("resultados_H1", showWarnings = FALSE)

# =============================================================================
# §1.1  PREPARACIÓN — VARIABLES DE TRATAMIENTO
# Usar nombres reales confirmados: completion_year, post_completion
# =============================================================================
cat("=== §1.1 Preparación ===\n")

panel_h1 <- panel_complete %>%
  arrange(iso3, year) %>%
  group_by(iso3) %>%
  mutate(
    # Reconstruir time_since_comp (tenía 672 NAs)
    time_since_comp = pmax(0L, year - completion_year),

    # Año relativo al completion point (ventana -5 a +10, ref = -1)
    rel_year = as.integer(year - completion_year),
    rel_year_clamp = pmax(-5L, pmin(10L, rel_year)),
    rel_year_f = relevel(factor(rel_year_clamp), ref = "-1"),

    # Dummy período pre-decisión (para test anticipación)
    pre_decision = as.integer(year < decision_year),

    # Deuda media pre-HIPC por país (referencia para H5)
    dxp_pre = mean(dxp[year < completion_year], na.rm = TRUE),

    # Variación deuda respecto al nivel pre-HIPC
    dxp_change = dxp - dxp_pre

  ) %>%
  ungroup()

# Verificar
cat(sprintf("time_since_comp reconstruido — NAs: %d\n",
            sum(is.na(panel_h1$time_since_comp))))
cat(sprintf("rel_year_f — niveles: %d\n", nlevels(panel_h1$rel_year_f)))
cat(sprintf("post_completion — tratados: %d obs (%.1f%%)\n\n",
            sum(panel_h1$post_completion),
            mean(panel_h1$post_completion)*100))

# =============================================================================
# §1.2  ESTADÍSTICOS DESCRIPTIVOS PRE/POST
# =============================================================================
cat("=== §1.2 Descriptivos pre/post ===\n")

desc_prepost <- panel_h1 %>%
  group_by(post_completion) %>%
  summarise(
    n_obs       = n(),
    dxp_media   = round(mean(dxp,      na.rm=TRUE), 2),
    dxp_sd      = round(sd(dxp,        na.rm=TRUE), 2),
    gdpg_media  = round(mean(gdpg,     na.rm=TRUE), 2),
    sdi_media   = round(mean(sdi,      na.rm=TRUE), 2),
    wgi_media   = round(mean(wgi_comp, na.rm=TRUE), 3),
    inf_media   = round(mean(inf,      na.rm=TRUE), 2),
    .groups     = "drop"
  ) %>%
  mutate(periodo = ifelse(post_completion == 0, "Pre-culminación", "Post-culminación"))

print(desc_prepost)

# Test de medias (t-test)
t_dxp  <- t.test(dxp  ~ post_completion, data = panel_h1)
t_gdpg <- t.test(gdpg ~ post_completion, data = panel_h1)
cat(sprintf("\nDiferencia dxp  pre/post: t = %.3f, p = %.4f %s\n",
            t_dxp$statistic, t_dxp$p.value,
            ifelse(t_dxp$p.value < 0.05, "✅", "—")))
cat(sprintf("Diferencia gdpg pre/post: t = %.3f, p = %.4f %s\n\n",
            t_gdpg$statistic, t_gdpg$p.value,
            ifelse(t_gdpg$p.value < 0.05, "✅", "—")))

write.xlsx(desc_prepost, "resultados_H1/Tabla1_descriptivos_prepost.xlsx",
           rowNames = FALSE, overwrite = TRUE)

# =============================================================================
# §1.3  DiD BASE — TWFE (Two-Way Fixed Effects)
# Y_it = α_i + λ_t + β·post_completion_it + X_it·γ + ε_it
# Variables dependientes: dxp (deuda/PIB), gdpg (crecimiento PIB)
# =============================================================================
cat("=== §1.3 DiD base (TWFE) ===\n")

# Datos como panel plm
panel_plm <- pdata.frame(panel_h1, index = c("iso3", "year"))

# ── Modelo 1: dxp sin controles ──────────────────────────────
m1 <- plm(dxp ~ post_completion,
          data   = panel_plm,
          model  = "within",
          effect = "twoways")

# ── Modelo 2: dxp con controles ──────────────────────────────
m2 <- plm(dxp ~ post_completion + ln_gdppc + trade + inf + fdi_w +
            wgi_comp_lag + ln_relief,
          data   = panel_plm,
          model  = "within",
          effect = "twoways")

# ── Modelo 3: gdpg con controles ─────────────────────────────
m3 <- plm(gdpg ~ post_completion + ln_gdppc + trade + inf + fdi_w +
            wgi_comp_lag + ln_relief,
          data   = panel_plm,
          model  = "within",
          effect = "twoways")

# ── Modelo 4: dxp con FCF (donde disponible) ─────────────────
m4 <- plm(dxp ~ post_completion + ln_gdppc + trade + inf + fdi_w +
            wgi_comp_lag + ln_relief + FCF,
          data   = panel_plm,
          model  = "within",
          effect = "twoways")

# Errores estándar robustos clusterizados por país
se_rob <- function(m) coeftest(m, vcov = vcovHC(m, type="HC1", cluster="group"))

cat("\n── Modelo 1: dxp sin controles ──\n");      print(se_rob(m1))
cat("\n── Modelo 2: dxp con controles ──\n");      print(se_rob(m2))
cat("\n── Modelo 3: gdpg con controles ──\n");     print(se_rob(m3))
cat("\n── Modelo 4: dxp + FCF ──\n");              print(se_rob(m4))

# Test F conjunto
cat("\n── Tests F (efectos fijos) ──\n")
cat("M1:", sprintf("F = %.3f, p = %.4f\n", pFtest(m1, plm(dxp ~ post_completion, data=panel_plm, model="pooling"))$statistic, pFtest(m1, plm(dxp ~ post_completion, data=panel_plm, model="pooling"))$p.value))

# Hausman test (FE vs RE)
m2_re <- plm(dxp ~ post_completion + ln_gdppc + trade + inf + fdi_w +
               wgi_comp_lag + ln_relief,
             data=panel_plm, model="random")
haus <- phtest(m2, m2_re)
cat(sprintf("Hausman test: χ² = %.3f, p = %.4f → %s\n\n",
            haus$statistic, haus$p.value,
            ifelse(haus$p.value < 0.05, "✅ FE consistente", "RE eficiente")))

# Tabla resumen DiD
tabla_did <- data.frame(
  Modelo     = c("M1: dxp sin controles", "M2: dxp con controles",
                 "M3: gdpg con controles", "M4: dxp + FCF"),
  beta_post  = c(coef(m1)["post_completion"], coef(m2)["post_completion"],
                 coef(m3)["post_completion"], coef(m4)["post_completion"]),
  se_robusto = c(se_rob(m1)["post_completion","Std. Error"],
                 se_rob(m2)["post_completion","Std. Error"],
                 se_rob(m3)["post_completion","Std. Error"],
                 se_rob(m4)["post_completion","Std. Error"]),
  p_valor    = c(se_rob(m1)["post_completion","Pr(>|t|)"],
                 se_rob(m2)["post_completion","Pr(>|t|)"],
                 se_rob(m3)["post_completion","Pr(>|t|)"],
                 se_rob(m4)["post_completion","Pr(>|t|)"]),
  N          = c(nobs(m1), nobs(m2), nobs(m3), nobs(m4))
) %>%
  mutate(
    beta_post  = round(beta_post,  3),
    se_robusto = round(se_robusto, 3),
    p_valor    = round(p_valor,    4),
    sig        = ifelse(p_valor < 0.01, "***",
                 ifelse(p_valor < 0.05, "**",
                 ifelse(p_valor < 0.10, "*", "")))
  )

cat("\n── TABLA RESUMEN DiD ──\n")
print(tabla_did)
write.xlsx(tabla_did, "resultados_H1/Tabla2_DiD_TWFE.xlsx",
           rowNames = FALSE, overwrite = TRUE)

# Criterio de falsación H1:
# β(post_completion) negativo y significativo al 5% en M2
beta_h1 <- coef(m2)["post_completion"]
pval_h1 <- se_rob(m2)["post_completion","Pr(>|t|)"]
cat(sprintf("\n── CRITERIO FALSACIÓN H1 ──\n"))
cat(sprintf("β(post_completion) en M2: %.3f (p = %.4f)\n", beta_h1, pval_h1))
cat(sprintf("H1 %s: efecto %s y %s al 5%%\n\n",
            ifelse(beta_h1 < 0 & pval_h1 < 0.05, "NO RECHAZADA ✅", "RECHAZADA ❌"),
            ifelse(beta_h1 < 0, "negativo", "positivo"),
            ifelse(pval_h1 < 0.05, "significativo", "no significativo")))

# =============================================================================
# §1.4  EVENT-STUDY — DINÁMICA TEMPORAL
# Y_it = α_i + λ_t + Σ_k β_k · D(rel_year=k)_it + X_it·γ + ε_it
# Ref: k = -1; ventana: k ∈ {-5,...,10}
# Criterio tendencias paralelas: β_k no significativos para k < 0
# =============================================================================
cat("=== §1.4 Event-study ===\n")

# Modelo event-study con controles
es_m <- plm(dxp ~ rel_year_f + ln_gdppc + trade + inf + fdi_w + wgi_comp_lag,
            data   = panel_plm,
            model  = "within",
            effect = "twoways")

es_coef <- se_rob(es_m)

# Extraer coeficientes de rel_year_f
es_df <- as.data.frame(es_coef[grepl("rel_year_f", rownames(es_coef)), ]) %>%
  rownames_to_column("term") %>%
  rename(estimate = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`) %>%
  mutate(
    k     = as.integer(gsub("rel_year_f", "", term)),
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se,
    sig   = pval < 0.05
  ) %>%
  arrange(k)

# Añadir punto de referencia k = -1 (β = 0 por construcción)
ref_row <- data.frame(term="ref", estimate=0, se=0, pval=1,
                      k=-1L, lower=0, upper=0, sig=FALSE)
es_df <- bind_rows(es_df, ref_row) %>% arrange(k)

cat("\nCoeficientes event-study:\n")
print(es_df %>% select(k, estimate, se, pval, sig))

# Test tendencias paralelas: F-test sobre coeficientes pre (k < -1)
pre_terms <- rownames(es_coef)[grepl("rel_year_f-[2-5]", rownames(es_coef))]
if (length(pre_terms) >= 2) {
  cat(sprintf("\nTest tendencias paralelas (k < -1): %d coeficientes pre\n",
              length(pre_terms)))
  pre_sig <- sum(es_df$pval[es_df$k < -1] < 0.05, na.rm=TRUE)
  cat(sprintf("Coeficientes pre significativos: %d de %d → %s\n",
              pre_sig, length(pre_terms),
              ifelse(pre_sig == 0, "✅ Tendencias paralelas validadas",
                     "⚠️  Posible violación de tendencias paralelas")))
}

# Figura event-study (Figura 3.6 de la tesis)
fig_es <- ggplot(es_df, aes(x = k, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "#D35400",
             linewidth = 0.8) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#D6E4F7", alpha = 0.5) +
  geom_line(color = "#1F3864", linewidth = 0.9) +
  geom_point(aes(color = sig), size = 2.5) +
  scale_color_manual(values = c("FALSE" = "#717D7E", "TRUE" = "#D35400"),
                     labels = c("No sig.", "Sig. 5%"),
                     name   = "") +
  annotate("text", x = -0.3, y = max(es_df$upper, na.rm=TRUE)*0.9,
           label = "Completion\npoint", color = "#D35400", size = 3,
           hjust = 0) +
  labs(title    = "Event-study: efecto de la culminación HIPC sobre Deuda/PIB",
       subtitle = "Efectos fijos bidireccionales | IC 95% | Referencia: t = -1",
       x        = "Años respecto al completion point",
       y        = "Efecto estimado sobre dxp (p.p.)") +
  scale_x_continuous(breaks = seq(-5, 10, 1)) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "gray40", size = 9),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

ggsave("resultados_H1/Fig3.6_event_study_dxp.png",
       fig_es, width = 9, height = 5.5, dpi = 150)
cat("✓ Fig3.6 guardada\n\n")

write.xlsx(es_df %>% select(k, estimate, se, lower, upper, pval, sig),
           "resultados_H1/Tabla3_event_study.xlsx",
           rowNames = FALSE, overwrite = TRUE)

# =============================================================================
# §1.5  HETEROGENEIDAD — SUBGRUPOS
# Interacciones: post_completion × gobernanza alta/baja
#                post_completion × región
# =============================================================================
cat("=== §1.5 Heterogeneidad ===\n")

panel_h1 <- panel_h1 %>%
  mutate(
    # Dummy gobernanza alta (por encima de mediana en el panel)
    wgi_alto = as.integer(wgi_comp > median(wgi_comp, na.rm=TRUE)),
    # Interacción tratamiento × gobernanza
    post_x_wgi = post_completion * wgi_comp_lag
  )

panel_plm2 <- pdata.frame(panel_h1, index = c("iso3","year"))

# Modelo con interacción continua
m5 <- plm(dxp ~ post_completion * wgi_comp_lag + ln_gdppc + trade +
            inf + fdi_w + ln_relief,
          data   = panel_plm2,
          model  = "within",
          effect = "twoways")

cat("\n── Modelo 5: dxp con interacción post × wgi_comp_lag ──\n")
print(se_rob(m5))

beta_inter <- coef(m5)["post_completion:wgi_comp_lag"]
pval_inter <- se_rob(m5)["post_completion:wgi_comp_lag","Pr(>|t|)"]
cat(sprintf("\nInteracción post×wgi: β = %.3f (p = %.4f) → %s\n\n",
            beta_inter, pval_inter,
            ifelse(beta_inter < 0 & pval_inter < 0.05,
                   "✅ Mayor gobernanza → mayor reducción deuda",
                   "— efecto interacción no significativo")))

# =============================================================================
# §1.6  ROBUSTEZ
# =============================================================================
cat("=== §1.6 Pruebas de robustez ===\n")

# ── Test placebo: asignar completion_year ficticio (comp_year - 5) ───────────
panel_h1_plac <- panel_h1 %>%
  mutate(
    comp_year_plac  = completion_year - 5,
    post_plac       = as.integer(year >= comp_year_plac &
                                   year < completion_year)
  )

panel_plm_plac <- pdata.frame(panel_h1_plac, index=c("iso3","year"))

m_plac <- plm(dxp ~ post_plac + ln_gdppc + trade + inf + fdi_w + wgi_comp_lag,
              data   = panel_plm_plac,
              model  = "within",
              effect = "twoways")

beta_plac <- coef(m_plac)["post_plac"]
pval_plac <- se_rob(m_plac)["post_plac","Pr(>|t|)"]
cat(sprintf("Test placebo (comp_year-5): β = %.3f, p = %.4f → %s\n",
            beta_plac, pval_plac,
            ifelse(pval_plac > 0.10,
                   "✅ Placebo no significativo (robusto)",
                   "⚠️  Placebo significativo — revisar")))

# ── Test anticipación: excluir los 2 años previos al completion ──────────────
panel_no_antic <- panel_h1 %>%
  filter(!(rel_year %in% c(-2, -1)))

panel_plm_na <- pdata.frame(panel_no_antic, index=c("iso3","year"))

m_noanticip <- plm(dxp ~ post_completion + ln_gdppc + trade + inf +
                     fdi_w + wgi_comp_lag + ln_relief,
                   data   = panel_plm_na,
                   model  = "within",
                   effect = "twoways")

beta_na <- coef(m_noanticip)["post_completion"]
pval_na <- se_rob(m_noanticip)["post_completion","Pr(>|t|)"]
cat(sprintf("Sin años anticipación:      β = %.3f, p = %.4f → %s\n",
            beta_na, pval_na,
            ifelse(sign(beta_na) == sign(beta_h1) & pval_na < 0.05,
                   "✅ Resultado robusto",
                   "⚠️  Resultado cambia")))

# ── Resumen robustez ─────────────────────────────────────────────────────────
tabla_rob <- data.frame(
  Especificacion = c("M2: baseline", "M4: + FCF",
                     "M5: + interacción wgi",
                     "Placebo (comp-5)",
                     "Sin años anticipación"),
  beta           = round(c(coef(m2)["post_completion"],
                            coef(m4)["post_completion"],
                            coef(m5)["post_completion"],
                            beta_plac, beta_na), 3),
  p_valor        = round(c(pval_h1,
                            se_rob(m4)["post_completion","Pr(>|t|)"],
                            se_rob(m5)["post_completion","Pr(>|t|)"],
                            pval_plac, pval_na), 4),
  N              = c(nobs(m2), nobs(m4), nobs(m5),
                     nobs(m_plac), nobs(m_noanticip))
) %>%
  mutate(sig = ifelse(p_valor<0.01,"***",
                ifelse(p_valor<0.05,"**",
                ifelse(p_valor<0.10,"*",""))))

cat("\n── TABLA ROBUSTEZ H1 ──\n")
print(tabla_rob)
write.xlsx(tabla_rob, "resultados_H1/Tabla4_robustez_H1.xlsx",
           rowNames = FALSE, overwrite = TRUE)

# =============================================================================
# GUARDAR RESULTADOS Y PANEL ACTUALIZADO
# =============================================================================
cat("\n=== Guardando resultados ===\n")

# Guardar panel con variables H1 añadidas
save(panel_h1, panel_complete, surv_data, pca_wgi,
     m1, m2, m3, m4, m5, es_df,
     file = "panel_HIPC.RData")

cat("✓ panel_HIPC.RData\n")
cat("✓ resultados_H1/Tabla1_descriptivos_prepost.xlsx\n")
cat("✓ resultados_H1/Tabla2_DiD_TWFE.xlsx\n")
cat("✓ resultados_H1/Tabla3_event_study.xlsx\n")
cat("✓ resultados_H1/Tabla4_robustez_H1.xlsx\n")
cat("✓ resultados_H1/Fig3.6_event_study_dxp.png\n")

# =============================================================================
# RESUMEN FINAL H1
# =============================================================================
cat("\n=== RESUMEN H1 ===\n")
cat(sprintf("DiD base     — β = %.3f, p = %.4f %s\n",
            beta_h1, pval_h1,
            ifelse(beta_h1 < 0 & pval_h1 < 0.05, "✅", "⚠️")))
cat(sprintf("Interacción  — β(post×wgi) = %.3f, p = %.4f %s\n",
            beta_inter, pval_inter,
            ifelse(pval_inter < 0.05, "✅", "—")))
cat(sprintf("Placebo      — β = %.3f, p = %.4f %s\n",
            beta_plac, pval_plac,
            ifelse(pval_plac > 0.10, "✅", "⚠️")))
cat(sprintf("Sin anticip. — β = %.3f, p = %.4f %s\n",
            beta_na, pval_na,
            ifelse(pval_na < 0.05, "✅", "⚠️")))

cat("\n=== FASE 1 COMPLETADA ===\n")
cat("Siguiente paso → fase2_H1_Synth.R (control sintético)\n")
cat("                 o fase3_H3_mediacion.R si priorizas impacto social\n")
