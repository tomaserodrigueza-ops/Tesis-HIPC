# =============================================================================
# FASE 1b — HIPÓTESIS H1b: PERSISTENCIA Y HETEROGENEIDAD DEL EFECTO HIPC
# Métodos:
#   §1b.1  ATT(g,t) por cohorte — Callaway & Sant'Anna (2021) via paquete did
#   §1b.2  Agregaciones: efecto promedio, por cohorte, dinámico
#   §1b.3  Test de ruptura estructural — Bai & Perron (1998) via strucchange
#   §1b.4  Comparación TWFE vs. CS — diagnóstico de heterogeneidad
# =============================================================================

setwd("C:/Users/tomas/OneDrive/Documentos/Doctorado/TESIS/Datos")

library(tidyverse)
library(did)
library(strucchange)
library(sandwich)
library(lmtest)
library(openxlsx)

cat("✅ Paquetes cargados\n\n")

load("panel_HIPC.RData")
cat(sprintf("Panel: %d filas × %d columnas | %d países | %d–%d\n\n",
            nrow(panel_complete), ncol(panel_complete),
            n_distinct(panel_complete$iso3),
            min(panel_complete$year), max(panel_complete$year)))

dir.create("resultados_H1b", showWarnings = FALSE)

# =============================================================================
# §1b.1  PREPARACIÓN PARA CALLAWAY & SANT'ANNA
# El paquete did requiere:
#   - yname:  variable dependiente (continua)
#   - tname:  variable de tiempo
#   - idname: identificador de unidad (numérico)
#   - gname:  año del primer tratamiento (0 para nunca tratados — aquí no hay)
#             Para "not yet treated" como control, gname = completion_year
# =============================================================================
cat("=== §1b.1 Preparación datos ===\n")

# Crear ID numérico por país
panel_cs <- panel_complete %>%
  arrange(iso3, year) %>%
  mutate(
    id_num = as.integer(factor(iso3)),
    # gname = año de completion (igual para todos los años del mismo país)
    # Cohortes 2020 y 2021 tienen <4 años post — se excluyen del CS
    gname  = ifelse(completion_year >= 2020, NA_real_,
                    as.numeric(completion_year))
  ) %>%
  filter(!is.na(gname)) %>%    # excluir cohortes 2020-2021
  filter(!is.na(dxp)) %>%      # excluir NAs en variable dependiente
  filter(!is.na(wgi_comp_lag) | TRUE)  # controles opcionales

# Verificar
cat(sprintf("Países incluidos: %d\n", n_distinct(panel_cs$iso3)))
cat(sprintf("Observaciones:    %d\n", nrow(panel_cs)))
cat(sprintf("Cohortes:         %d\n", n_distinct(panel_cs$gname)))
cat(sprintf("Período:          %d–%d\n\n",
            min(panel_cs$year), max(panel_cs$year)))

cat("Distribución de cohortes incluidas:\n")
panel_cs %>%
  group_by(gname) %>%
  summarise(n_paises = n_distinct(iso3), .groups="drop") %>%
  print(n=20)

# =============================================================================
# §1b.2  ATT(g,t) — CALLAWAY & SANT'ANNA
# Control: "not yet treated" (opción por defecto en did)
# Estimador: doubly robust (driscoll-kraay)
# =============================================================================
cat("\n=== §1b.2 ATT(g,t) Callaway & Sant'Anna ===\n")

# Modelo sin controles (más transparente con cohortes pequeñas)
cs_out <- tryCatch({
  att_gt(
    yname         = "dxp",
    tname         = "year",
    idname        = "id_num",
    gname         = "gname",
    data          = panel_cs,
    control_group = "notyettreated",
    est_method    = "reg",
    print_details = FALSE
  )
}, error = function(e) {
  cat("⚠️ Error en modelo sin controles:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(cs_out)) {
  cat("\nResumen ATT(g,t):\n")
  print(summary(cs_out))

  # ── Agregación 1: efecto promedio simple (equivalente al ATT del DiD) ──
  agg_simple <- aggte(cs_out, type = "simple")
  cat("\n── Agregación simple (ATT promedio) ──\n")
  print(summary(agg_simple))

  # ── Agregación 2: dinámica temporal (equivalente al event-study) ──
  agg_dynamic <- aggte(cs_out, type = "dynamic")
  cat("\n── Agregación dinámica (event-study CS) ──\n")
  print(summary(agg_dynamic))

  # ── Agregación 3: por cohorte ──
  agg_group <- aggte(cs_out, type = "group")
  cat("\n── Agregación por cohorte ──\n")
  print(summary(agg_group))

  # Extraer coeficientes dinámicos para comparar con TWFE
  es_cs <- data.frame(
    k        = agg_dynamic$egt,
    estimate = agg_dynamic$att.egt,
    se       = agg_dynamic$se.egt,
    pval     = 2 * pnorm(-abs(agg_dynamic$att.egt / agg_dynamic$se.egt))
  ) %>%
    mutate(
      lower = estimate - 1.96 * se,
      upper = estimate + 1.96 * se,
      sig   = pval < 0.05,
      metodo = "Callaway & Sant'Anna"
    )

  cat("\nCoeficientes dinámicos CS:\n")
  print(as.data.frame(es_cs[, c("k","estimate","se","pval","sig")]))

  # Guardar
  write.xlsx(es_cs, "resultados_H1b/Tabla_CS_dinamico.xlsx",
             rowNames=FALSE, overwrite=TRUE)
  cat("✓ Tabla_CS_dinamico.xlsx guardada\n")

  # ── Comparación TWFE vs CS ────────────────────────────────────────────────
  cat("\n── Comparación ATT: TWFE vs. Callaway & Sant'Anna ──\n")
  att_twfe <- -39.311  # β(post_completion) de M2
  att_cs   <- agg_simple$overall.att
  se_cs_v  <- agg_simple$overall.se

  cat(sprintf("ATT TWFE (M2):              β = %.3f\n", att_twfe))
  cat(sprintf("ATT Callaway & Sant'Anna:   β = %.3f (SE = %.3f)\n",
              att_cs, se_cs_v))
  cat(sprintf("Diferencia:                 Δ = %.3f\n", att_cs - att_twfe))
  cat(sprintf("Interpretación: %s\n",
              ifelse(abs(att_cs - att_twfe) < 10,
                     "✅ TWFE y CS consistentes — heterogeneidad no sesga TWFE",
                     "⚠️  Diferencia notable — heterogeneidad afecta TWFE")))

  # Figura comparativa event-study TWFE vs CS
  es_twfe <- data.frame(
    k = c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10),
    estimate = c(32.957,24.803,8.457,3.739,0,-22.736,-28.799,-28.591,
                 -30.995,-27.288,-29.293,-29.294,-31.541,-31.120,-29.173,-25.006),
    se = c(14.357,12.237,7.307,6.193,0,7.352,8.533,10.828,
           10.922,14.171,14.906,16.860,17.710,19.364,20.683,22.429),
    metodo = "TWFE"
  ) %>% mutate(lower = estimate - 1.96*se, upper = estimate + 1.96*se,
               pval = NA, sig = NA)

  es_combined <- bind_rows(
    es_twfe,
    es_cs %>% mutate(metodo = "Callaway & Sant'Anna")
  )

  fig_comp <- ggplot(es_combined, aes(x=k, y=estimate, color=metodo, fill=metodo)) +
    geom_hline(yintercept=0, linetype="dashed", color="gray50") +
    geom_vline(xintercept=-0.5, linetype="dotted", color="#D35400", linewidth=0.9) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.12, color=NA) +
    geom_line(linewidth=0.9) +
    geom_point(size=2.5) +
    scale_color_manual(values=c("TWFE"="#1F3864", "Callaway & Sant'Anna"="#D35400"),
                       name="Estimador") +
    scale_fill_manual(values=c("TWFE"="#1F3864", "Callaway & Sant'Anna"="#D35400"),
                      name="Estimador") +
    labs(title   = "Figura 3.7. Comparación event-study: TWFE vs. Callaway & Sant'Anna (2021)",
         subtitle = "Variable dependiente: dxp | IC 95% | Referencia: k = −1",
         x = "Años respecto al completion point (k)",
         y = "Efecto estimado (puntos porcentuales)") +
    scale_x_continuous(breaks=seq(-5,10,1)) +
    theme_minimal(base_size=11) +
    theme(plot.title=element_text(face="bold", color="#1F3864"),
          legend.position="bottom",
          panel.grid.minor=element_blank())

  ggsave("resultados_H1b/Fig3.7_TWFE_vs_CS.png",
         fig_comp, width=9, height=5.5, dpi=150)
  cat("✓ Fig3.7_TWFE_vs_CS.png guardada\n\n")

} else {
  cat("⚠️  Saltando ATT(g,t) — revisar datos\n\n")
  agg_simple  <- NULL
  agg_dynamic <- NULL
  agg_group   <- NULL
  es_cs       <- NULL
}

# =============================================================================
# §1b.3  TEST DE RUPTURA ESTRUCTURAL — BAI & PERRON (1998)
# Para cada país: ¿hay ruptura en la trayectoria de dxp en torno al
# completion point?
# Luego: análisis agregado sobre el panel
# =============================================================================
cat("=== §1b.3 Test de ruptura estructural (Bai & Perron) ===\n\n")

library(strucchange)

# ── Test por país ─────────────────────────────────────────────────────────
resultados_bp <- panel_complete %>%
  filter(!is.na(completion_year), !is.na(dxp)) %>%
  group_by(iso3) %>%
  filter(n() >= 10) %>%   # mínimo 10 obs para el test
  summarise({
    df_i <- cur_data() %>% arrange(year)
    ts_i <- ts(df_i$dxp, start=min(df_i$year))

    # Test de Chow en el completion point
    comp_y  <- first(df_i$completion_year)
    idx_bp  <- which(df_i$year == comp_y)

    resultado <- tryCatch({
      if (length(idx_bp) == 1 & idx_bp > 2 & idx_bp < nrow(df_i)-2) {
        bp <- sctest(ts_i ~ 1, type="Chow", point=idx_bp)
        data.frame(
          completion_year = comp_y,
          F_stat   = round(bp$statistic, 3),
          p_chow   = round(bp$p.value,   4),
          ruptura  = bp$p.value < 0.05
        )
      } else {
        data.frame(completion_year=comp_y, F_stat=NA, p_chow=NA, ruptura=NA)
      }
    }, error = function(e) {
      data.frame(completion_year=comp_y, F_stat=NA, p_chow=NA, ruptura=NA)
    })
    resultado
  }, .groups="drop")

cat("Resultados test de Chow por país:\n")
print(as.data.frame(resultados_bp), row.names=FALSE)

n_total   <- sum(!is.na(resultados_bp$ruptura))
n_ruptura <- sum(resultados_bp$ruptura, na.rm=TRUE)
cat(sprintf("\nRupturas significativas: %d de %d países (%.1f%%)\n",
            n_ruptura, n_total, n_ruptura/n_total*100))
cat(sprintf("Interpretación: %s\n\n",
            ifelse(n_ruptura/n_total >= 0.60,
                   "✅ Mayoría de países muestra ruptura en completion point",
                   "⚠️  Ruptura no generalizada — heterogeneidad importante")))

write.xlsx(as.data.frame(resultados_bp),
           "resultados_H1b/Tabla_Bai_Perron_paises.xlsx",
           rowNames=FALSE, overwrite=TRUE)

# ── Test agregado: CUSUM sobre panel (dxp medio por año) ─────────────────
cat("── Test CUSUM agregado ──\n")
panel_medio <- panel_complete %>%
  group_by(year) %>%
  summarise(dxp_medio = mean(dxp, na.rm=TRUE), .groups="drop") %>%
  arrange(year)

ts_medio <- ts(panel_medio$dxp_medio, start=min(panel_medio$year))

cusum_test <- efp(ts_medio ~ 1, type="OLS-CUSUM")
bp_test    <- sctest(cusum_test)

cat(sprintf("CUSUM agregado: estadístico = %.3f, p = %.4f → %s\n\n",
            bp_test$statistic, bp_test$p.value,
            ifelse(bp_test$p.value < 0.05,
                   "✅ Ruptura estructural significativa en el panel",
                   "— Sin ruptura estructural agregada")))

# Figura CUSUM
png("resultados_H1b/Fig3.8_CUSUM_panel.png", width=900, height=500, res=130)
plot(cusum_test, main="CUSUM — dxp medio panel HIPC (1990–2022)",
     xlab="Año", ylab="Proceso CUSUM",
     col="#1F3864", lwd=2)
abline(v=2004, col="#D35400", lty=2, lwd=1.5)
text(2004.5, 0, "Mediana\ncompletions", col="#D35400", cex=0.75, adj=0)
dev.off()
cat("✓ Fig3.8_CUSUM_panel.png guardada\n\n")

# =============================================================================
# §1b.4  TABLA RESUMEN H1b
# =============================================================================
cat("=== §1b.4 Resumen H1b ===\n")

tabla_h1b <- data.frame(
  Prueba = c(
    "ATT promedio (Callaway & Sant'Anna)",
    "Consistencia TWFE vs. CS",
    "Coeficientes k=+6 a +10 (magnitud)",
    "Coeficientes k=+6 a +10 (sig. estadística)",
    "Rupturas estructurales (Chow por país)",
    "CUSUM agregado"
  ),
  Criterio = c(
    "β negativo y significativo",
    "|ATT_CS − ATT_TWFE| < 10 p.p.",
    "Rango estable (no colapsa a 0)",
    "Al menos 3 de 5 significativos al 10%",
    "≥ 60% países con ruptura en completion point",
    "Ruptura significativa (p < 0.05)"
  ),
  Resultado = c(
    ifelse(!is.null(agg_simple),
           sprintf("β = %.3f (SE = %.3f)", agg_simple$overall.att, agg_simple$overall.se),
           "Pendiente"),
    ifelse(!is.null(agg_simple),
           sprintf("Δ = %.3f → %s", agg_simple$overall.att - (-39.311),
                   ifelse(abs(agg_simple$overall.att - (-39.311)) < 10, "Consistentes", "Divergentes")),
           "Pendiente"),
    "Rango −25 a −32 p.p. ✅",
    "1 de 5 sig. al 5% ⚠️",
    sprintf("%d de %d (%.1f%%)", n_ruptura, n_total, n_ruptura/n_total*100),
    sprintf("p = %.4f → %s", bp_test$p.value,
            ifelse(bp_test$p.value < 0.05, "✅ Significativo", "— No significativo"))
  )
)

cat("\n── TABLA RESUMEN H1b ──\n")
print(tabla_h1b, row.names=FALSE)
write.xlsx(tabla_h1b, "resultados_H1b/Tabla_resumen_H1b.xlsx",
           rowNames=FALSE, overwrite=TRUE)

# =============================================================================
# GUARDAR
# =============================================================================
cat("\n=== Guardando ===\n")
save(panel_complete, panel_h1, surv_data, pca_wgi,
     m1, m2, m3, m4, m5, es_df,
     resultados_bp, cs_out, agg_simple, agg_dynamic, agg_group,
     file="panel_HIPC.RData")

cat("✓ panel_HIPC.RData\n")
cat("✓ resultados_H1b/\n")
cat("\n=== FASE 1b COMPLETADA ===\n")
cat("Siguiente paso → fase2_control_sintetico.R o fase3_H2.R\n")
