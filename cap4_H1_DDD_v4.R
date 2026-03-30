# =============================================================================
# CAP. 4 — §4.4  TRIPLE DIFERENCIA (DDD) — versión 4 (DEFINITIVA)
#
# Corrección: panel_control usa gdppc (niveles) no ln_gdppc (log),
# y fdi en lugar de fdi_w. Se reconstruye panel_ddd con las
# transformaciones correctas antes de combinar.
# =============================================================================

setwd("C:/Users/tomas/OneDrive/Documentos/Doctorado/TESIS/Datos")

library(tidyverse)
library(lmtest)
library(sandwich)
library(openxlsx)

cat("=== CAP. 4 §4.4 — DDD v4 ===\n\n")

# =============================================================================
# §1  RECONSTRUIR PANEL COMBINADO CON TRANSFORMACIONES CORRECTAS
# =============================================================================
cat("--- Cargando paneles fuente ---\n")

load("panel_HIPC.RData")         # contiene panel_complete
load("panel_control_nohipc.RData") # contiene panel_control

# ── Panel HIPC ────────────────────────────────────────────────────────────────
vars_hipc <- c("iso3","year","dxp","post_completion","ln_gdppc",
               "trade","inf","fdi_w","wgi_comp_lag","ln_relief",
               "ross_oil_prod","region","IGC","IGA")

panel_hipc_sub <- panel_complete %>%
  select(any_of(vars_hipc)) %>%
  mutate(
    hipc_group = 1L,
    post_ddd   = post_completion
  )

cat(sprintf("HIPC: %d obs | %d países\n",
            nrow(panel_hipc_sub), n_distinct(panel_hipc_sub$iso3)))

# ── Panel control — transformaciones correctas ─────────────────────────────────
cat(sprintf("Control (raw): %d obs | %d países\n",
            nrow(panel_control), n_distinct(panel_control$iso3)))
cat(sprintf("Columnas control: %s\n\n", paste(names(panel_control), collapse=", ")))

panel_ctrl_sub <- panel_control %>%
  mutate(
    hipc_group  = 0L,
    post_completion = 0L,
    post_ddd    = as.integer(year >= 2004),   # umbral global post-HIPC
    # Transformaciones faltantes
    ln_gdppc    = if ("gdppc" %in% names(.)) log(gdppc + 1) else NA_real_,
    fdi_w       = if ("fdi"   %in% names(.)) fdi            else NA_real_,
    # Variables no disponibles en control → NA
    wgi_comp_lag = NA_real_,
    ln_relief    = NA_real_,
    ross_oil_prod= NA_real_,
    IGC          = NA_real_,
    IGA          = NA_real_,
    # region puede no estar — intentar rescatarla
    region       = if ("region" %in% names(.)) region else NA_character_
  ) %>%
  select(iso3, year, dxp, post_completion, ln_gdppc, trade, inf,
         fdi_w, wgi_comp_lag, ln_relief, ross_oil_prod, region,
         IGC, IGA, hipc_group, post_ddd)

cat(sprintf("Control (transformado): %d obs | %d países\n",
            nrow(panel_ctrl_sub), n_distinct(panel_ctrl_sub$iso3)))

# Verificar ln_gdppc en control
cat(sprintf("ln_gdppc NAs en control: %d / %d\n\n",
            sum(is.na(panel_ctrl_sub$ln_gdppc)), nrow(panel_ctrl_sub)))

# ── Combinar ──────────────────────────────────────────────────────────────────
panel_ddd <- bind_rows(panel_hipc_sub, panel_ctrl_sub) %>%
  filter(!is.na(dxp))

cat(sprintf("Panel combinado: %d obs | HIPC: %d países | Control: %d países\n\n",
            nrow(panel_ddd),
            n_distinct(panel_ddd$iso3[panel_ddd$hipc_group == 1]),
            n_distinct(panel_ddd$iso3[panel_ddd$hipc_group == 0])))

# =============================================================================
# §2  CONTROLES PRE-TRATAMIENTO (sustituyen EF de país)
# =============================================================================
pre_means <- panel_ddd %>%
  filter(year < 2000) %>%
  group_by(iso3) %>%
  summarise(
    dxp_pre0   = mean(dxp,      na.rm = TRUE),
    gdppc_pre0 = mean(ln_gdppc, na.rm = TRUE),
    trade_pre0 = mean(trade,    na.rm = TRUE),
    inf_pre0   = mean(inf,      na.rm = TRUE),
    .groups    = "drop"
  )

panel_ddd <- panel_ddd %>% left_join(pre_means, by = "iso3")

# =============================================================================
# §3  PANEL DE ESTIMACIÓN — VERIFICAR AMBOS GRUPOS PRESENTES
# =============================================================================
panel_est <- panel_ddd %>%
  filter(!is.na(dxp), !is.na(post_ddd),
         !is.na(ln_gdppc), !is.na(trade), !is.na(inf),
         !is.na(dxp_pre0), !is.na(gdppc_pre0))

cat(sprintf("Panel estimación: %d obs | HIPC: %d | Control: %d\n\n",
            nrow(panel_est),
            n_distinct(panel_est$iso3[panel_est$hipc_group == 1]),
            n_distinct(panel_est$iso3[panel_est$hipc_group == 0])))

# Verificar que hipc_group varía en el panel de estimación
stopifnot("Grupo control vacío — revisar transformaciones" =
          n_distinct(panel_est$hipc_group) == 2)

# =============================================================================
# §4  MODELOS DDD
# Especificación: dxp ~ hipc_group * post_ddd + controles + factor(year)
# hipc_group:post_ddd = β de interés (efecto neto HIPC vs. tendencia control)
# =============================================================================
cat("--- Estimando modelos DDD ---\n\n")

se_cl <- function(mod, data) {
  coeftest(mod, vcov = vcovCL(mod, cluster = ~iso3, data = data))
}

controles_niv <- intersect(c("ln_gdppc","trade","inf","fdi_w"), names(panel_est))
controles_pre <- intersect(c("dxp_pre0","gdppc_pre0","trade_pre0","inf_pre0"),
                            names(panel_est))

# ── M1: solo EF año ────────────────────────────────────────────────────────
m1 <- lm(dxp ~ hipc_group * post_ddd + factor(year), data = panel_est)
ct1 <- se_cl(m1, panel_est)
cat("M1 (EF año):\n")
print(ct1["hipc_group:post_ddd", ])

# ── M2: + controles de nivel ────────────────────────────────────────────────
f2  <- as.formula(paste("dxp ~ hipc_group * post_ddd +",
                         paste(c(controles_niv,"factor(year)"), collapse=" + ")))
m2  <- lm(f2, data = panel_est)
ct2 <- se_cl(m2, panel_est)
cat("\nM2 (+ controles nivel):\n")
print(ct2["hipc_group:post_ddd", ])

# ── M3: + controles pre-tratamiento ─────────────────────────────────────────
f3  <- as.formula(paste("dxp ~ hipc_group * post_ddd +",
                         paste(c(controles_niv, controles_pre,
                                 "factor(year)"), collapse=" + ")))
m3  <- lm(f3, data = panel_est)
ct3 <- se_cl(m3, panel_est)
cat("\nM3 (+ controles pre-tratamiento):\n")
print(ct3["hipc_group:post_ddd", ])

# ── M4: + EF región × año ───────────────────────────────────────────────────
panel_m4 <- panel_est %>%
  filter(!is.na(region)) %>%
  mutate(reg_yr = interaction(region, year, sep="_", drop=TRUE))

m4 <- tryCatch(
  lm(as.formula(paste("dxp ~ hipc_group * post_ddd +",
                       paste(c(controles_niv, controles_pre,
                               "factor(reg_yr)"), collapse=" + "))),
     data = panel_m4),
  error = function(e) { cat("M4 error:", e$message, "\n"); NULL }
)
ct4 <- if (!is.null(m4)) tryCatch(se_cl(m4, panel_m4), error=function(e) NULL) else NULL
cat("\nM4 (+ EF región×año):\n")
if (!is.null(ct4) && "hipc_group:post_ddd" %in% rownames(ct4)) {
  print(ct4["hipc_group:post_ddd", ])
} else cat("No convergió\n")

# =============================================================================
# §5  TABLA RESUMEN
# =============================================================================
sacar <- function(ct, mod, lbl) {
  v <- "hipc_group:post_ddd"
  if (is.null(ct) || !v %in% rownames(ct))
    return(tibble(Modelo=lbl, beta=NA_real_, SE=NA_real_, p=NA_real_,
                  sig="", N=NA_integer_))
  r <- ct[v, ]; p <- r["Pr(>|t|)"]
  tibble(Modelo=lbl,
         beta=round(r["Estimate"],3), SE=round(r["Std. Error"],3),
         p=round(p,4),
         sig=case_when(p<.01~"***",p<.05~"**",p<.10~"*",TRUE~""),
         N=nobs(mod))
}

tabla <- bind_rows(
  sacar(ct1, m1, "M1: DDD + EF año"),
  sacar(ct2, m2, "M2: + controles nivel"),
  sacar(ct3, m3, "M3: + controles pre-tratamiento"),
  sacar(ct4, m4, "M4: + EF región×año")
)

cat("\n\n=== TABLA RESUMEN DDD ===\n")
print(tabla)

ref <- tabla$beta[tabla$Modelo == "M3: + controles pre-tratamiento"]
p3  <- tabla$p[tabla$Modelo   == "M3: + controles pre-tratamiento"]

cat(sprintf("\nEfecto bruto DiD (§4.1):  β = -39.311 pp. (p = 0.0004)\n"))
cat(sprintf("Efecto neto DDD (M3):     β = %.3f pp.  (p = %.4f) %s\n",
            ref, p3, ifelse(!is.na(ref) & ref < 0 & p3 < 0.05, "✅", "⚠️")))

# =============================================================================
# §6  GUARDAR
# =============================================================================
dir.create("resultados_H1", showWarnings = FALSE)
write.xlsx(tabla, "resultados_H1/Tabla4.4_DDD_v4.xlsx",
           rowNames=FALSE, overwrite=TRUE)
save(panel_ddd, panel_est, m1, m2, m3, m4, ct1, ct2, ct3, ct4, tabla,
     file = "resultados_H1/ddd_resultados_v4.RData")

cat("\n✓ Tabla4.4_DDD_v4.xlsx\n")
cat("✓ ddd_resultados_v4.RData\n")
cat("=== §4.4 DDD COMPLETADO ===\n")
cat("Siguiente: cap4_H2_BaiPerron.R\n")
