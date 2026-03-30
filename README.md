# Tesis doctoral — Iniciativa HIPC y sostenibilidad de la deuda externa

**Título completo:**  
*Iniciativa HIPC y sostenibilidad de la deuda externa: un análisis cuantitativo comparado de condicionalidades, impacto social y gobernanza institucional*

**Autor:** Tomás Rodríguez A.  
**Programa:** Doctorado en Economía  
**Panel:** 39 países HIPC · 1990–2022 · 1.287 obs × 72 variables  
**Repositorio:** https://github.com/tomaserodrigueza-ops/Tesis-HIPC

---

## Estado de la tesis — 30 marzo 2026

### Estructura de capítulos

| Cap. | Título | Hipótesis | Archivo FINAL | Estado |
|------|--------|-----------|---------------|--------|
| 1 | Introducción general | — | `Cap1_Completo_FINAL.docx` | ✅ Completo |
| 2 | Marco teórico e histórico | Fundamento H1–H7 | `Cap2_Marco_teorico_FINAL.docx` | ✅ Completo |
| 3 | Diseño metodológico y estrategia de identificación causal | H1–H7 (estrategia) | `Cap3_Metodologia.docx` | ✅ Completo |
| 4 | Análisis cuantitativo de la sostenibilidad de la deuda | H1, H2, H7 | Ver §4.x abajo | 🔄 En progreso |
| 5 | Condicionalidades, gobernanza e impacto social | H3, H4, H5, H6 | — | ⏳ Pendiente |
| 6 | Análisis comparado y estudios de caso | H1–H7 aplicados | — | ⏳ Pendiente |
| 7 | Conclusiones, implicaciones y agenda futura | Síntesis H1–H7 | — | ⏳ Pendiente |

### Hipótesis H1–H7

| Código | Título | Método principal | Cap. empírico | Estado |
|--------|--------|-----------------|---------------|--------|
| H1 | Efecto causal inmediato del alivio HIPC sobre ratios de deuda | DiD TWFE + CS + DDD | Cap. 4 §4.1–4.4 | ✅ Contrastada |
| H2 | Persistencia y heterogeneidad temporal del efecto | Event-study + Bai-Perron + CS-2021 | Cap. 4 §4.5 | 🔄 Script listo |
| H3 | Brecha condicionalidades formales vs. cumplimiento efectivo | Kaplan-Meier + Cox | Cap. 5 §5.1 | ⏳ Pendiente |
| H4 | Determinantes geopolíticos del cumplimiento | 2SLS con ht_colonial + TradeSaliency | Cap. 5 §5.2 | ⏳ Pendiente |
| H5 | Impacto social mediado por gobernanza | Panel EF + mediación Imai et al. | Cap. 5 §5.3 | ⏳ Pendiente |
| H6 | Primacía institucional: IGC/IGA dominan sobre ln_relief | Análisis dominancia Budescu + 2SLS | Cap. 5 §5.4 | ⏳ Pendiente |
| H7 | Paradoja del reendeudamiento post-HIPC | Cox + DDD + logística multinomial | Cap. 4 §4.6 | 🔄 Script listo |

---

## Resultados empíricos confirmados (H1)

| Estimador | β | SE | p | N | Veredicto |
|-----------|---|----|---|---|-----------|
| DiD TWFE M1 (sin controles) | −40.6 pp. | 11.7 | 0.0006 | 1.287 | ✅ |
| DiD TWFE M2 (con controles) | −39.3 pp. | 11.1 | 0.0004 | 891 | ✅ |
| DDD M2 (+ controles nivel) | −81.1 pp. | 27.8 | 0.0035 | 1.606 | ✅ |
| DDD M3 (+ controles pre-tratam.) | −95.8 pp. | 32.6 | 0.0033 | 1.525 | ✅ |
| Test placebo (comp_year−5) | +8.2 pp. | — | 0.125 | 891 | ✅ sin efecto |
| Sin años de anticipación | −49.4 pp. | — | 0.0007 | 829 | ✅ robusto |

**Nota sobre DDD:** el estimador DiD TWFE (§4.1) usa efectos fijos de país y año; el DDD (§4.4) usa efectos fijos de año solamente con controles pre-tratamiento. Las magnitudes no son directamente comparables pero la dirección y significancia son consistentes. El coeficiente DDD de referencia es M2 (−81.1 pp.).

---

## Archivos del repositorio

### Documentos Word (capítulos y artículos)

```
Cap1_Completo_FINAL.docx          ← Cap. 1 completo con prefacio (versión FINAL)
Cap2_Marco_teorico_FINAL.docx     ← Cap. 2 completo, §2.5 reescrito (Opción C)
Cap3_Metodologia.docx             ← Cap. 3 metodológico completo
Cap4_Sec4.1_H1_especificacion_tecnica.docx  ← §4.1 DiD TWFE M1–M4
Cap4_Sec4.2_H1_dinamica_temporal.docx       ← §4.2 Event-study
Cap4_Sec4.3_4.7_completar.docx              ← §4.3 CS · §4.4 DDD · §4.5 Bai-Perron · §4.6 Cox · §4.7 síntesis
Cap4_Sec4.4_DDD.docx              ← §4.4 DDD redactado con resultados reales
Articulo_CW_Instituciones_HIPC.docx     ← Artículo ETIC (español)
Articulo_CW_Instituciones_HIPC_EN.docx  ← Artículo ETIC (inglés)
Articulo_HIPC_LAC.docx            ← Artículo HIPC en LAC
```

### Scripts R — secuencia de ejecución

#### Fase 0 — Construcción del panel

```
descarga_API.R                 ← descarga WDI, WEO, QoG via API
descarga_dxx.R                 ← descarga deuda externa (WB IDS)
fase0_completar_panel.R        ← integración panel 72 variables
fase0b_0d_ACP_v5.R             ← ACP diagnóstico de variables
construir_IGC_IGA.R            ← construcción índices IGC e IGA (ejecutado ✅)
fiscal_variables.R             ← variables fiscales ICTD
diagnostico_panel.R            ← diagnóstico calidad del panel
seleccion_control_nohipc.R     ← selección 15 países no-HIPC por Mahalanobis (ejecutado ✅)
```

#### Fase 1 — Capítulo 4: Sostenibilidad (H1, H2, H7)

```
fase1_H1_DiD.R          ← §4.1–4.2: DiD TWFE + event-study (EJECUTADO ✅)
fase1b_H1b.R            ← §4.5: Callaway-Sant'Anna ATT(g,t)
control_sintetico.R     ← §4.3: Control sintético (Uganda, Bolivia, Ghana, Zambia)
cap4_H1_DDD_v4.R        ← §4.4: Triple diferencia DDD (EJECUTADO ✅) — versión DEFINITIVA
cap4_H2_BaiPerron.R     ← §4.5: Ruptura estructural Bai-Perron + cohortes (LISTO)
cap4_H7_Cox.R           ← §4.6: Modelo Cox supervivencia + logística DSF (LISTO)
```

> **Nota sobre versiones DDD:** el repositorio contiene `cap4_H1_DDD.R` (v1), `cap4_H1_DDD_v2.R` y `cap4_H1_DDD_v3.R` como versiones intermedias descartadas. La versión definitiva es **`cap4_H1_DDD_v4.R`**. Las versiones anteriores se conservan por trazabilidad metodológica.

#### Fase 2 — Capítulo 5: Condicionalidades y gobernanza (H3–H6)

```
[pendientes — sesión siguiente]
cap5_H3_KaplanMeier.R    ← §5.1: Brecha condicionalidades (Kaplan-Meier + Cox)
cap5_H4_2SLS.R           ← §5.2: Determinantes geopolíticos (2SLS, ht_colonial)
cap5_H5_mediacion.R      ← §5.3: Impacto social (panel + mediación Imai)
cap5_H6_dominancia.R     ← §5.4: Primacía gobernanza (análisis dominancia Budescu)
```

#### Artículo ETIC

```
scripts_articulo_etic.R   ← 955 líneas R completo para el artículo CW-instituciones
```

---

## Panel de datos

| Dimensión | Valor |
|-----------|-------|
| Países tratados | 39 países HIPC |
| Países control | 15 países no-HIPC (Mahalanobis) |
| Período | 1990–2022 |
| Observaciones (panel HIPC) | 1.287 filas × 72 columnas |
| Observaciones (panel combinado DDD) | 1.777 filas × 54 países |
| Archivo principal | `panel_HIPC.RData` (no en repo — >100 MB) |
| Archivo control | `panel_control_nohipc.RData` (no en repo) |

### Variables clave

| Variable | Descripción | Fuente |
|----------|-------------|--------|
| `dxp` | Deuda externa total / PIB (%) | WDI / IDS |
| `post_completion` | Dummy = 1 desde el completion point | Construida |
| `ln_relief` | Log del alivio HIPC en VAN (USD) | HIPC CBP Database |
| `IGC` | Índice Gobernanza Anticorrupción (ACP) | V-Dem / ICRG / QoG |
| `IGA` | Índice Gobernanza Accountability (ACP) | V-Dem / FH / Polity |
| `wgi_comp` | WGI compuesto (promedio 6 dimensiones) | Kaufmann et al. (2010) |
| `ajr_settmort` | Mortalidad histórica colonizadores (instrumento IV H6) | AJR (2001) / QoG |
| `ht_colonial` | Vínculo colonial histórico (instrumento IV H4) | QoG Institute |
| `ross_oil_prod` | Producción de petróleo per cápita (log) | QoG Institute |
| `ln_china` | Log deuda bilateral con China | AidData (2023) |
| `eurobond_dummy` | Emisión de eurobonos soberanos | Construida (Bloomberg) |
| `dsf_high` | Clasificación riesgo DSF elevado (proxy) | FMI DSA / construida |

### Índices propios (IGC e IGA)

Construidos mediante Análisis de Componentes Principales en `construir_IGC_IGA.R`:

**IGC** (Anticorrupción): ACP sobre `vdem_corr`, `vdem_execorr`, `vdem_pubcorr` + `icrg_qog`  
— PC1 explica 68% de la varianza · Cobertura pre-1996: 74.4%

**IGA** (Accountability): ACP sobre `vdem_libdem`, `vdem_polyarchy`, `fh_cl`, `fh_pr`, `p_polity2`  
— PC1 explica 82% de la varianza · Cobertura pre-1996: 97.4%  
— Correlación IGC–IGA = 0.434 (dimensiones suficientemente distintas)

---

## Pendientes técnicos

| Item | Descripción | Prioridad |
|------|-------------|-----------|
| TradeSaliency | Descarga UN Comtrade para instrumento H4 | Alta |
| dsf_high | Datos DSF oficiales FMI (actualmente proxy cuantitativa) | Alta |
| cap4_H2_BaiPerron.R | Ejecutar y pegar resultados | Alta |
| cap4_H7_Cox.R | Ejecutar y pegar resultados | Alta |
| Cap. 4 unificado | Ensamblar §4.1–4.7 en un solo documento | Media |
| Cap. 5 | Redactar H3–H6 tras ejecutar scripts | Media |
| Fig. 4.2.1 | Renombrar "Figura 3.6" en Cap4_Sec4.2 | Baja |

---

## Artículos derivados

| Título | Revista objetivo | Estado |
|--------|-----------------|--------|
| *Privatize to adjust, weaken to govern* | Economics of Transition and Institutional Change (ETIC) | ✅ Versiones ES + EN listas |
| *Deuda china y soberanía fiscal en países post-HIPC* | (por definir) | ⏳ Borrador pendiente |

---

## Cómo replicar

```r
# 1. Construir el panel
source("fase0_completar_panel.R")
source("construir_IGC_IGA.R")
source("seleccion_control_nohipc.R")

# 2. Cap. 4 — H1
source("fase1_H1_DiD.R")        # DiD TWFE + event-study
source("control_sintetico.R")   # Control sintético
source("cap4_H1_DDD_v4.R")      # Triple diferencia (versión definitiva)

# 3. Cap. 4 — H2 y H7
source("cap4_H2_BaiPerron.R")   # Bai-Perron + Callaway-Sant'Anna
source("cap4_H7_Cox.R")         # Cox + logística DSF

# Paquetes requeridos
install.packages(c(
  "tidyverse", "plm", "lmtest", "sandwich", "did",
  "strucchange", "survival", "survminer", "nnet",
  "Synth", "openxlsx", "WDI"
))
```

---

## Registro de sesiones

| Fecha | Avance principal |
|-------|-----------------|
| Mar 2026 (sesiones 1–4) | Panel de datos, hipótesis H1–H7, DiD TWFE, IGC/IGA |
| 27–28 mar 2026 | Cap. 1 completo, Cap. 2 ampliado, Cap. 3 metodológico |
| 28 mar 2026 | Artículos ETIC (ES + EN), renumeración 7 capítulos |
| 29 mar 2026 | Cap. 2 reescrito (§2.5 Opción C), §4.4 DDD v4 con resultados reales |
| 30 mar 2026 | README actualizado, scripts H2 y H7 listos para ejecutar |
