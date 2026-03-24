
#  LIBRERÍAS -----------------------------------------------
pacman::p_load(tidyverse, MASS,car,mgcv,caret,pROC,ResourceSelection,jtools,            
               forestmodel,DataExplorer,plotly,DT,DescTools)

# CARGA Y CALIDAD DE DATOS --------------------------------
data <- read_csv("C:/Users/user/3D Objects/MODELOS_POS/Modelo_Logistico_Logic/Deserción_estu/Datos/dataset.csv", show_col_types = FALSE)
glimpse(data)

# Valores faltantes
plot_missing(data) +
  labs(title = "Porcentaje de valores faltantes por variable") +
  theme_minimal()

# Duplicados
sum(duplicated(data))

# RENOMBRADO DE VARIABLES ---------------------------------
datos <- data %>%
  rename(
    Estado_civil                                     = `Marital status`,
    Modalidad_de_solicitud                           = `Application mode`,
    Orden_de_solicitud                               = `Application order`,
    Curso                                            = Course,
    Asistencia_diurna_nocturna                       = `Daytime/evening attendance`,
    Titulo_previo                                    = `Previous qualification`,
    Nacionalidad                                     = Nacionality,
    Titulo_de_la_madre                               = `Mother's qualification`,
    Titulo_del_padre                                 = `Father's qualification`,
    Ocupacion_de_la_madre                            = `Mother's occupation`,
    Ocupacion_del_padre                              = `Father's occupation`,
    Desplazado                                       = Displaced,
    Necesidades_educativas_especiales                = `Educational special needs`,
    Deudor                                           = Debtor,
    Cuotas_de_matricula_al_dia                       = `Tuition fees up to date`,
    Género                                           = Gender,
    Becario                                          = `Scholarship holder`,
    Edad_en_el_momento_de_la_inscripcion             = `Age at enrollment`,
    Internacional                                    = International,
    Unidades_curriculares_1er_semestre_acreditadas   = `Curricular units 1st sem (credited)`,
    Unidades_curriculares_1er_semestre_matriculadas  = `Curricular units 1st sem (enrolled)`,
    Unidades_curriculares_1er_semestre_evaluaciones  = `Curricular units 1st sem (evaluations)`,
    Unidades_curriculares_1er_semestre_aprobadas     = `Curricular units 1st sem (approved)`,
    Unidades_curriculares_1er_semestre_calificacion  = `Curricular units 1st sem (grade)`,
    Unidades_curriculares_1er_semestre_sin_evaluaciones = `Curricular units 1st sem (without evaluations)`,
    Unidades_curriculares_2do_semestre_acreditadas   = `Curricular units 2nd sem (credited)`,
    Unidades_curriculares_2do_semestre_matriculadas  = `Curricular units 2nd sem (enrolled)`,
    Unidades_curriculares_2do_semestre_evaluaciones  = `Curricular units 2nd sem (evaluations)`,
    Unidades_curriculares_2do_semestre_aprobadas     = `Curricular units 2nd sem (approved)`,
    Unidades_curriculares_2do_semestre_calificacion  = `Curricular units 2nd sem (grade)`,
    Unidades_curriculares_2do_semestre_sin_evaluaciones = `Curricular units 2nd sem (without evaluations)`,
    Tasa_de_desempleo                                = `Unemployment rate`,
    Tasa_de_inflacion                                = `Inflation rate`,
    PIB                                              = GDP,
    Objetivo                                         = Target
  )

# EDA - ANÁLISIS EXPLORATORIO -----------------------------

# Variable respuesta
tabla <- table(datos$Objetivo)
cat   <- prop.table(tabla)

plot_ly(labels = names(cat),
        values = as.vector(tabla),
        type   = "pie",
        text   = paste0(names(cat), "<br>", as.vector(tabla),
                        " (", round(as.vector(cat) * 100, 1), "%)"),
        textinfo  = "text",
        hoverinfo = "label+percent+value")

# Distribución por curso (antes de agrupar)
tabla1 <- table(datos$Curso)
cat1   <- prop.table(tabla1)
plot_ly(labels = names(cat1), values = as.vector(tabla1),
        type = "pie", hole = 0,
        text = paste0(names(cat1), "<br>", as.vector(tabla1),
                      " (", round(as.vector(cat1) * 100, 1), "%)"),
        textinfo = "text", hoverinfo = "label+percent+value")

# Distribución por título previo (antes de agrupar)
tabla2 <- table(datos$Titulo_previo)
cat2   <- prop.table(tabla2)
plot_ly(labels = names(cat2), values = as.vector(cat2), type = "pie", hole = 0)

# TRANSFORMACIÓN DE VARIABLES -----------------------------

# Conversión a factor
datos <- datos %>%
  mutate(across(c(Género, Becario, Cuotas_de_matricula_al_dia, Deudor,
                  Titulo_previo, Asistencia_diurna_nocturna,
                  Estado_civil, Desplazado), as.factor))

# Agrupación de cursos en áreas de conocimiento
datos <- datos %>%
  mutate(Area_conocimiento = case_when(
    Curso %in% c(6, 12, 13) ~ "Salud",
    Curso %in% c(1, 7)      ~ "Tecnologia",
    Curso %in% c(4, 8)      ~ "Agro_Equino",
    Curso %in% c(2, 5, 15)  ~ "Diseno_Comuni",
    Curso %in% c(9, 14, 17) ~ "Gestion",
    Curso %in% c(3, 10, 11) ~ "Servicios_Social",
    Curso %in% c(16)        ~ "Educacion",
    TRUE                    ~ "Otro"
  ))

# Agrupación de título previo en niveles educativos
datos <- datos %>%
  mutate(Nivel_educativo_previo = case_when(
    Titulo_previo == 1                       ~ "Secundaria_completa",
    Titulo_previo %in% c(7, 8, 9, 10, 11)   ~ "Secundaria_incompleta",
    Titulo_previo %in% c(12, 13)             ~ "Basica",
    Titulo_previo %in% c(14, 16)             ~ "Tecnico_profesional",
    Titulo_previo %in% c(2, 3, 15)           ~ "Superior_grado",
    Titulo_previo %in% c(4, 5, 17)           ~ "Superior_posgrado",
    Titulo_previo == 6                        ~ "Superior_incompleta",
    TRUE                                      ~ "Otro"
  ))

# Distribución de variables agrupadas
tabla3 <- table(datos$Area_conocimiento)
cat3   <- prop.table(tabla3)
plot_ly(labels = names(cat3), values = as.vector(tabla3), type = "pie", hole = 0,
        text = paste0(names(cat3), "<br>", as.vector(tabla3),
                      " (", round(as.vector(cat3) * 100, 1), "%)"),
        textinfo = "text", hoverinfo = "label+percent+value")

tabla4 <- table(datos$Nivel_educativo_previo)
cat4   <- prop.table(tabla4)
plot_ly(labels = names(cat4), values = as.vector(tabla4), type = "pie", hole = 0,
        text = paste0(names(cat4), "<br>", as.vector(tabla4),
                      " (", round(as.vector(cat4) * 100, 1), "%)"),
        textinfo = "text", hoverinfo = "label+percent+value")

# SELECCIÓN DE VARIABLES Y PREPARACIÓN --------------------
df <- datos %>%
  select(Objetivo, Estado_civil, Area_conocimiento,
         Tasa_de_desempleo, Tasa_de_inflacion, PIB,
         Género, Becario, Edad_en_el_momento_de_la_inscripcion,
         Cuotas_de_matricula_al_dia, Deudor, Nivel_educativo_previo,
         Asistencia_diurna_nocturna, Desplazado,
         Unidades_curriculares_1er_semestre_acreditadas,
         Unidades_curriculares_1er_semestre_matriculadas,
         Unidades_curriculares_1er_semestre_evaluaciones,
         Unidades_curriculares_1er_semestre_aprobadas,
         Unidades_curriculares_1er_semestre_calificacion,
         Unidades_curriculares_2do_semestre_acreditadas,
         Unidades_curriculares_2do_semestre_matriculadas,
         Unidades_curriculares_2do_semestre_evaluaciones,
         Unidades_curriculares_2do_semestre_aprobadas,
         Unidades_curriculares_2do_semestre_calificacion)

# Variable respuesta binaria
df$Objetivo <- ifelse(df$Objetivo == "Dropout", 1, 0)

# SPLIT TRAIN / TEST (80/20) ------------------------------
set.seed(123)
training_idx_edu <- df$Objetivo %>% createDataPartition(p = 0.8, list = FALSE)
train.edu <- df[ training_idx_edu, ]
test.edu  <- df[-training_idx_edu, ]

# MODELAMIENTO --------------------------------------------

# Modelo completo
M <- glm(Objetivo ~ ., data = train.edu, family = "binomial")

# Selección de variables: Stepwise AIC (bidireccional)
mod <- stepAIC(M, direction = "both", trace = FALSE)
summary(mod)
vif(mod)

# Visualización de coeficientes (OR con IC)
plot_summs(mod, confint = T, exp = T) + scale_x_log10()
forest_model(mod, exponentiate = T)

# SUPUESTOS -----------------------------------------------

# 8.1 Linealidad con el logit (GAM)
# Box-Tidwell no aplica: variables continuas con ceros
# Se usa GAM como alternativa visual
gam_model <- gam(Objetivo ~
                   s(Unidades_curriculares_1er_semestre_aprobadas) +
                   s(Unidades_curriculares_1er_semestre_calificacion) +
                   s(Unidades_curriculares_2do_semestre_acreditadas) +
                   s(Unidades_curriculares_2do_semestre_matriculadas) +
                   s(Unidades_curriculares_2do_semestre_evaluaciones) +
                   s(Unidades_curriculares_2do_semestre_aprobadas) +
                   s(Unidades_curriculares_2do_semestre_calificacion) +
                   s(Edad_en_el_momento_de_la_inscripcion) +
                   s(Tasa_de_desempleo) +
                   Area_conocimiento + Género + Becario +
                   Cuotas_de_matricula_al_dia + Deudor +
                   Nivel_educativo_previo + Desplazado,
                 family = binomial,
                 data = train.edu)
plot(gam_model, pages = 1)

## Eliminación de variable no significativa
# Unidades_curriculares_2do_semestre_evaluaciones: p = 0.1162 → no aporta
anova(mod,
      update(mod, ~. - Unidades_curriculares_2do_semestre_evaluaciones),
      test = "Chisq")

mod_new <- update(mod, ~. - Unidades_curriculares_2do_semestre_evaluaciones)
vif(mod_new)
AIC(mod, mod_new)
BIC(mod, mod_new)
# BIC favorece mod_new (penaliza más la complejidad con n grande)

## Observaciones influyentes (Distancias de Cook)
plot(mod_new, which = 4)
influencePlot(mod_new)

# Inspección de las 3 más influyentes
influen <- train.edu[c(875, 1738, 3406), ] %>%
  select(Objetivo, Edad_en_el_momento_de_la_inscripcion,
         Unidades_curriculares_1er_semestre_aprobadas,
         Unidades_curriculares_1er_semestre_calificacion,
         Unidades_curriculares_2do_semestre_aprobadas,
         Unidades_curriculares_2do_semestre_matriculadas,
         Cuotas_de_matricula_al_dia, Deudor, Becario,
         Desplazado, Area_conocimiento, Nivel_educativo_previo)

# Verificación obs 3406: 18 años con título de maestría → error de registro
datos[training_idx_edu[3406],
      c("Edad_en_el_momento_de_la_inscripcion",
        "Titulo_previo", "Nivel_educativo_previo")]

# Obs 875 y 1738: revisadas, sin incoherencia objetiva → se conservan

# Eliminación del error de registro y reajuste
train.edu <- train.edu[-3406, ]
mod_new   <- glm(formula(mod_new), data = train.edu, family = "binomial")

# Verificar estabilidad de coeficientes tras eliminación
summ(mod_new, exp = T, confint = T)
# BONDAD DE AJUSTE ----------------------------------------
# Hosmer-Lemeshow: H0 = el modelo se ajusta a los datos
hl <- hoslem.test(train.edu$Objetivo, mod_new$fitted.values, g = 10);hl
cbind(hl$observed, hl$expected)

# p = 0.063 → no se rechaza H0 (ajuste aceptable, límite)

# MÉTRICAS DE EVALUACIÓN  -------------------------------------------------

PseudoR2(mod_new, which = c("CoxSnell", "Nagelkerke", "McFadden"))
# EVALUACIÓN DEL MODELO -------------------------------------
pred_test <- predict(mod_new, newdata = test.edu, type = "response")
roc_test  <- roc(test.edu$Objetivo, pred_test)
auc(roc_test)

## Matriz de confusión — umbral 0.5
pred_clase <- ifelse(pred_test > 0.5, 1, 0)
cm_05 <- confusionMatrix(as.factor(pred_clase),
                         as.factor(test.edu$Objetivo),
                         positive = "1")
cm_05

## Umbral óptimo por criterio de Youden
umbral_youden    <- coords(roc_test, "best", best.method = "youden")$threshold
pred_clase_youden <- ifelse(pred_test > umbral_youden, 1, 0)
cm_youden <- confusionMatrix(as.factor(pred_clase_youden),
                             as.factor(test.edu$Objetivo),
                             positive = "1")
cm_youden

## Comparación de umbrales
comparacion <- data.frame(
  Metrica       = c("Accuracy", "Sensibilidad", "Especificidad", "Kappa"),
  Umbral_0.5    = round(c(cm_05$overall["Accuracy"],
                          cm_05$byClass["Sensitivity"],
                          cm_05$byClass["Specificity"],
                          cm_05$overall["Kappa"]), 4),
  Umbral_Youden = round(c(cm_youden$overall["Accuracy"],
                          cm_youden$byClass["Sensitivity"],
                          cm_youden$byClass["Specificity"],
                          cm_youden$overall["Kappa"]), 4)
)

DT::datatable(comparacion,
              options  = list(dom = "t", pageLength = 4),
              rownames = FALSE) %>%
  DT::formatRound(columns = c("Umbral_0.5", "Umbral_Youden"), digits = 4)

# VISUALIZACIONES FINALES --------------------------------

# Curva ROC con punto de corte óptimo
plot(roc_test,
     col = "steelblue", lwd = 2,
     main = "Curva ROC - Modelo de Deserción Estudiantil",
     print.auc = TRUE, print.auc.y = 0.4)

punto <- coords(roc_test, "best", best.method = "youden")
points(punto$specificity, punto$sensitivity,
       pch = 19, col = "firebrick", cex = 1.5)
text(punto$specificity - 0.05, punto$sensitivity - 0.04,
     paste0("Umbral = ", round(punto$threshold, 3)),
     col = "firebrick", cex = 0.85)

# Distribución de probabilidades predichas por clase real
data.frame(prob = pred_test,
           real = as.factor(test.edu$Objetivo)) %>%
  ggplot(aes(x = prob, fill = real)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = umbral_youden,
             linetype = "dashed", color = "firebrick") +
  scale_fill_manual(values = c("steelblue", "coral"),
                    labels = c("No desertor", "Desertor")) +
  labs(title = "Distribución de probabilidades predichas",
       x = "Probabilidad predicha", y = "Densidad",
       fill = "Clase real") +
  theme_minimal()
