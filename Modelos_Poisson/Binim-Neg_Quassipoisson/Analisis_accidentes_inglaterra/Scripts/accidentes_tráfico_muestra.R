# Librerías ---------------------------------------------------------------

pacman::p_load(BiocManager, lmtest, car, MASS, NHSRdatasets, ResourceSelection,
               pROC, rcompanion, nortest, tidyverse, caret, mgcv, rms,
               jtools, forestmodel,gvlma, trafo, readxl, tidyverse, naniar,
               DataExplorer, FactoMineR, factoextra, corrplot,sjPlot,ggplot2,
               MVN, DT, tidyplots, performance,see,SmartEDA,doBy, fitdistrplus,
               forcats)

#pak::pak("jbengler/tidyplots")

# Carga de datos ----------------------------------------------------------
data <- read_csv(("UK_Accident.csv"), show_col_types = F)
glimpse(data)

plot_missing(data) +
  labs(title = "Porcentaje de valores faltantes por variable") +
  theme_minimal()

# 1. ELIMINAR NAs Y LIMPIAR FACTORES
data <- data %>%
  na.omit() %>%  # Eliminar filas con NA
  mutate(across(where(is.factor), droplevels))  # Eliminar niveles no usados

## Realizamos selección de  una muestra de 100000 registros
set.seed(123)
sample_size <- 100000
datos <- data %>% sample_n(sample_size)

## Renombrado variables a español
datos <- datos %>% 
  rename(
    indice_accidente = Accident_Index,
    coordenada_este_osgr = Location_Easting_OSGR,
    coordenada_norte_osgr = Location_Northing_OSGR,
    longitud = Longitude,
    latitud = Latitude,
    fuerza_policial = Police_Force,
    severidad_accidente = Accident_Severity,
    numero_vehiculos = Number_of_Vehicles,
    numero_victimas = Number_of_Casualties,
    fecha = Date,
    dia_semana = Day_of_Week,
    hora = Time,
    autoridad_local_distrito = `Local_Authority_(District)`,
    autoridad_local_vial = `Local_Authority_(Highway)`,
    clase_via_principal = `1st_Road_Class`,
    numero_via_principal = `1st_Road_Number`,
    tipo_via = Road_Type,
    limite_velocidad = Speed_limit,
    control_interseccion = Junction_Control,
    clase_via_secundaria = `2nd_Road_Class`,
    numero_via_secundaria = `2nd_Road_Number`,
    cruce_peatonal_control_humano = `Pedestrian_Crossing-Human_Control`,
    cruce_peatonal_instalaciones_fisicas = `Pedestrian_Crossing-Physical_Facilities`,
    condiciones_luminosas = Light_Conditions,
    condiciones_climaticas = Weather_Conditions,
    condiciones_superficie_via = Road_Surface_Conditions,
    condiciones_especiales_sitio = Special_Conditions_at_Site,
    peligros_calzada = Carriageway_Hazards,
    zona_urbana_rural = Urban_or_Rural_Area,
    policia_asistio_accidente = Did_Police_Officer_Attend_Scene_of_Accident,
    lsoa_ubicacion_accidente = LSOA_of_Accident_Location,
    año = Year
  )

datos$zona_urbana_rural <- factor(datos$zona_urbana_rural, 
                                  levels = c(1,2,3),
                                  labels = c("Urbana","Rural","Desconocida"))
datos$severidad_accidente <- factor(datos$severidad_accidente,
                                    levels = c(1,2,3),
                                    labels = c("Grave", "Moderado", "Leve"))

df <- datos %>% 
  select(numero_victimas,
    numero_vehiculos,
         limite_velocidad,
         dia_semana,
         año,
         fecha,
         tipo_via,
         control_interseccion,
         condiciones_luminosas,
         condiciones_climaticas,
         condiciones_superficie_via,
         zona_urbana_rural,
         policia_asistio_accidente,
         severidad_accidente)

df<- df %>% mutate(across(where(is.character), as.factor))
#df <- df %>% mutate(severidad_numerica = as.numeric(severidad_accidente))

## Renombrando niveles 
df <- df %>%
  mutate(
    # Tipo de vía
    tipo_via = fct_recode(tipo_via,
                          "Doble calzada" = "Dual carriageway",
                          "Calle de una vía" = "One way street",
                          "Glorieta" = "Roundabout",
                          "Calzada única" = "Single carriageway",
                          "Ramal de acceso" = "Slip road",
                          "Desconocido" = "Unknown"
    ),
    
    # Control de intersección
    control_interseccion = fct_recode(control_interseccion,
                                      "Persona autorizada" = "Authorised person",
                                      "Semaforo automatico" = "Automatic traffic signal",
                                      "Ceda el paso o sin control" = "Giveway or uncontrolled",
                                      "Ninguno" = "None",
                                      "Señal de Pare" = "Stop Sign"
    ),
    
    # Condiciones luminosas
    condiciones_luminosas = fct_recode(condiciones_luminosas,
                                       "Oscuridad: Sin iluminación" = "Darkeness: No street lighting",
                                       "Oscuridad: Iluminación desconocida" = "Darkness: Street lighting unknown",
                                       "Oscuridad: Luces encendidas" = "Darkness: Street lights present and lit",
                                       "Oscuridad: Luces apagadas" = "Darkness: Street lights present but unlit",
                                       "Luz de día" = "Daylight: Street light present"
    ),
    
    # Condiciones climíticas
    condiciones_climaticas = fct_recode(condiciones_climaticas,
                                        "Buen tiempo con vientos fuertes" = "Fine with high winds",
                                        "Buen tiempo sin vientos fuertes" = "Fine without high winds",
                                        "Niebla o bruma" = "Fog or mist",
                                        "Otro" = "Other",
                                        "Lluvia con vientos fuertes" = "Raining with high winds",
                                        "Lluvia sin vientos fuertes" = "Raining without high winds",
                                        "Nieve con vientos fuertes" = "Snowing with high winds",
                                        "Nieve sin vientos fuertes" = "Snowing without high winds",
                                        "Desconocido" = "Unknown"
    ),
    
    # Condiciones de la superficie
    condiciones_superficie_via = fct_recode(condiciones_superficie_via,
                                            "Seca" = "Dry",
                                            "Inundada (>3cm agua)" = "Flood (Over 3cm of water)",
                                            "Helada/Hielo" = "Frost/Ice",
                                            "Normal" = "Normal",
                                            "Nieve" = "Snow",
                                            "Mojada/Hímeda" = "Wet/Damp"
    ),
    
    # Policía asistio (binaria/factor)
    policia_asistio_accidente = fct_recode(policia_asistio_accidente,
                                           "No" = "No",
                                           "Sí" = "Yes"
    )
  )

##################### ANÁLISIS EXPLORATORIO #####################
### Análisis de NA
plot_missing(df) +
labs(title = "Porcentaje de valores faltantes por variable") +
  theme_minimal()

### coeficiente de variación
coef_variacion <- function(x, na.rm = TRUE) {
  100 * sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

attach(df)
## Medidas descriptivas
D1 <- summaryBy(numero_victimas ~ tipo_via, data = df,
                FUN = c(mean, sd, median, min, max, IQR, length, coef_variacion)) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))
        
datatable(D1)

D2 <- summaryBy(numero_victimas~zona_urbana_rural, data = df,
                FUN = c(mean, sd, median, min, max, IQR, length, coef_variacion)) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))
         
datatable(D2)

D3 <- summaryBy(numero_victimas~severidad_accidente, data = df,
                FUN = c(mean, sd, median, min, max, IQR, length, coef_variacion)) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))
datatable(D3)

D4 <- summaryBy(numero_victimas~condiciones_superficie_via, data = df,
                FUN = c(mean, sd, median, min, max, IQR, length, coef_variacion)) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))
datatable(D4)

D5 <- summaryBy(numero_victimas~condiciones_climaticas, data = df,
                FUN = c(mean, sd, median, min, max, IQR, length, coef_variacion)) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))
datatable(D5)

D6 <- summaryBy(numero_victimas~limite_velocidad, data = df,
                FUN = c(mean, sd, median, min, max, IQR, length, coef_variacion)) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))
datatable(D6)

D7 <- summaryBy(numero_victimas~numero_vehiculos, data = df,
                FUN = c(mean, sd, median, min, max, IQR, length, coef_variacion)) %>% 
  mutate(across(where(is.numeric), ~round(., 3)))
datatable(D7)

## histograma
ggplot(df) + 
  geom_histogram(aes(x = numero_victimas), color = "blue")+
  theme_minimal()

### Gráficos de boxplot

Box1 <- ggplot(df) + 
  geom_boxplot(aes(y = numero_victimas)) +
  theme_minimal()

Box2 <- ggplot(df) + 
  geom_boxplot(aes(y = numero_vehiculos), fill = "steelblue") +
  theme_minimal()

Box3 <- ggplot(df) + 
  geom_boxplot(aes(y = limite_velocidad, x = severidad_accidente), fill = "orange") +
  theme_minimal()

### Gráfico de barras
bar1 <- ggplot(df) + 
  geom_bar(aes(x = tipo_via, fill = factor(tipo_via))) +
  coord_cartesian(ylim = c(0, 100000)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

bar2 <- ggplot(df) + 
  geom_bar(aes(x = condiciones_superficie_via, fill = factor(condiciones_superficie_via))) +
  coord_cartesian(ylim = c(0, 100000)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

bar3 <- ggplot(df) + 
  geom_bar(
    aes(x = condiciones_luminosas,
        fill = factor(condiciones_luminosas))
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

bar4 <- ggplot(df) + 
  geom_bar(
    aes(x = condiciones_climaticas,
        fill = factor(condiciones_climaticas))
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

### Gráfico de boxplot (identificación de outliers)
BOX1 <- ggplot(df, aes(x = tipo_via, y = numero_victimas, fill = tipo_via)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Relación tipo de la Vía ",
       x = "tipo Vía",
       y = "Número de Víctimas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")


BOX2 <- ggplot(df, aes(x = condiciones_superficie_via, y = numero_victimas, fill = condiciones_superficie_via)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Relación Superficie de la Vía",
       x = "Condiciones de la Superficie",
       y = "Número de Víctimas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

BOX3 <- ggplot(df, aes(x = condiciones_luminosas, y = numero_victimas, fill = condiciones_luminosas)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Relación condiciones_luminosas ",
       x = "condiciones_luminosas",
       y = "Número de Víctimas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

BOX4 <- ggplot(df, aes(x = condiciones_climaticas, y = numero_victimas, fill = condiciones_climaticas)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Relación condiciones_climaticas ",
       x = "condiciones_climaticas",
       y = "Número de Víctimas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")


# 1. Ajuste de modelos (asegúrate de que existan)
fit.poisson = fitdist(df$numero_victimas, "pois")
fit.negbin  = fitdist(df$numero_victimas, "nbinom")

# 2. Configuración del panel
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# CDF Comp corregido
# Usamos 'fitcol' para los modelos y 'datacol' para los datos observados
cdfcomp(list(fit.poisson, fit.negbin), 
        legendtext = c("Poisson", "NegBin"), 
        main = "Comparación de CDF",
        fitcol = c("steelblue", "darkorange"), # Argumento específico para líneas
        datacol = "grey70")                  # Argumento específico para puntos

# QQ Comp corregido
# En qqcomp también se usa 'fitcol' para los símbolos de los modelos
qqcomp(list(fit.poisson, fit.negbin), 
       legendtext = c("Poisson", "NegBin"),
       main = "Gráfico Q-Q",
       fitcol = c("steelblue", "darkorange"))

par(mfrow = c(1, 1))

### tablas cruzadas
#Victimas vs tipo de vías
tabla1 <- table(datos$numero_victimas, datos$tipo_via)
print(tabla1)

# Víctimas vs clima
tabla2 <- table(datos$numero_victimas, datos$condiciones_climaticas)
print(tabla2)

# Víctimas vs iluminación
tabla3 <- table(datos$numero_victimas, datos$condiciones_luminosas)
print(tabla3)

#################### ANÁLISIS DE MULTINORMALIDAD ####################
set.seed(123)
df_numeric <- df %>% 
  select(numero_victimas, numero_vehiculos, limite_velocidad) %>% sample_n(10000)

# El numero de registros es una cantidad que sobrepasa los limites de 
# procesamiento de la función MVN, por ende, se realiza la toma al azar de al menos 
# 10.000 registros para validar multinormalidad usando tambien una semilla de 
# de reproducibilidad.

mvn(df_numeric, mvn_test = "mardia", univariate_test = "AD")
MVN::univariate_diagnostic_plot(df_numeric,type = c("boxplot"))

plot_histogram(df_numeric,
               geom_histogram_args = list(fill = "darkblue", col = "black") ,
               ggtheme = theme_minimal() +
               theme(
                 panel.grid.major = element_line(color = "black"),
                 panel.grid.minor = element_line(color = "black"),
                 strip.background = element_rect(fill = "orange",
                                                 color = "black"))
  ) 

## EL resultado de rechazo de multinormalidad es esperado, ya que, las variables
## de conteo no siguen una distribución normal.

#library("writexl")
#write_xlsx(df, "muestra.xlsx")
######################## MODELAMIENTO ###############################

# Conversión de variables a factor ----------------------------------------
df <- df %>% mutate(across(where(is.character), as.factor))
df_modelo <- df[, -c(4, 5, 12)]

# Aplicación de stepwise (Selección de variables) --------------------------------------------------
General <- glm(numero_victimas ~ ., data = df_modelo, family = "poisson")
mod <- stepAIC(General, direction = "both", trace = F )


summary(mod)
summ(mod, confint = T, exp = T)


mod1 <- glm(numero_victimas ~ numero_vehiculos + limite_velocidad +
  condiciones_climaticas + condiciones_luminosas + zona_urbana_rural + severidad_accidente +
    limite_velocidad*tipo_via,
  family = poisson, data = df_modelo)

summary(mod1)
summ(mod1, confint = T, exp = T)

BIC(mod)
BIC(mod1)

anova(mod, mod1)      # Simple vs Complejo

### se trabajara con el modelo escogido por la función StepAIC "mod" ya que aunque el 
### BIC es mejor en "mod1", la prueba de anova indica que no es signoficante con respecto
### al modelo escogido por stepAIC().

## Inferencia sobre los coeficientes
coeftest(mod) # errores standar tipo sándwich

## Prueba de sobre dispersión
deviance(mod)/summary(mod)$df[2] 

## devianza = 0.3007461 , como el supuesto se inflige cuando es > 1, en este caso,
## el modelo podrá ser el ideal "Poisson" pero si realizáramos un mejor manejo de
## subdisperción emplearemos un modelo quassipoisson para ver comparación 

# Modelo QuassiPoisson ----------------------------------------------------
Formula <- formula(mod)

mod_Quass <- glm(Formula, data = df_modelo, family = quasipoisson)
summary(mod_Quass)

performance::check_overdispersion(mod)

## Prueba de sobre dispersión
deviance(mod_Quass)/summary(mod_Quass)$df[2] #devianza = 0.3007461, el modelo ya
# considera la probabilidad de sobre dispersión

summ(mod_Quass, confint = T, exp = T)


# Gráficos ----------------------------------------------------------------

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(mod_Quass,
     which = 1:4,
     col = "steelblue",
     pch = 20,
     cex = 0.5)
par(mfrow = c(1, 1))

# gráficos 2 --------------------------------------------------------------

# 1. Generar las predicciones del modelo
df$predicciones <- predict(mod_Quass, type = "response")

# 2. Preparar datos para comparar frecuencias
comparativa <- df_modelo %>%
  group_by(numero_victimas) %>%
  summarise(Real = n()) %>%
  left_join(
    df %>%
      group_by(numero_victimas) %>%
      summarise(Predicho = sum(predicciones) / mean(predicciones) * (n()/nrow(df_modelo))), # Ajuste de escala
    by = "numero_victimas"
  ) %>%
  filter(numero_victimas <= 5) # Filtramos hasta 5 para que sea legible

# 3. Gráfico de barras comparativo
graf_compara <- ggplot(comparativa) +
 geom_bar(aes(x = factor(numero_victimas), y = Real), stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_point(aes(x = factor(numero_victimas), y = Predicho), color = "red", size = 4) +
  geom_line(aes(x = factor(numero_victimas), y = Predicho, group = 1), color = "red", linetype = "dashed") +
  labs(title = "Validación del Modelo Quasipoisson",
       subtitle = "Barras azules: Datos Reales | Puntos rojos: Predicción del Modelo",
       x = "Número de Víctimas",
       y = "Frecuencia (Cantidad de Accidentes)") +
  theme_minimal()

# modelo con offset -------------------------------------------------------

off_mod1 <- glm( numero_victimas ~ offset(log(numero_vehiculos)) + limite_velocidad + tipo_via + 
                  control_interseccion + condiciones_luminosas + condiciones_superficie_via + 
                  zona_urbana_rural + severidad_accidente,
                 family = "quasipoisson", data = df_modelo)

summary(off_mod1)

summ(off_mod1, exp = T, confint = T)

## Inferencia sobre los coeficientes
coeftest(off_mod1) # errores standar tipo sándwich

## Prueba de sobre dispersión
deviance(off_mod1)/summary(off_mod1)$df[2] # devianza = 0.3818196 el modelo quassipoisson
# Podría ser el ideal

## aunque el BIC del modelo "mod" es mejor que el del offset(), cabe destacar que,
## este nuevo modelo tambien describe como el numero de vehículos a través del
## tiempo es un predictor muy eficaz, por ende, seria bueno tambien incluir en el
## reporte ambos modelos.

# Exportación objetos -----------------------------------------------------
# 1. Definir ruta
ruta_out <- "ú/Users/user/3D Objects/MODELOS_POS/Modelos_Poisson/Bases/ACCIDENTES/Out/"
if (!dir.exists(ruta_out)) dir.create(ruta_out, recursive = TRUE)

# 2. Guardar Tablas Descriptivas (D1 a D7)
tablas_descriptivas <- list(via=D1, zona=D2, sev=D3, sup=D4, clim=D5, vel=D6, veh=D7)
saveRDS(tablas_descriptivas, paste0(ruta_out, "tablas_descriptivas_completas.rds"))

# 3. Guardar Gráficos de Barras (bar1 a bar4)
graficos_barras <- list(via=bar1, superficie=bar2, luz=bar3, clima=bar4)
saveRDS(graficos_barras, paste0(ruta_out, "graficos_barras.rds"))

# 4. Guardar Gráficos Boxplot (Box1 a Box3 y BOX1 a BOX4)
graficos_boxplot <- list(
  simples = list(B1=Box1, B2=Box2, B3=Box3),
  outliers = list(via=BOX1, superficie=BOX2, luz=BOX3, clima=BOX4)
)
saveRDS(graficos_boxplot, paste0(ruta_out, "graficos_boxplot_todos.rds"))

# 5. Guardar Modelos y otros objetos clave
saveRDS(mod, paste0(ruta_out, "modelo_final_stepAIC.rds"))
saveRDS(off_mod1, paste0(ruta_out, "modelo_quasipoisson_offset.rds"))
saveRDS(mod_Quass, paste0(ruta_out, "modelo_quasipoisson.rds"))
saveRDS(graf_compara, paste0(ruta_out, "grafico_comparación.rds"))
saveRDS(Formula, paste0(ruta_out, "Formula.rds"))
saveRDS(df_modelo, paste0(ruta_out, "df_modelo.rds"))

message("Verificación: Se han guardado 7 tablas descriptivas, 4 gráficos de barras y 7 boxplots.")


