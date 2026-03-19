
# Librerías ---------------------------------------------------------------

pacman::p_load(BiocManager, lmtest, car, MASS, NHSRdatasets, ResourceSelection,
               pROC, rcompanion, nortest, tidyverse, caret, mgcv, rms,
               jtools, forestmodel,gvlma, trafo, readxl, tidyverse, naniar,
               DataExplorer, FactoMineR, factoextra, corrplot,sjPlot,ggplot2,
               MVN, DT, tidyplots, performance,see,SmartEDA,doBy, fitdistrplus,
               forcats, patchwork,plotly, gapminder, gganimate, hrbrthemes,
               babynames)



# Carga de datos ----------------------------------------------------------
data <- read_csv(("US_Accidents_March23_sampled_500k.csv"), show_col_types = F)
glimpse(data)

### Renombrando variables
data <- data %>% 
  rename(
    ID = ID,
    Fuente = Source,
    Severidad = Severity,
    Hora_Inicio = Start_Time,
    Hora_Fin = End_Time,
    Latitud_Inicio = Start_Lat,
    Longitud_Inicio = Start_Lng,
    Latitud_Fin = End_Lat,
    Longitud_Fin = End_Lng,
    Distancia_mi = `Distance(mi)`,
    Descripcion = Description,
    Calle = Street,
    Ciudad = City,
    Condado = County,
    Estado = State,
    Codigo_Postal = Zipcode,
    Pais = Country,
    Zona_Horaria = Timezone,
    Codigo_Aeropuerto = Airport_Code,
    Timestamp_Clima = Weather_Timestamp,
    Temperatura_F = `Temperature(F)`,
    Sensacion_Termica_F = `Wind_Chill(F)`,
    Humedad_Porc = `Humidity(%)`,
    Presion_pulg = `Pressure(in)`,
    Visibilidad_mi = `Visibility(mi)`,
    Direccion_Viento = Wind_Direction,
    Velocidad_Viento_mph = `Wind_Speed(mph)`,
    Precipitacion_pulg = `Precipitation(in)`,
    Condicion_Climatica = Weather_Condition,
    Servicio_Amenidad = Amenity,
    Resalto = Bump,
    Cruce = Crossing,
    Ceda_el_Paso = Give_Way,
    Interseccion = Junction,
    Sin_Salida = No_Exit,
    Via_Ferrea = Railway,
    Glorieta = Roundabout,
    Estacion = Station,
    Pare = Stop,
    Reductor_Velocidad = Traffic_Calming,
    Semaforo = Traffic_Signal,
    Retorno = Turning_Loop,
    Amanecer_Atardecer = Sunrise_Sunset,
    Crepusculo_Civil = Civil_Twilight,
    Crepusculo_Nautico = Nautical_Twilight,
    Crepusculo_Astronomico = Astronomical_Twilight
  )

# Verificar los nuevos nombres
colnames(data)

plot_missing(data) +
  labs(title = "Porcentaje de valores faltantes por variable") +
  theme_minimal()

data <- data %>% select(-Velocidad_Viento_mph,
                -Sensacion_Termica_F,
                -Precipitacion_pulg,
                -Longitud_Fin,
                -Latitud_Fin,
                -ID) 

# 1. ELIMINAR NAs Y LIMPIAR FACTORES
data <- data %>%
  na.omit() %>%  # Eliminar filas con NA
  mutate(across(where(is.factor), droplevels))  # Eliminar niveles no usados


# conversión de variables a factor ----------------------------------------
data <- data %>%
  
  # 1. Variables de texto a factor
  mutate(across(c(Fuente, Estado, Pais, Zona_Horaria, 
                  Direccion_Viento, Condicion_Climatica,
                  Amanecer_Atardecer, Crepusculo_Civil,
                  Crepusculo_Nautico, Crepusculo_Astronomico), 
                as.factor)) %>%
  
  # 2. Severidad como factor ORDINAL
  mutate(Severidad = factor(Severidad, 
                            levels = c(1, 2, 3, 4),
                            labels = c("Leve", "Moderado", 
                                       "Grave", "Muy_Grave"),
                            ordered = TRUE)) %>%
  
  # 3. Variables lógicas a factor
  mutate(across(where(is.logical), 
                ~ factor(., levels = c(FALSE, TRUE), 
                         labels = c("No", "Si")))) %>%
  
  # 4. Extraer componentes temporales de Hora_Inicio
  mutate(
    año       = as.factor(year(Hora_Inicio)),
    mes       = as.factor(month(Hora_Inicio, label = TRUE)),
    dia_semana = as.factor(wday(Hora_Inicio, label = TRUE)),
    hora      = hour(Hora_Inicio),
    franja_horaria = factor(case_when(
      hora >= 0  & hora < 6  ~ "Madrugada",
      hora >= 6  & hora < 12 ~ "Mañana",
      hora >= 12 & hora < 18 ~ "Tarde",
      hora >= 18 & hora <= 23 ~ "Noche"
    ), levels = c("Madrugada", "Mañana", "Tarde", "Noche"),
    ordered = FALSE)
  )

data <- data %>%
  mutate(
    # 1. Traducir Fuente
    Fuente = fct_recode(Fuente,
                        "Fuente 1" = "Source1",
                        "Fuente 2" = "Source2",
                        "Fuente 3" = "Source3"
    ),
    
    # 2. Traducir País
    Pais = fct_recode(Pais, "EE. UU." = "US"),
    
    # 3. Traducir Zonas Horarias
    Zona_Horaria = fct_recode(Zona_Horaria,
                              "Central (EE.UU.)"  = "US/Central",
                              "Este (EE.UU.)"     = "US/Eastern",
                              "Montaña (EE.UU.)"  = "US/Mountain",
                              "Pacífico (EE.UU.)" = "US/Pacific"
    ),
    
    # 4. Traducir Direcciones del Viento (Mapeo de puntos cardinales)
    Direccion_Viento = fct_recode(Direccion_Viento,
                                  "Calma" = "Calm", "Calma" = "CALM",
                                  "Norte" = "North", "N" = "N",
                                  "Sur" = "South", "S" = "S",
                                  "Este" = "East", "E" = "E",
                                  "Oeste" = "West", "W" = "W",
                                  "Variable" = "Variable", "Var" = "VAR"
    ),
    
    # 5. Traducir Ciclos de luz (Día/Noche)
    across(c(Amanecer_Atardecer, Crepusculo_Civil, Crepusculo_Nautico, Crepusculo_Astronomico),
           ~ fct_recode(.x, "Día" = "Day", "Noche" = "Night")),
    
    # 6. Traducir Condición Climática (Los más comunes y patrones)
    # Nota: Al ser 107, usaremos reemplazo de texto para mayor eficiencia
    Condicion_Climatica = as.character(Condicion_Climatica) %>%
      str_replace_all(c(
        "Fair" = "Despejado",
        "Clear" = "Cielo despejado",
        "Cloudy" = "Nublado",
        "Mostly" = "Mayormente",
        "Partly" = "Parcialmente",
        "Overcast" = "Cubierto",
        "Scattered Clouds" = "Nubes dispersas",
        "Rain" = "Lluvia",
        "Light" = "Ligera",
        "Heavy" = "Fuerte",
        "Drizzle" = "Llovizna",
        "Fog" = "Niebla",
        "Mist" = "Neblina",
        "Haze" = "Bruma",
        "Snow" = "Nieve",
        "Thunderstorm|T-Storm" = "Tormenta",
        "Wintry Mix" = "Mezcla invernal",
        "Windy" = "con Viento",
        "Showers" = "Chubascos",
        "Patches of" = "Parches de"
      )) %>%
      as.factor()
  )

plot_missing(data) +
  labs(title = "Porcentaje de valores faltantes por variable") +
  theme_minimal()

# Gráficos  --------------------------------------------------------------

### Gráfico de barras
bar1 <- ggplot(df) + 
  geom_bar(aes(x = Estado, fill = factor(Estado))) +
  coord_cartesian(ylim = c(0, 10000)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

b1 <- ggplotly(bar1)

bar2 <- ggplot(df) + 
  geom_bar(aes(x = franja_horaria, fill = factor(franja_horaria))) +
  coord_cartesian(ylim = c(0, 40000)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

b2 <- ggplotly(bar2)

bar3 <-  ggplot(df) + 
  geom_bar(aes(x = Condicion_Climatica_Agrup,
               fill = factor(Condicion_Climatica_Agrup))) +
  coord_cartesian(ylim = c(0, 50000)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

b3 <- ggplotly(bar3)

bar4 <- ggplot(df) + 
  geom_bar(aes(x = mes,
               fill = factor(mes))) +
  coord_cartesian(ylim = c(0, 15000)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

b4 <- ggplotly(bar4)

bar5 <- ggplot(df) + 
  geom_bar(aes(x = Semaforo,
               fill = factor(Semaforo))) +
  coord_cartesian(ylim = c(0, 80000)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

b5 <- ggplotly(bar5)

bar6 <- ggplot(df) + 
  geom_bar(aes(x = Interseccion,
               fill = factor(Interseccion))) +
  coord_cartesian(ylim = c(0, 100000)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

b6 <- ggplotly(bar6)

## Gráfico de ojiva
# ── 1. Ojiva por AÑO ──────────────────────────────────────
ojiva_año <- datos %>%
  group_by(año) %>%
  summarise(total = sum(Accidentes)) %>%
  arrange(año) %>%
  mutate(
    acumulado     = cumsum(total),
    freq_rel_acum = acumulado / sum(total) * 100
  )

ojiva_año <- ojiva_año %>%
  mutate(año = as.numeric(as.character(año)))

p1 <- ggplot(ojiva_año, aes(x = año, y = freq_rel_acum, group = 1)) +
  # Usamos stat = "identity" para evitar el error de geom_area
  geom_area(fill = "#2196F3", alpha = 0.15, stat = "identity") +
  geom_line(color = "#2196F3", linewidth = 1.2) +
  geom_point(color = "#2196F3", size = 3) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%")) +
  # Aseguramos que los años en el eje X se vean bien (sin decimales)
  scale_x_continuous(breaks = seq(min(ojiva_año$año), max(ojiva_año$año), by = 1)) +
  labs(title    = "Accidentes por Año",
       subtitle = "Frecuencia relativa acumulada",
       x = "Año", y = "% Acumulado") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) +
  transition_reveal(año)

p1_1 <- animate(p1, renderer = gifski_renderer())
anim_save("ojiva_accidentes_anual.gif", animation = p1_1)
# ── 2. Ojiva por MES ──────────────────────────────────────
ojiva_mes <- datos %>%
  group_by(mes) %>%
  summarise(total = sum(Accidentes)) %>%
  arrange(mes) %>%
  mutate(
    acumulado     = cumsum(total),
    freq_rel_acum = acumulado / sum(total) * 100
  )

ojiva_mes <- ojiva_mes %>%
  mutate(mes_eje = as.numeric(as.factor(mes)))

p2 <- ggplot(ojiva_mes, aes(x = mes_eje, y = freq_rel_acum, group = 1)) +
  # Agregamos stat = "identity" a geom_area para evitar el conflicto
  geom_area(fill = "#E91E63", alpha = 0.15, stat = "identity") +
  geom_line(color = "#E91E63", linewidth = 1.2) +
  geom_point(color = "#E91E63", size = 3) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%")) +
  scale_x_continuous(breaks = 1:12) +
  labs(title    = "Accidentes por Mes",
       subtitle = "Frecuencia relativa acumulada",
       x = "Mes", y = "% Acumulado") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) + 
  transition_reveal(mes_eje)

p2_2 <- animate(p2, renderer = gifski_renderer())
anim_save("ojiva_accidentes_mes.gif", animation = p2_2)

# ── 3. Ojiva por FRANJA HORARIA ───────────────────────────
ojiva_franja <- datos %>%
  group_by(franja_horaria) %>%
  summarise(total = sum(Accidentes)) %>%
  mutate(franja_horaria = factor(franja_horaria,
                                 levels = c("Madrugada", "Mañana",
                                            "Tarde", "Noche"))) %>%
  arrange(franja_horaria) %>%
  mutate(
    acumulado     = cumsum(total),
    freq_rel_acum = acumulado / sum(total) * 100
  )

ojiva_franja <- ojiva_franja %>%
  mutate(franja_idx = as.numeric(franja_horaria))

p3 <- ggplot(ojiva_franja, aes(x = franja_idx, y = freq_rel_acum, group = 1)) +
  geom_area(fill = "#FF9800", alpha = 0.15, stat = "identity") +
  geom_line(color = "#FF9800", linewidth = 1.2) +
  geom_point(color = "#FF9800", size = 3) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%")) +
  # Volvemos a poner los nombres de las franjas en el eje X
  scale_x_continuous(breaks = 1:4, labels = levels(ojiva_franja$franja_horaria)) +
  labs(title    = "Accidentes por Franja Horaria",
       subtitle = "Frecuencia relativa acumulada",
       x = "Franja Horaria", y = "% Acumulado") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold")) +
  transition_reveal(franja_idx)

p3_3 <- animate(p3, renderer = gifski_renderer())
anim_save("ojiva_accidentes_franja.gif", animation = p3_3)
### Gráficos de boxplot

Box1 <- ggplot(df) + 
  geom_boxplot(aes(y = Accidentes), fill = "steelblue") +
  theme_minimal()

### Gráfico de boxplot (identificación de outliers)
BOX1 <- ggplot(df, aes(x = Estado, y = Accidentes, fill = Estado)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Relación accidentes según tipo de estado",
       x = "Estado",
       y = "Número de accidentes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")


BOX2 <- ggplot(df, aes(x = Condicion_Climatica_Agrup,
    y = Accidentes, fill = Condicion_Climatica_Agrup)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Relación Accidentes según el clima",
       x = "Condiciones climaticas",
       y = "Número de accidentes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

BOX3 <- ggplot(df, aes(x = mes, y = Accidentes, fill = message())) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Relación accidentes según mes ",
       x = "Meses",
       y = "Número de accidentes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

BOX4 <- ggplot(df, aes(x = Semaforo, y = Accidentes, fill = Semaforo)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Relación según tipo de semaforo ",
       x = "Semaforo",
       y = "Número de accidentes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

BOX5 <- ggplot(df, aes(x = Interseccion, y = Accidentes, fill = Interseccion)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Relación según tipo de intersección ",
       x = "Intersección ",
       y = "Número de accidentes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")


# Selección y ajuste para modelo poisson ----------------------------------

datos <- data %>%
  group_by(Estado, año, mes, franja_horaria, 
           Condicion_Climatica, Semaforo, Interseccion) %>%
  summarise(
    Accidentes = n(),
    temp_media   = mean(Temperatura_F),
    visib_media  = mean(Visibilidad_mi),
    .groups = "drop"
  )

datos <- datos %>%
  mutate(Condicion_Climatica_Agrup = case_when(
    
    # Despejado
    Condicion_Climatica %in% c("Despejado", "Cielo despejado",
                               "Despejado / con Viento") ~ "Despejado",
    
    # Nublado
    Condicion_Climatica %in% c("Nublado", "Nublado / con Viento",
                               "Cubierto", "Mayormente Nublado",
                               "Mayormente Nublado / con Viento",
                               "Parcialmente Nublado",
                               "Parcialmente Nublado / con Viento",
                               "Nubes dispersas") ~ "Nublado",
    
    # Lluvia
    grepl("Lluvia|Llovizna|Chubascos|Shower", 
          Condicion_Climatica) ~ "Lluvia",
    
    # Nieve
    grepl("Nieve|Sleet|Ice Pellets|Freezing|Hail|
           Mezcla invernal|Blowing Nieve", 
          Condicion_Climatica) ~ "Nieve_Hielo",
    
    # Niebla / Visibilidad reducida
    grepl("Niebla|Neblina|Bruma|Fog|Smoke|Haze", 
          Condicion_Climatica) ~ "Visibilidad_Reducida",
    
    # Tormenta / Viento fuerte
    grepl("Tormenta|Thunder|Tornado|Squalls|
           Fuerte Viento|Dust|Sand|Volcanic", 
          Condicion_Climatica) ~ "Tormenta_Extremo",
    
    # Resto
    TRUE ~ "Otras"
  )) %>%
  
  # Convertir a factor
  mutate(Condicion_Climatica_Agrup = factor(Condicion_Climatica_Agrup))

# Verificar
levels(datos$Condicion_Climatica_Agrup)

mean(datos$Accidentes)
var(datos$Accidentes)

# Distribución de la variable respuesta
hist(datos$Accidentes, 
     main = "Distribución de n_accidentes",
     xlab = "Conteo", col = "steelblue")


df <- datos %>% 
  select(Accidentes,
           Estado,
           año,
           mes,
           franja_horaria,
           Condicion_Climatica_Agrup,
           Semaforo,
           Interseccion,
           temp_media,
           visib_media)

## ANÁLISIS  DE GRÁFICO 
# 1. Ajuste de modelos (asegúrate de que existan)
fit.poisson = fitdist(df$Accidentes, "pois")
fit.negbin  = fitdist(df$Accidentes, "nbinom")

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


# Aplicación del modelo ---------------------------------------------------

General <- glm(Accidentes ~ ., data = df, family = "poisson")
summ(General,exp = T, confint = T)

## Prueba de sobre dispersión
deviance(General)/summary(General)$df[2] 
performance::check_overdispersion(General)
## devianza = 3.913967 , como el supuesto se inflige, en este caso,
## el modelo presenta sobredisperción, por lo cuál,
## emplearemos un modelo Binomial negativo para ver comparación.

# Modelo Binomial Negativo----------------------------------------------------
Formula <- formula(General)

mod_bin_neg <- glm.nb(Accidentes ~., data = df)
summary(mod_bin_neg)

## Prueba de sobre dispersión
deviance(mod_bin_neg)/summary(mod_bin_neg)$df[2] #devianza = 0.832627, el modelo ya
# considera la probabilidad de sobredispersión

summ(mod_bin_neg, confint = T, exp = T)
summary(mod_bin_neg)

## Inferencia sobre los coeficientes
coeftest(mod_bin_neg) # errores standar tipo sándwich

# Gráfico (supuestos) ----------------------------------------------------------------

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
plot(mod_bin_neg,
     which = 1:4,
     col = "steelblue",
     pch = 20,
     cex = 0.5)
par(mfrow = c(1, 1))

# gráficos 2 --------------------------------------------------------------

# 1. Generar predicciones
df$predicciones <- predict(mod_bin_neg, type = "response")

# 2. Comparativa Real vs Predicho
comparativa <- df %>%
  group_by(Accidentes) %>%
  summarise(
    Real    = n(),
    Predicho = mean(predicciones)  # promedio de predicciones por grupo
  )

# 3. Visualización
ggplot(comparativa, aes(x = Accidentes)) +
  geom_point(aes(y = Real,    color = "Real"),    size = 2) +
  geom_point(aes(y = Predicho, color = "Predicho"), size = 2) +
  geom_line(aes(y = Real,    color = "Real")) +
  geom_line(aes(y = Predicho, color = "Predicho")) +
  scale_color_manual(values = c("Real" = "steelblue", 
                                "Predicho" = "darkorange")) +
  labs(title = "Comparación: Valores Reales vs Predichos",
       x = "Número de Accidentes",
       y = "Frecuencia",
       color = "Tipo") +
  theme_minimal() +
  xlim(0, 50)  # limitar eje x para mejor visualización
# ya que tienes valores hasta 687
#%>%
 # filter(Accidentes <= 5) # Filtramos hasta 5 para que sea legible


# Exportación objetos -----------------------------------------------------
# 1. Definir ruta
ruta_out <- "C:/Users/user/3D Objects/MODELOS_POS/Modelos_Poisson/Bases/ACCI_EE.UU/out/"
if (!dir.exists(ruta_out)) dir.create(ruta_out, recursive = TRUE)

# 2. Guardar Tablas Descriptivas (D1 a D7)
#tablas_descriptivas <- list(via=D1, zona=D2, sev=D3, sup=D4, clim=D5, vel=D6, veh=D7)
#saveRDS(tablas_descriptivas, paste0(ruta_out, "tablas_descriptivas_completas.rds"))

# 3. Guardar Gráficos de Barras (bar1 a bar4)
graficos_barras <- list(b1,b2,b3,b4, b5,b6)
saveRDS(graficos_barras, paste0(ruta_out, "graficos_barras.rds"))

# 4. Guardar Gráficos Boxplot (Box1 a Box3 y BOX1 a BOX4)
graficos_boxplot <- list(
  simples = list(B1=Box1),
  outliers = list(BOX1,BOX2, BOX3, BOX4, BOX5)
)

saveRDS(Ojiva, paste0(ruta_out, "Ojiva.rds"))
saveRDS(graficos_boxplot, paste0(ruta_out, "graficos_boxplot_todos.rds"))

# 5. Guardar Modelos y otros objetos clave
saveRDS(General, paste0(ruta_out, "modelo.rds"))
saveRDS(mod_bin_neg, paste0(ruta_out, "modelo_binomial.rds"))
saveRDS(Formula, paste0(ruta_out, "Formula.rds"))
saveRDS(df, paste0(ruta_out, "df_modelo.rds"))

library(writexl)
write_xlsx(df, "muestra_ee_uu.xlsx")
