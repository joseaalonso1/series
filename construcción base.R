rm(list = ls())
pacman::p_load(tidyverse, glue, readxl, janitor, lubridate, scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Leer los datos
df_9 <- read_csv("040_R04A_417_9.csv")
df_10 <- read_csv("040_R04A_417_10.csv")
catalog <- read_csv("catalogo_R04A_417_BM.csv", locale = locale(encoding = "LATIN1"))

# Join con catálogo
df_9 <- df_9 %>%
  left_join(catalog, by = "concepto")

df_10 <- df_10 %>%
  left_join(catalog, by = "concepto")

# Definir códigos por etapa
# Definir nuevos códigos por etapa
etapa_1 <- c(
  101800505014, 101801406055, 101808208113, 101808208114, 101808208115,
  101801406056, 101808408120, 101808408119, 101808408121, 101808408122,
  101808408123, 101808408124, 101808408125, 101808408126,
  101801406057, 101808608135, 101808608136, 101808608137, 101808608138, 101808608139
)

etapa_2 <- c(
  101800505015, 101801506059, 101805907088, 101805907089,
  101801506060, 101805907090, 101806007091, 101806007092, 101806007093,
  101806007094, 101806007095, 101806007096, 101806007097, 101806007098,
  101801506061, 101806107099, 101806107100, 101806107101, 101806107102, 101806107103
)

etapa_3 <- c(
  101800505016, 101801606062, 101806207104, 101806207105, 101806207106,
  101801606063, 101806307107, 101806307108, 101806307110, 101806307109,
  101806307111, 101806307112, 101806307113, 101806307114,
  101801606064, 101806407115, 101806407116, 101806407117, 101806407118, 101806407119
)

codigos_confirmados <- c(etapa_1, etapa_2, etapa_3)

codigos_confirmados <- c(etapa_1, etapa_2, etapa_3)

# Añadir columna de etapa
df_9 <- df_9 %>%
  mutate(stage = case_when(
    concepto %in% etapa_1 ~ "Stage 1",
    concepto %in% etapa_2 ~ "Stage 2",
    concepto %in% etapa_3 ~ "Stage 3",
    TRUE ~ NA_character_
  ))

df_10 <- df_10 %>%
  mutate(stage = case_when(
    concepto %in% etapa_1 ~ "Stage 1",
    concepto %in% etapa_2 ~ "Stage 2",
    concepto %in% etapa_3 ~ "Stage 3",
    TRUE ~ NA_character_
  ))

# Filtrar conceptos confirmados y agrupar
balance <- df_9 %>%
  filter(concepto %in% codigos_confirmados) %>%
  group_by(periodo, descripcion, concepto, stage) %>%
  summarise(importe_pesos = sum(importe_pesos, na.rm = TRUE), .groups = "drop")

eprc <- df_10 %>%
  filter(concepto %in% codigos_confirmados) %>%
  group_by(periodo, descripcion, concepto, stage) %>%
  summarise(importe_pesos = sum(importe_pesos, na.rm = TRUE), .groups = "drop")

I <- balance %>%
  rename(saldo = importe_pesos) %>%
  left_join(eprc %>% rename(eprc = importe_pesos),
            by = c("periodo", "descripcion", "concepto", "stage"))

# Transformar a millones y calcular PE
I <- I %>%
  mutate(
    saldo = saldo / 10^6,
    eprc = eprc / 10^6,
    pe = eprc / saldo
  )

# Si periodo es tipo YYYYMM, convertirlo a fecha (asumiendo que es día 01)
I <- I %>%
  mutate(periodo_date = ymd(paste0(periodo, "01")))

I<-I %>%filter( stage=="Stage 1" & (descripcion=="Automotriz"| descripcion=="3. Créditos a la vivienda"|descripcion== "Nómina"| descripcion=="Personales"|
              descripcion=="Tarjeta de crédito"|descripcion=="Actividad empresarial o comercial"|descripcion=="Entidades financieras"))

II <- I %>%
  filter(
    !str_starts(descripcion, "A\\)"),
    !str_starts(descripcion, "1\\."),
    !str_starts(descripcion, "2\\."),
    !str_starts(descripcion, "3\\."),
    !is.na(saldo), saldo != 0,
    !is.na(eprc), eprc != 0
  )


graficar_pe <- function(data, etapa, color = "blue") {
  data %>%
    filter(stage == etapa) %>%
    ggplot(aes(x = periodo_date, y = pe)) +
    geom_line(color = color) +
    facet_wrap(~descripcion, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(
      title = glue::glue("Evolución de PE - {etapa}"),
      x = "Periodo",
      y = "PE (%)"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "grey80", size = 0.5),
      panel.grid.minor = element_line(color = "grey90", size = 0.25)
    )
}


# Generar las tres gráficas
#graficar_pe(II, "Stage 1", color = "blue")
#graficar_pe(II, "Stage 2", color = "orange")
#graficar_pe(II, "Stage 3", color = "red")


II <- II %>% 
  select(-periodo_date, -eprc, -stage, -concepto, -saldo) %>% 
  mutate(pe = pe * -1) %>% 
  pivot_wider(names_from = descripcion, values_from = pe, names_prefix = "y_pe_") %>% 
  janitor::clean_names()


F0 <- seq(ymd("2022-01-01"), by = "month", length.out = nrow(II))
II <- II %>%
  mutate(fecha = format(F0, "%Y-%m")) %>% select(-periodo)
#tipo de cambio peso /usd

III <- read_csv(
  glue::glue("variables/Consulta_20250701-222533023.csv"),
  skip = 18,
  locale = locale(encoding = "ISO-8859-1")
) %>% glimpse() %>% rename(USD_MXN=SF17908)


F1 <- seq(ymd("2020-01-01"), by = "month", length.out = nrow(III))
III <- III %>%
  mutate(fecha = format(F1, "%Y-%m")) %>% select(-Fecha)


#tiie de fondeo
IV <- read_csv(
  glue::glue("variables/Consulta_20250701-222657325.csv"),
  skip = 18,
  locale = locale(encoding = "ISO-8859-1")
) %>%
  rename(TIIE_fondeo = SF331451) %>%
  mutate(
    fecha = dmy(Fecha),  # Cambia a dmy, ymd, o mdy según lo que hayas visto en el paso 2
    mes = format(fecha, "%Y-%m")
  ) %>%
  select(TIIE_fondeo, mes) %>%
  group_by(mes) %>%
  summarise(TIIE_fondeo = mean(TIIE_fondeo, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(fecha = mes)


#remesas (USD m)
V <- read_csv(
  glue::glue("variables/Consulta_20250701-231006674.csv"),
  skip = 18,
  locale = locale(encoding = "ISO-8859-1")
)%>% rename(remesas=SE27803) 


F3 <- seq(ymd("2020-01-01"), by = "month", length.out = nrow(V))
V <- V %>%
  mutate(fecha = format(F3, "%Y-%m")) %>% select(-Fecha)


#m1,m2,m3 (miles de pesos)

VI <- read_csv(
  glue::glue("variables/Consulta_20250701-230824740.csv"),
  skip = 18,
  locale = locale(encoding = "ISO-8859-1")
) %>% rename(m1=SF311408, m2=SF311418, m3=SF311433) %>% select(Fecha,m1,m2,m3)

F4 <- seq(ymd("2020-01-01"), by = "month", length.out = nrow(VI))
VI <- VI %>%
  mutate(fecha = format(F4, "%Y-%m")) %>% select(-Fecha)

#balanza comercial no petrolera (miles de USD)
VII <- read_csv(
  glue::glue("variables/Consulta_20250701-231137093.csv"),
  skip = 18,
  locale = locale(encoding = "ISO-8859-1")
)%>% rename(balanza=SE35403) 

F5 <- seq(ymd("2020-01-01"), by = "month", length.out = nrow(VII))
VII <- VII %>%
  mutate(fecha = format(F5, "%Y-%m")) %>% select(-Fecha)

#INPC
VIII <- read_excel(
  glue::glue("variables/ca55_2018a.xlsx"),
  skip = 12
)%>%janitor::clean_names() %>% rename(inpc=x865541) 

F6 <- seq(ymd("2020-01-01"), by = "month", length.out = nrow(VIII))
VIII <- VIII %>%
  mutate(fecha = format(F6, "%Y-%m")) %>% select(inpc,fecha)

#EMOE confianza empresarial// cambiar esto [...] archivo desde 2020 
IX <- read_excel(
  glue::glue("variables/EMOE_1.xlsx"),
  skip = 6
)%>%janitor::clean_names() %>% rename(igoec=indicador) 
IX<-IX %>% mutate(igoec = na_if(igoec, "-")) %>%
  drop_na(igoec)

F7 <- seq(ymd("2020-01-01"), by = "month", length.out = nrow(IX))
IX <- IX %>%
  mutate(fecha = format(F7, "%Y-%m")) %>% select(igoec,fecha)

#IGAE
X <- read_excel(
  glue::glue("variables/IGAE_2.xlsx"),
  skip = 5
)%>%janitor::clean_names()
X <- X %>%
  slice(2) %>% 
  clean_names() %>%
  rename_with(~ gsub(" ", "_", .x)) %>%
  mutate(across(everything(), as.numeric)) %>%
  pivot_longer(
    cols = -x1, 
    names_to = "d", 
    values_to = "igae"
  ) %>%
  filter(!str_detect(d, "anual"))   # Excluye columnas con "anual" en el nombre

# Genera fechas mensuales
F8 <- seq(ymd("1993-01-01"), by = "month", length.out = nrow(X))

# Asigna fechas, filtra desde 2020
X <- X %>%
  mutate(fecha = format(F8, "%Y-%m")) %>%
  select(igae, fecha) %>%
  filter(fecha >= "2020-01")


#google
XI <- read_csv(
  glue::glue("variables/multiTimeline.csv"),
  skip = 1,
  locale = locale(encoding = "ISO-8859-1")
) %>%
  janitor::clean_names() %>%
  mutate(
    semana = ymd(semana),  # Transformas a fecha real
    mes = format(semana, "%Y-%m")
  ) %>%
  group_by(mes) %>%
  summarise(
    google_deuda = mean(deuda_ma_c_xico, na.rm = TRUE),
    google_empleo = mean(empleo_ma_c_xico, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(mes >= "2020-01", mes <= "2025-12") %>%
  rename(fecha = mes)

#PT permanentes
XII <- read_csv(
  glue::glue("variables/ppweb.csv")
) %>%
  janitor::clean_names() %>%
  filter(str_detect(x2, "/")) %>%  # Solo los registros que tienen año/mes
  mutate(
    fecha = parse_date_time(x2, orders = "Y/b")  # Interpreta el formato "YYYY/Mon"
  ) %>%
  filter(fecha >= ymd("2020-01-01"), fecha <= ymd("2025-12-31")) %>%  # Filtra fechas
  select(nacional, fecha)

#IPC
XIII <- read_excel(
  glue::glue("variables/PerformanceGraphExport-2.xls"),
  skip = 6
) %>%
  janitor::clean_names()

F11 <- seq(ymd("2020-01-01"), by = "day", length.out = nrow(XIII))  # asumes datos diarios

XIII <- XIII %>%
  mutate(fecha = F11) %>%
  mutate(fecha = format(fecha, "%Y-%m")) %>%  # extraes el mes
  group_by(fecha) %>%
  summarise(s_p_bmv_ipc = mean(s_p_bmv_ipc, na.rm = TRUE)) %>%
  ungroup()

#SP500
XIV <- read_excel(
  glue::glue("variables/PerformanceGraphExport.xls"),
  skip = 6
) %>%
  janitor::clean_names()

F12 <- seq(ymd("2020-01-01"), by = "day", length.out = nrow(XIV))  # asumes datos diarios

XIV <- XIV %>%
  mutate(fecha = F12) %>%
  mutate(fecha = format(fecha, "%Y-%m")) %>%  # extraes el mes
  group_by(fecha) %>%
  summarise(s_p_500 = mean(s_p_500, na.rm = TRUE)) %>%
  ungroup()


# Lista de dataframes
bases <- list(II,III, IV, V, VI, VII, VIII, IX, X, XI, XII, XIII,XIV)

bases <- lapply(bases, function(df) {
  df %>%
    mutate(
      fecha = parse_date_time(fecha, orders = c("ymd", "dmy", "mdy", "ym", "my")),  # Detecta el formato
      llave = format(fecha, "%Y-%m")
    ) %>%
    select(-fecha)
})

master_base <- reduce(bases, full_join, by = "llave")


rm(master_base)  # Limpias si estaba mal
gc()             # Limpias memoria
# Corre de nuevo el bloque que lo genera
master_base <- reduce(bases, full_join, by = "llave")
master_base <- master_base %>% ungroup()%>%dplyr::arrange(llave)

# Filtrar y crear fecha tipo Date
master_base <- master_base %>%
  filter(llave >= "2022-01", llave <= "2025-03") %>%
  mutate(fecha = ym(llave)) %>%
  relocate(fecha, .after = llave)  # Opcional: deja fecha al lado de llave

master_base<-master_base %>% rename(Puestos_Trabajo=nacional) 


# Escribir a Excel

#openxlsx:: write.xlsx(master_base, "master_base_filtrado.xlsx", overwrite = TRUE)

library(tidyverse)
library(readxl)
library(lubridate)
library(tseries)
library(writexl)

# ========================
# Cargar datos
# ========================
XV <- master_base %>% mutate(llave = ym(llave)) %>% arrange(llave)
variables_numericas <- XV %>% select(-fecha, -llave) %>% select(where(is.numeric)) %>% names()

# ========================
# Función con tseries::adf.test
# ========================
evaluar_adf_tseries <- function(x, nombre) {
  x <- na.omit(x)
  if (length(x) < 20 || sd(x) < 1e-6 || length(unique(x)) < 5) {
    return(tibble(variable = nombre, adf_stat = NA, adf_pvalue = NA, estacionaria = FALSE, usable = "NO"))
  }
  test <- tryCatch(adf.test(x, k = 0), error = function(e) NULL)
  if (is.null(test)) {
    return(tibble(variable = nombre, adf_stat = NA, adf_pvalue = NA, estacionaria = FALSE, usable = "NO"))
  }
  stat <- test$statistic
  pval <- test$p.value
  estacionaria <- pval < 0.05
  usable <- ifelse(estacionaria, "OK", "NO")
  tibble(variable = nombre, adf_stat = stat, adf_pvalue = pval, estacionaria = estacionaria, usable = usable)
}

# ========================
# Evaluar series originales
# ========================
resultados_adf_original <- map_dfr(variables_numericas, ~ evaluar_adf_tseries(XV[[.x]], .x))

# ========================
# Primeras diferencias
# ========================
XVI <- XV %>%
  mutate(across(all_of(variables_numericas), ~ .x - lag(.x), .names = "diff_{.col}"))

variables_diff <- paste0("diff_", variables_numericas)

resultados_adf_diff <- map_dfr(variables_diff, ~ evaluar_adf_tseries(XVI[[.x]], .x))

# ========================
# Exportar resultados
# ========================
write_xlsx(resultados_adf_original, "resultados_adf_tseries_original.xlsx")
write_xlsx(resultados_adf_diff, "resultados_adf_tseries_diferencias.xlsx")

#afc y  pacf// se me estaban olvidando
#Moving average -> pero es necesario el smoothing ?

