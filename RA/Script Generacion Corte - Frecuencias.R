
# 1. Numero de hogares acompañados = CalculoHogares
# 2. CalculoIntegrantes vs Cruzar RegAdmin (marca que indique si idintegrante esa en Reg Admin)
# 3. De los que cruzan anteriormente a cuantos hogares unicos corresponden
# 4. Estado del hoagr de los anteriores hogares unicos (CalculoHogares)

#### Rutas ####

Carpeta = dirname(rstudioapi::getSourceEditorContext()$path)#El ultimo slash o backslash no se debe incluir

slash = "/"

Entradas=paste(Carpeta,"1. Entradas", sep = slash)# Defina el escritorio de entrada donde están los archivos requeridos.
Salidas =paste(Carpeta,"2. Salidas", sep = slash)# Defina el escritorio de salida donde serán enviado los archivos generados.

#     LIBRERIAS   ####
setwd(Carpeta)
source("Librerias.R", encoding = "UTF-8")# Las librerias que se usaran

#### Preparación Base Corte ####

setwd(Entradas)

LOGROS_HOG = read_delim("Unidos_Logros_Hogar_20211229.txt", delim = "|", escape_double = FALSE, 
                        locale = locale(), trim_ws = TRUE)

ConsultaDuplicadoE09 = read_delim("ConsultaDuplicadoE09.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

Marca_hogares_e_integrantes_con_posible_duplicidad_Precargue_SISBEN_IV = read_excel("Marca hogares e integrantes con posible duplicidad_Precargue SISBEN IV.xlsx", 
                                                                                    sheet = "Hoja1")

HogaresA = length(LOGROS_HOG$EstadoHogar[LOGROS_HOG$EstadoHogar == "ACOMPAÑADO"])

HogaresA = LOGROS_HOG %>% filter(EstadoHogar == "ACOMPAÑADO")

LOGROS_HOG = LOGROS_HOG %>% mutate(PosiblesDuplicados = ifelse((idHogar %in% ConsultaDuplicadoE09$A01) | 
                                                                 (idHogar %in% Marca_hogares_e_integrantes_con_posible_duplicidad_Precargue_SISBEN_IV$A01), 0, 1))


HogaresPosiblesDuplicados = LOGROS_HOG %>% filter(PosiblesDuplicados == 0)


Unidos_Logros_Integrante = read_delim("Unidos_Logros_Integrante_20211229.txt", 
                                      delim = "|", escape_double = FALSE, locale = locale(),  trim_ws = TRUE)

ReporteAdministrativo = read_delim(paste("Reporte_Administrativos","ReporteAdministrativo_27_diciembre.txt", sep = slash), delim = ";", escape_double = FALSE, trim_ws = TRUE)

Unidos_Logros_Integrante = Unidos_Logros_Integrante %>% mutate(PosiblesDuplicados =  ifelse(idHogar %in% HogaresPosiblesDuplicados$idHogar, 0, 1))


CalculoIntegrantes = Unidos_Logros_Integrante %>% mutate(CruceRegAdmin = ifelse(idIntegranteHogar %in% ReporteAdministrativo$Id_Persona, 0, 1))

CalculoIntegrantes_2 = CalculoIntegrantes %>% filter(CruceRegAdmin == 0 & PosiblesDuplicados == 1)
HogaresUnicos = CalculoIntegrantes_2 %>% distinct(idHogar)
HogaresUnicos_2 = merge(HogaresUnicos, LOGROS_HOG[, c("idHogar", "EstadoHogar")], by = "idHogar", all.x = T)

# table(HogaresUnicos_2$EstadoHogar, useNA = "always")

# Ejercicio 

# 694909 Hogares Acompañados - verificar que al menos uno de sus integrantes cruza con Reg Admin
# Hogares Acompañados

HogaresUnicos_3 = HogaresUnicos_2 %>% filter(EstadoHogar == "ACOMPAÑADO")

HogaresUnicos_3 = HogaresUnicos_3 %>% filter(!idHogar %in% c(20589629, 20954899, 20486272))


#### Generación Corte ####

BaseGestion = merge(HogaresUnicos_3, LOGROS_HOG, by = "idHogar", all.x = T)

BaseGestion =  BaseGestion %>% rename("logro01_identificación" = logro01,
                                      "logro02_afiliación a salud" = logro02,
                                      "logro03_vacunación" = logro03,
                                      "logro04_atencion a menores en riesgo de desnutrición" = logro04,
                                      "logro05_control de crecimiento" = logro05,
                                      "logro06_educación inicial" = logro06,
                                      "logro07_escolarización" = logro07,
                                      "logro08_no trabajo infantil" = logro08,
                                      "logro09_acceso a agua" = logro09,
                                      "logro10_saneamiento básico" = logro10,
                                      "logro11_ingresos suficientes" = logro11,
                                      "logro12_libreta militar" = logro12,
                                      "logro13_registro de personas con discapacidad" = logro13,
                                      "logro14_elementos de apoyo y rehabilitación" = logro14,
                                      "logro15_seguridad alimentaria" = logro15,
                                      "logro16_derechos sexuales y reproductivos" = logro16,
                                      "logro17_leer y escribir" = logro17,
                                      "logro18_estudios postsecundarios" = logro18,
                                      "logro19_herramientas digitales" = logro19,
                                      "logro20_educación financiera" = logro20,
                                      "logro21_no pisos en tierra" = logro21,
                                      "logro22_paredes adecuadas" = logro22,
                                      "logro23_no hacinamiento" = logro23,
                                      "logro24_ingresos adultos mayores" = logro24,
                                      "logro25_actividad productiva" = logro25,
                                      "logro26_seguridad juridica del predio" = logro26,
                                      "logro27_familias en acción" = logro27,
                                      "logro28_jovenes en acción" = logro28)

BaseGestion = merge(BaseGestion, CalculoIntegrantes[, c("idHogar", "idIntegranteHogar")], by = "idHogar", all.x = T)
BaseGestion = merge(BaseGestion, Unidos_Logros_Integrante, by = c("idHogar", "idIntegranteHogar"), all.x = T)


BaseGestion =  BaseGestion %>% rename("logro01_I_identificación" = logro01,
                                      "logro02_I_afiliación a salud" = logro02,
                                      "logro03_I_vacunación" = logro03,
                                      "logro04_I_atencion a menores en riesgo de desnutrición" = logro04,
                                      "logro05_I_control de crecimiento" = logro05,
                                      "logro06_I_educación inicial" = logro06,
                                      "logro07_I_escolarización" = logro07,
                                      "logro08_I_no trabajo infantil" = logro08,
                                      "logro09_I_acceso a agua" = logro09,
                                      "logro10_I_saneamiento básico" = logro10,
                                      "logro11_I_ingresos suficientes" = logro11,
                                      "logro13_I_registro de personas con discapacidad" = logro13,
                                      "logro14_I_elementos de apoyo y rehabilitación" = logro14,
                                      "logro15_I_seguridad alimentaria" = logro15,
                                      "logro16_I_derechos sexuales y reproductivos" = logro16,
                                      "logro17_I_leer y escribir" = logro17,
                                      "logro18_I_estudios postsecundarios" = logro18,
                                      "logro20_I_educación financiera" = logro20,
                                      "logro21_I_no pisos en tierra" = logro21,
                                      "logro22_I_paredes adecuadas" = logro22,
                                      "logro23_I_no hacinamiento" = logro23,
                                      "logro24_I_ingresos adultos mayores" = logro24,
                                      "logro25_I_actividad productiva" = logro25,
                                      "logro26_I_seguridad juridica del predio" = logro26,
                                      "logro27_I_familias en acción" = logro27,
                                      "logro28_I_jovenes en acción" = logro28) %>% select(-idEncuesta.x, -idEncuesta.y, -fechaCalculo.x, -EstadoHogar.y) 


Unidos_IPM = read_delim("Unidos_IPM_20211028.txt", delim = "|", escape_double = FALSE, trim_ws = TRUE)

Base_Gestion_2021 = read_delim("Base_Gestion_2021.txt", delim = "|", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE) 


BaseGestion = merge(BaseGestion, Unidos_IPM[, c("idHogar", "fechaCalculo", "denominacionIPM")], by = "idHogar", all.x = T)

BaseGestion = BaseGestion  %>% rename("fechaCalculoLogros" = fechaCalculo.y,
                                      "fechaCalculoIPM" = fechaCalculo,
                                      "EstadoHogar" = EstadoHogar.x) %>% select(idHogar, EstadoHogar, idIntegranteHogar, logro01_identificación, `logro02_afiliación a salud`, logro03_vacunación, `logro04_atencion a menores en riesgo de desnutrición`,
                                                                                `logro05_control de crecimiento`, `logro06_educación inicial`, logro07_escolarización, `logro08_no trabajo infantil`, `logro09_acceso a agua`, `logro10_saneamiento básico`, `logro11_ingresos suficientes`,
                                                                                `logro12_libreta militar`, `logro13_registro de personas con discapacidad`, `logro14_elementos de apoyo y rehabilitación`, `logro15_seguridad alimentaria`, `logro16_derechos sexuales y reproductivos`, `logro17_leer y escribir`,
                                                                                `logro18_estudios postsecundarios`, `logro19_herramientas digitales`, `logro20_educación financiera`, `logro21_no pisos en tierra`, `logro22_paredes adecuadas`, `logro23_no hacinamiento`, `logro24_ingresos adultos mayores`, `logro25_actividad productiva`,
                                                                                `logro26_seguridad juridica del predio`, `logro27_familias en acción`, `logro28_jovenes en acción`,
                                                                                logro01_I_identificación, `logro02_I_afiliación a salud`, logro03_I_vacunación,
                                                                                `logro04_I_atencion a menores en riesgo de desnutrición`,
                                                                                `logro05_I_control de crecimiento`, `logro06_I_educación inicial`, logro07_I_escolarización, `logro08_I_no trabajo infantil`,
                                                                                `logro09_I_acceso a agua`, `logro10_I_saneamiento básico`, `logro11_I_ingresos suficientes`, `logro13_I_registro de personas con discapacidad`,
                                                                                `logro14_I_elementos de apoyo y rehabilitación`, `logro15_I_seguridad alimentaria`, `logro16_I_derechos sexuales y reproductivos`, `logro17_I_leer y escribir`,
                                                                                `logro18_I_estudios postsecundarios`, `logro20_I_educación financiera`, `logro21_I_no pisos en tierra`, `logro22_I_paredes adecuadas`, `logro23_I_no hacinamiento`,
                                                                                `logro24_I_ingresos adultos mayores`, `logro25_I_actividad productiva`, `logro26_I_seguridad juridica del predio`, `logro27_I_familias en acción`, `logro28_I_jovenes en acción`,
                                                                                fechaCalculoLogros, denominacionIPM, fechaCalculoIPM)


BaseGestion = merge(BaseGestion, Base_Gestion_2021[, c("idHogar", "idIntegranteHogar", "CodigoDepartamento", "Departamento", "CodigoMunicipio", "Municipio", "Zona", "Telefono Celular", "EdadCargue")],
                    by = c("idHogar", "idIntegranteHogar"), all.x = T)


BaseGestion = BaseGestion  %>%  select(idHogar, EstadoHogar, idIntegranteHogar, CodigoDepartamento, Departamento, CodigoMunicipio, Municipio, Zona, `Telefono Celular`, EdadCargue,
                                       
                                       `logro02_afiliación a salud`,
                                       `logro06_educación inicial`, 
                                       logro07_escolarización, 
                                       `logro08_no trabajo infantil`, 
                                       `logro09_acceso a agua`,
                                       `logro10_saneamiento básico`, 
                                       `logro17_leer y escribir`, 
                                       `logro18_estudios postsecundarios`,
                                       `logro21_no pisos en tierra`,
                                       `logro22_paredes adecuadas`,
                                       `logro23_no hacinamiento`,
                                       `logro25_actividad productiva`, 
                                       `logro27_familias en acción`,
                                       
                                       `logro02_I_afiliación a salud`,
                                       `logro06_I_educación inicial`, 
                                       logro07_I_escolarización, 
                                       `logro08_I_no trabajo infantil`, 
                                       `logro09_I_acceso a agua`,
                                       `logro10_I_saneamiento básico`, 
                                       `logro17_I_leer y escribir`, 
                                       `logro18_I_estudios postsecundarios`, 
                                       `logro21_I_no pisos en tierra`, 
                                       `logro22_I_paredes adecuadas`, 
                                       `logro23_I_no hacinamiento`, 
                                       `logro25_I_actividad productiva`,
                                       `logro27_I_familias en acción`,
                                       
                                       fechaCalculoLogros, denominacionIPM, fechaCalculoIPM)


view(dfSummary(BaseGestion))

# Corte certificado
setwd(paste(Salidas,"Cortes Certificados", sep = slash))
write.table(BaseGestion, file = "BaseGestion Hogares Acomp Diciembre 2021.txt", sep = ";", row.names = F)


#### Frecuencias ####

setwd(Entradas)

BaseGestion_Hogares_Acompa_2021 = BaseGestion

Precargue = read_delim("Unidos_Sabana_20211027.txt", delim = "|", escape_double = FALSE, col_types = cols(IdIntegrante = col_number(),
                                                                                                          A03 = col_character(),
                                                                                                          A03_1 = col_character(),
                                                                                                          E03 = col_number(),
                                                                                                          F01_1 = col_number(),
                                                                                                          F01_2 = col_number(),
                                                                                                          F01_3 = col_number(),
                                                                                                          F01_4 = col_number(),
                                                                                                          F01_5 = col_number(),
                                                                                                          F01_6 = col_number(),
                                                                                                          F01_7 = col_number(),
                                                                                                          F01_8 = col_number()), locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)

setwd(Carpeta)
source("Ciclo vital.R", encoding = "UTF-8")

#BaseGestion_Hogares_Acompa_2021$E02_1 = BaseGestion_Hogares_Acompa_2021$EdadCargue
#BaseGestion_Hogares_Acompa_2021$CicloVital = Ciclo(BaseGestion_Hogares_Acompa_2021, E02_1)

BaseGestion_Hogares_Acompa_2021 = BaseGestion_Hogares_Acompa_2021 %>% mutate(CicloVital = case_when(EdadCargue <= 5 ~ "1. Primera Infancia",
                                                                                                    between(EdadCargue, 6, 11) ~ "2. Niñez",
                                                                                                    between(EdadCargue, 12, 17) ~ "3. Adolescencia",
                                                                                                    between(EdadCargue, 18, 24) ~ "4. Juventud",
                                                                                                    between(EdadCargue, 25, 59) ~ "5. Adulto",
                                                                                                    EdadCargue > 59 ~ "6. Adulto Mayor",
                                                                                                    TRUE ~ "NA"))


setwd(Entradas) 

BaseGestion_Hogares_Acompa_2021 = merge(BaseGestion_Hogares_Acompa_2021, Precargue[, c("IdIntegrante", "E03", "F01_1", "F01_2", "F01_3", "F01_4", "F01_5", "F01_6", "F01_7", "F01_8")], 
                                        by.x = "idIntegranteHogar", by.y = "IdIntegrante", all.x = T)


SEXO = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, E03) %>% pivot_wider(names_from  = E03,
                                                                                                                          values_from = E03,
                                                                                                                          values_fill = 0,
                                                                                                                          values_fn   = list(E03 = length))

SEXO = SEXO %>% rename(SEXO_HOMBRE = "1",
                       SEXO_MUJER  = "2")


DiscapacidadVer = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, F01_1) %>% pivot_wider(names_from  = F01_1,
                                                                                                                                       values_from = F01_1,
                                                                                                                                       values_fill = 0,
                                                                                                                                       values_fn   = list(F01_1 = length))
DiscapacidadVer = DiscapacidadVer %>% rename(DISCAPACIDAD_VER_SI = "1",
                                             DISCAPACIDAD_VER_NO = "2",
                                             DISCAPACIDAD_VER_SIN_DATO = "0")

DiscapacidadOir = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, F01_2) %>% pivot_wider(names_from  = F01_2,
                                                                                                                                       values_from = F01_2,
                                                                                                                                       values_fill = 0,
                                                                                                                                       values_fn   = list(F01_2 = length))


DiscapacidadOir = DiscapacidadOir %>% rename(DISCAPACIDAD_OIR_SI = "1",
                                             DISCAPACIDAD_OIR_NO = "2",
                                             DISCAPACIDAD_OIR_SIN_DATO = "0")



DiscapacidadHablar = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, F01_3) %>% pivot_wider(names_from  = F01_3,
                                                                                                                                          values_from = F01_3,
                                                                                                                                          values_fill = 0,
                                                                                                                                          values_fn   = list(F01_3 = length))
DiscapacidadHablar = DiscapacidadHablar %>% rename(DISCAPACIDAD_HABLAR_SI = "1",
                                                   DISCAPACIDAD_HABLAR_NO = "2",
                                                   DISCAPACIDAD_HABLAR_SIN_DATO = "0")



DiscapacidadMoverse = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, F01_4) %>% pivot_wider(names_from  = F01_4,
                                                                                                                                           values_from = F01_4,
                                                                                                                                           values_fill = 0,
                                                                                                                                           values_fn   = list(F01_4 = length))
DiscapacidadMoverse = DiscapacidadMoverse %>% rename(DISCAPACIDAD_MOVERSE_SI = "1",
                                                     DISCAPACIDAD_MOVERSE_NO = "2",
                                                     DISCAPACIDAD_MOVERSE_SIN_DATO = "0")



DiscapacidadBañarse = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, F01_5) %>% pivot_wider(names_from  = F01_5,
                                                                                                                                           values_from = F01_5,
                                                                                                                                           values_fill = 0,
                                                                                                                                           values_fn   = list(F01_5 = length))
DiscapacidadBañarse = DiscapacidadBañarse %>% rename(DISCAPACIDAD_BañaRSE_VESTIRSE_SI = "1",
                                                     DISCAPACIDAD_BañaRSE_VESTIRSE_NO = "2",
                                                     DISCAPACIDAD_BañaRSE_VESTIRSE_SIN_DATO = "0")


DiscapacidadSalirCalle = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, F01_6) %>% pivot_wider(names_from  = F01_6,
                                                                                                                                              values_from = F01_6,
                                                                                                                                              values_fill = 0,
                                                                                                                                              values_fn   = list(F01_6 = length))
DiscapacidadSalirCalle = DiscapacidadSalirCalle %>% rename(DISCAPACIDAD_SALIR_A_LA_CALLE_SI = "1",
                                                           DISCAPACIDAD_SALIR_A_LA_CALLE_NO = "2",
                                                           DISCAPACIDAD_SALIR_A_LA_CALLE_SIN_DATO = "0")




DiscapacidadEntender = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, F01_7) %>% pivot_wider(names_from  = F01_7,
                                                                                                                                            values_from = F01_7,
                                                                                                                                            values_fill = 0,
                                                                                                                                            values_fn   = list(F01_7 = length))
DiscapacidadEntender = DiscapacidadEntender %>% rename(DISCAPACIDAD_ENTENDER_APRENDER_SI = "1",
                                                       DISCAPACIDAD_ENTENDER_APRENDER_NO = "2",
                                                       DISCAPACIDAD_ENTENDER_APRENDER_SIN_DATO = "0")




Discapacidad = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, F01_8) %>% pivot_wider(names_from  = F01_8,
                                                                                                                                    values_from = F01_8,
                                                                                                                                    values_fill = 0,
                                                                                                                                    values_fn   = list(F01_8 = length))
Discapacidad = Discapacidad %>% rename(DISCAPACIDAD_SI = "2",
                                       DISCAPACIDAD_NO = "1",
                                       DISCAPACIDAD_SIN_DATO = "0")



ZONA = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, idHogar, Zona) 
ZONA = reshape2::dcast(data = ZONA[!duplicated(paste(ZONA$CodigoMunicipio, ZONA$idHogar)), ],
                       CodigoMunicipio ~ Zona,
                       fun.aggregate = length,
                       value.var = "Zona")


TOTAL_INTEGRANTES = BaseGestion_Hogares_Acompa_2021 %>% group_by(CodigoMunicipio) %>% summarise(TOTAL_INTEGRANTES = length(idIntegranteHogar))                          

TOTAL_HOGARES     = BaseGestion_Hogares_Acompa_2021 %>% group_by(CodigoMunicipio) %>% summarise(TOTAL_HOGARES = length(unique(idHogar)))  



CICLO_VITAL = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, CicloVital) %>% 
  pivot_wider(names_from  = CicloVital,
              values_from = CicloVital,
              values_fill = 0,
              values_fn   = list(CicloVital = length))


####  LOGRO 02 AFILIACION A SALUD ####

L02_AFILIACION_SALUD = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, idHogar, `logro02_afiliación a salud`)

L02_AFILIACION_SALUD = reshape2::dcast(data = L02_AFILIACION_SALUD[!duplicated(paste(L02_AFILIACION_SALUD$CodigoMunicipio, L02_AFILIACION_SALUD$idHogar)), ],
                                       CodigoMunicipio ~ `logro02_afiliación a salud`,
                                       fun.aggregate = length,
                                       value.var = "logro02_afiliación a salud")


L02_AFILIACION_SALUD = rename_all(L02_AFILIACION_SALUD, recode, ALCANZADO = "L02_AFILIACION_SALUD_ALCANZADO",
                                  GESTIONADO = "L02_AFILIACION_SALUD_GESTIONADO",
                                  "POR ALCANZAR" = "L02_AFILIACION_SALUD_POR_ALCANZAR",
                                  "NO DETERMINADO" = "L02_AFILIACION_SALUD_NO_DETERMINADO",
                                  "NO APLICA" = "L02_AFILIACION_SALUD_NO_APLICA")


L02_I_AFILIACION_SALUD = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, `logro02_I_afiliación a salud`) %>% 
  pivot_wider(names_from  = `logro02_I_afiliación a salud`,
              values_from = `logro02_I_afiliación a salud`,
              values_fill = 0,
              values_fn   = list(`logro02_I_afiliación a salud` = length))

L02_I_AFILIACION_SALUD = rename_all(L02_I_AFILIACION_SALUD, recode, ALCANZADO = "L02_I_AFILIACION_SALUD_ALCANZADO",
                                    GESTIONADO = "L02_I_AFILIACION_SALUD_GESTIONADO",
                                    "POR ALCANZAR" = "L02_I_AFILIACION_SALUD_POR_ALCANZAR",
                                    "NO DETERMINADO" = "L02_I_AFILIACION_SALUD_NO_DETERMINADO",
                                    "NO APLICA" = "L02_I_AFILIACION_SALUD_NO_APLICA")



#### LOGRO 06 EDUCACION INICIAL ####

L06_EDUCACION_INICIAL = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, idHogar, `logro06_educación inicial`)
L06_EDUCACION_INICIAL = reshape2::dcast(data = L06_EDUCACION_INICIAL[!duplicated(paste(L06_EDUCACION_INICIAL$CodigoMunicipio, L06_EDUCACION_INICIAL$idHogar)), ],
                                        CodigoMunicipio ~ `logro06_educación inicial`,
                                        fun.aggregate = length,
                                        value.var = "logro06_educación inicial")

L06_EDUCACION_INICIAL = rename_all(L06_EDUCACION_INICIAL, recode, ALCANZADO = "L06_EDUCACION_INICIAL_ALCANZADO",
                                   GESTIONADO = "L06_EDUCACION_INICIAL_GESTIONADO",
                                   "POR ALCANZAR" = "L06_EDUCACION_INICIAL_POR_ALCANZAR",
                                   "NO DETERMINADO" = "L06_EDUCACION_INICIAL_NO_DETERMINADO",
                                   "NO APLICA" = "L06_EDUCACION_INICIAL_NO_APLICA")


L06_I_EDUCACION_INICIAL = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, `logro06_I_educación inicial`) %>% 
  pivot_wider(names_from  = `logro06_I_educación inicial`,
              values_from = `logro06_I_educación inicial`,
              values_fill = 0,
              values_fn   = list(`logro06_I_educación inicial` = length))


L06_I_EDUCACION_INICIAL = rename_all(L06_I_EDUCACION_INICIAL, recode, ALCANZADO = "L06_I_EDUCACION_INICIAL_ALCANZADO",
                                     GESTIONADO = "L06_I_EDUCACION_INICIAL_GESTIONADO",
                                     "POR ALCANZAR" = "L06_I_EDUCACION_INICIAL_POR_ALCANZAR",
                                     "NO DETERMINADO" = "L06_I_EDUCACION_INICIAL_NO_DETERMINADO",
                                     "NO APLICA" = "L06_I_EDUCACION_INICIAL_NO_APLICA")


#### LOGRO 07 ESCOLARIZACION ####

L07_I_ESCOLARIZACION = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, logro07_I_escolarización) %>% pivot_wider(names_from  = logro07_I_escolarización,
                                                                                                                                                               values_from = logro07_I_escolarización,
                                                                                                                                                               values_fill = 0,
                                                                                                                                                               values_fn   = list(logro07_I_escolarización = length))

L07_I_ESCOLARIZACION = rename_all(L07_I_ESCOLARIZACION, recode, ALCANZADO = "L07_I_ESCOLARIZACION_ALCANZADO",
                                  GESTIONADO = "L07_I_ESCOLARIZACION_GESTIONADO",
                                  "POR ALCANZAR" = "L07_I_ESCOLARIZACION_POR_ALCANZAR",
                                  "NO DETERMINADO" = "L07_I_ESCOLARIZACION_NO_DETERMINADO",
                                  "NO APLICA" = "L07_I_ESCOLARIZACION_NO_APLICA")


L07_ESCOLARIZACION = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, idHogar, logro07_escolarización)

L07_ESCOLARIZACION = reshape2::dcast(data = L07_ESCOLARIZACION[!duplicated(paste(L07_ESCOLARIZACION$CodigoMunicipio, L07_ESCOLARIZACION$idHogar)), ],
                                     CodigoMunicipio ~ logro07_escolarización,
                                     fun.aggregate = length,
                                     value.var = "logro07_escolarización")

L07_ESCOLARIZACION = rename_all(L07_ESCOLARIZACION, recode, ALCANZADO = "L07_ESCOLARIZACION_ALCANZADO",
                                GESTIONADO = "L07_ESCOLARIZACION_GESTIONADO",
                                "POR ALCANZAR" = "L07_ESCOLARIZACION_POR_ALCANZAR",
                                "NO DETERMINADO" = "L07_ESCOLARIZACION_NO_DETERMINADO",
                                "NO APLICA" = "L07_ESCOLARIZACION_NO_APLICA")


#### LOGRO 08 NO TRABAJO INFANTIL ####

L08_NO_TRABAJO_INFANTIL = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, idHogar, `logro08_no trabajo infantil`)

L08_NO_TRABAJO_INFANTIL = reshape2::dcast(data = L08_NO_TRABAJO_INFANTIL[!duplicated(paste(L08_NO_TRABAJO_INFANTIL$CodigoMunicipio, L08_NO_TRABAJO_INFANTIL$idHogar)), ],
                                          CodigoMunicipio ~ `logro08_no trabajo infantil`,
                                          fun.aggregate = length,
                                          value.var = "logro08_no trabajo infantil")

L08_NO_TRABAJO_INFANTIL = rename_all(L08_NO_TRABAJO_INFANTIL, recode, ALCANZADO = "L08_NO_TRABAJO_INFANTIL_ALCANZADO",
                                     GESTIONADO = "L08_NO_TRABAJO_INFANTIL_GESTIONADO",
                                     "POR ALCANZAR" = "L08_NO_TRABAJO_INFANTIL_POR_ALCANZAR",
                                     "NO DETERMINADO" = "L08_NO_TRABAJO_INFANTIL_NO_DETERMINADO",
                                     "NO APLICA" = "L08_NO_TRABAJO_INFANTIL_NO_APLICA")

L08_I_NO_TRABAJO_INFANTIL = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, `logro08_I_no trabajo infantil`) %>% pivot_wider(names_from  = `logro08_I_no trabajo infantil`,
                                                                                                                                                                           values_from = `logro08_I_no trabajo infantil`,
                                                                                                                                                                           values_fill = 0,
                                                                                                                                                                           values_fn   = list(`logro08_I_no trabajo infantil` = length))

L08_I_NO_TRABAJO_INFANTIL = rename_all(L08_I_NO_TRABAJO_INFANTIL, recode, ALCANZADO = "L08_I_NO_TRABAJO_INFANTIL_ALCANZADO",
                                       GESTIONADO = "L08_I_NO_TRABAJO_INFANTIL_GESTIONADO",
                                       "POR ALCANZAR" = "L08_I_NO_TRABAJO_INFANTIL_POR_ALCANZAR",
                                       "NO DETERMINADO" = "L08_I_NO_TRABAJO_INFANTIL_NO_DETERMINADO",
                                       "NO APLICA" = "L08_I_NO_TRABAJO_INFANTIL_NO_APLICA")


#### LOGRO 09 ACCESO A AGUA ####

L09_I_ACCESO_A_AGUA = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, `logro09_I_acceso a agua`) %>% pivot_wider(names_from  = `logro09_I_acceso a agua`,
                                                                                                                                                               values_from = `logro09_I_acceso a agua`,
                                                                                                                                                               values_fill = 0,
                                                                                                                                                               values_fn   = list(`logro09_I_acceso a agua` = length))


L09_I_ACCESO_A_AGUA = rename_all(L09_I_ACCESO_A_AGUA, recode, ALCANZADO = "L09_I_ACCESO_A_AGUA_ALCANZADO",
                                 GESTIONADO = "L09_I_ACCESO_A_AGUA_GESTIONADO",
                                 "POR ALCANZAR" = "L09_I_ACCESO_A_AGUA_POR_ALCANZAR",
                                 "NO DETERMINADO" = "L09_I_ACCESO_A_AGUA_NO_DETERMINADO",
                                 "NO APLICA" = "L09_I_ACCESO_A_AGUA_NO_APLICA")



L09_ACCESO_A_AGUA = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, idHogar, `logro09_acceso a agua`)

L09_ACCESO_A_AGUA = reshape2::dcast(data = L09_ACCESO_A_AGUA[!duplicated(paste(L09_ACCESO_A_AGUA$CodigoMunicipio, L09_ACCESO_A_AGUA$idHogar)), ],
                                    CodigoMunicipio ~ `logro09_acceso a agua`,
                                    fun.aggregate = length,
                                    value.var = "logro09_acceso a agua")

L09_ACCESO_A_AGUA = rename_all(L09_ACCESO_A_AGUA, recode, ALCANZADO = "L09_ACCESO_A_AGUA_ALCANZADO",
                               GESTIONADO = "L09_ACCESO_A_AGUA_GESTIONADO",
                               "POR ALCANZAR" = "L09_ACCESO_A_AGUA_POR_ALCANZAR",
                               "NO DETERMINADO" = "L09_ACCESO_A_AGUA_NO_DETERMINADO",
                               "NO APLICA" = "L09_ACCESO_A_AGUA_NO_APLICA")

#### LOGRO 10 SANEAMIENTO BASICO ####


L10_I_SANEAMIENTO_BASICO = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, `logro10_I_saneamiento básico`) %>% pivot_wider(names_from  = `logro10_I_saneamiento básico`,
                                                                                                                                                                         values_from = `logro10_I_saneamiento básico`,
                                                                                                                                                                         values_fill = 0,
                                                                                                                                                                         values_fn   = list(`logro10_I_saneamiento básico` = length))
L10_I_SANEAMIENTO_BASICO = rename_all(L10_I_SANEAMIENTO_BASICO, recode, ALCANZADO = "L10_I_SANEAMIENTO_BASICO_ALCANZADO",
                                      GESTIONADO = "L10_I_SANEAMIENTO_BASICO_GESTIONADO",
                                      "POR ALCANZAR" = "L10_I_SANEAMIENTO_BASICO_POR_ALCANZAR",
                                      "NO DETERMINADO" = "L10_I_SANEAMIENTO_BASICO_NO_DETERMINADO",
                                      "NO APLICA" = "L10_I_SANEAMIENTO_BASICO_NO_APLICA")


L10_SANEAMIENTO_BASICO = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, idHogar,`logro10_saneamiento básico`)


L10_SANEAMIENTO_BASICO = reshape2::dcast(data = L10_SANEAMIENTO_BASICO[!duplicated(paste(L10_SANEAMIENTO_BASICO$CodigoMunicipio, L10_SANEAMIENTO_BASICO$idHogar)), ],
                                         CodigoMunicipio ~ `logro10_saneamiento básico`,
                                         fun.aggregate = length,
                                         value.var = "logro10_saneamiento básico")


L10_SANEAMIENTO_BASICO = rename_all(L10_SANEAMIENTO_BASICO, recode, ALCANZADO = "L10_SANEAMIENTO_BASICO_ALCANZADO",
                                    GESTIONADO = "L10_SANEAMIENTO_BASICO_GESTIONADO",
                                    "POR ALCANZAR" = "L10_SANEAMIENTO_BASICO_POR_ALCANZAR",
                                    "NO DETERMINADO" = "L10_SANEAMIENTO_BASICO_NO_DETERMINADO",
                                    "NO APLICA" = "L10_SANEAMIENTO_BASICO_NO_APLICA")


#### LOGRO 17 LEER Y ESCRIBIR ####


L17_LEER_ESCRIBIR = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, idHogar, `logro17_leer y escribir`)

L17_LEER_ESCRIBIR = reshape2::dcast(data = L17_LEER_ESCRIBIR[!duplicated(paste(L17_LEER_ESCRIBIR$CodigoMunicipio, L17_LEER_ESCRIBIR$idHogar)), ],
                                    CodigoMunicipio ~ `logro17_leer y escribir`,
                                    fun.aggregate = length,
                                    value.var = "logro17_leer y escribir")

L17_LEER_ESCRIBIR = rename_all(L17_LEER_ESCRIBIR, recode, ALCANZADO = "L17_LEER_ESCRIBIR_ALCANZADO",
                               GESTIONADO = "L17_LEER_ESCRIBIR_GESTIONADO",
                               "POR ALCANZAR" = "L17_LEER_ESCRIBIR_POR_ALCANZAR",
                               "NO DETERMINADO" = "L17_LEER_ESCRIBIR_NO_DETERMINADO",
                               "NO APLICA" = "L17_LEER_ESCRIBIR_NO_APLICA")

L17_I_LEER_ESCRIBIR = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, `logro17_I_leer y escribir`) %>% pivot_wider(names_from  = `logro17_I_leer y escribir`,
                                                                                                                                                                 values_from = `logro17_I_leer y escribir`,
                                                                                                                                                                 values_fill = 0,
                                                                                                                                                                 values_fn   = list(`logro17_I_leer y escribir` = length))

L17_I_LEER_ESCRIBIR = rename_all(L17_I_LEER_ESCRIBIR, recode, ALCANZADO = "L17_I_LEER_ESCRIBIR_ALCANZADO",
                                 GESTIONADO = "L17_I_LEER_ESCRIBIR_GESTIONADO",
                                 "POR ALCANZAR" = "L17_I_LEER_ESCRIBIR_POR_ALCANZAR",
                                 "NO DETERMINADO" = "L17_I_LEER_ESCRIBIR_NO_DETERMINADO",
                                 "NO APLICA" = "L17_I_LEER_ESCRIBIR_NO_APLICA")


#### LOGRO 18 ESTUDIOS POST SECUNDARIOS ####


L18_ESTUDIOS_POSTSECUNDARIOS = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio,  idHogar, `logro18_estudios postsecundarios`)

L18_ESTUDIOS_POSTSECUNDARIOS = reshape2::dcast(data = L18_ESTUDIOS_POSTSECUNDARIOS[!duplicated(paste(L18_ESTUDIOS_POSTSECUNDARIOS$CodigoMunicipio, L18_ESTUDIOS_POSTSECUNDARIOS$idHogar)), ],
                                               CodigoMunicipio ~ `logro18_estudios postsecundarios`,
                                               fun.aggregate = length,
                                               value.var = "logro18_estudios postsecundarios")

L18_ESTUDIOS_POSTSECUNDARIOS = rename_all(L18_ESTUDIOS_POSTSECUNDARIOS, recode, ALCANZADO = "L18_ESTUDIOS_POSTSECUNDARIOS_ALCANZADO",
                                          GESTIONADO = "L18_ESTUDIOS_POSTSECUNDARIOS_GESTIONADO",
                                          "POR ALCANZAR" = "L18_ESTUDIOS_POSTSECUNDARIOS_POR_ALCANZAR",
                                          "NO DETERMINADO" = "L18_ESTUDIOS_POSTSECUNDARIOS_NO_DETERMINADO",
                                          "NO APLICA" = "L18_ESTUDIOS_POSTSECUNDARIOS_NO_APLICA")



L18_I_ESTUDIOS_POSTSECUNDARIOS = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, `logro18_I_estudios postsecundarios`) %>% pivot_wider(names_from  = `logro18_I_estudios postsecundarios`,
                                                                                                                                                                                     values_from = `logro18_I_estudios postsecundarios`,
                                                                                                                                                                                     values_fill = 0,
                                                                                                                                                                                     values_fn   = list(`logro18_I_estudios postsecundarios` = length))


L18_I_ESTUDIOS_POSTSECUNDARIOS = rename_all(L18_I_ESTUDIOS_POSTSECUNDARIOS, recode, ALCANZADO = "L18_I_ESTUDIOS_POSTSECUNDARIOS_ALCANZADO",
                                            GESTIONADO = "L18_I_ESTUDIOS_POSTSECUNDARIOS_GESTIONADO",
                                            "POR ALCANZAR" = "L18_I_ESTUDIOS_POSTSECUNDARIOS_POR_ALCANZAR",
                                            "NO DETERMINADO" = "L18_I_ESTUDIOS_POSTSECUNDARIOS_NO_DETERMINADO",
                                            "NO APLICA" = "L18_I_ESTUDIOS_POSTSECUNDARIOS_NO_APLICA")

#### LOGRO 21 NO PISOS EN TIERRA ####

L21_NO_PISOS_TIERRA = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, idHogar, `logro21_no pisos en tierra`)

L21_NO_PISOS_TIERRA = reshape2::dcast(data = L21_NO_PISOS_TIERRA[!duplicated(paste(L21_NO_PISOS_TIERRA$CodigoMunicipio, L21_NO_PISOS_TIERRA$idHogar)), ],
                                      CodigoMunicipio ~ `logro21_no pisos en tierra`,
                                      fun.aggregate = length,
                                      value.var = "logro21_no pisos en tierra")


L21_NO_PISOS_TIERRA = rename_all(L21_NO_PISOS_TIERRA, recode, ALCANZADO = "L21_NO_PISOS_TIERRA_ALCANZADO",
                                 GESTIONADO = "L21_NO_PISOS_TIERRA_GESTIONADO",
                                 "POR ALCANZAR" = "L21_NO_PISOS_TIERRA_POR_ALCANZAR",
                                 "NO DETERMINADO" = "L21_NO_PISOS_TIERRA_NO_DETERMINADO",
                                 "NO APLICA" = "L21_NO_PISOS_TIERRA_NO_APLICA")


L21_I_NO_PISOS_TIERRA = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, `logro21_I_no pisos en tierra`) %>% pivot_wider(names_from  = `logro21_I_no pisos en tierra`,
                                                                                                                                                                      values_from = `logro21_I_no pisos en tierra`,
                                                                                                                                                                      values_fill = 0,
                                                                                                                                                                      values_fn   = list(`logro21_I_no pisos en tierra` = length))

L21_I_NO_PISOS_TIERRA = rename_all(L21_I_NO_PISOS_TIERRA, recode, ALCANZADO = "L21_I_NO_PISOS_TIERRA_ALCANZADO",
                                   GESTIONADO = "L21_I_NO_PISOS_TIERRA_GESTIONADO",
                                   "POR ALCANZAR" = "L21_I_NO_PISOS_TIERRA_POR_ALCANZAR",
                                   "NO DETERMINADO" = "L21_I_NO_PISOS_TIERRA_NO_DETERMINADO",
                                   "NO APLICA" = "L21_I_NO_PISOS_TIERRA_NO_APLICA")


#### LOGRO 22 PAREDES ADECUADAS ####

L22_I_PAREDES_ADECUADAS = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, `logro22_I_paredes adecuadas`) %>% pivot_wider(names_from  = `logro22_I_paredes adecuadas`,
                                                                                                                                                                       values_from = `logro22_I_paredes adecuadas`,
                                                                                                                                                                       values_fill = 0,
                                                                                                                                                                       values_fn   = list(`logro22_I_paredes adecuadas` = length))


L22_I_PAREDES_ADECUADAS = rename_all(L22_I_PAREDES_ADECUADAS, recode, ALCANZADO = "L22_I_PAREDES_ADECUADAS_ALCANZADO",
                                     GESTIONADO = "L22_I_PAREDES_ADECUADAS_GESTIONADO",
                                     "POR ALCANZAR" = "L22_I_PAREDES_ADECUADAS_POR_ALCANZAR",
                                     "NO DETERMINADO" = "L22_I_PAREDES_ADECUADAS_NO_DETERMINADO",
                                     "NO APLICA" = "L22_I_PAREDES_ADECUADAS_NO_APLICA")



L22_PAREDES_ADECUADAS = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, idHogar, `logro22_paredes adecuadas`)

L22_PAREDES_ADECUADAS = reshape2::dcast(data = L22_PAREDES_ADECUADAS[!duplicated(paste(L22_PAREDES_ADECUADAS$CodigoMunicipio, L22_PAREDES_ADECUADAS$idHogar)), ],
                                        CodigoMunicipio ~ `logro22_paredes adecuadas`,
                                        fun.aggregate = length,
                                        value.var = "logro22_paredes adecuadas")

L22_PAREDES_ADECUADAS = rename_all(L22_PAREDES_ADECUADAS, recode, ALCANZADO = "L22_PAREDES_ADECUADAS_ALCANZADO",
                                   GESTIONADO = "L22_PAREDES_ADECUADAS_GESTIONADO",
                                   "POR ALCANZAR" = "L22_PAREDES_ADECUADAS_POR_ALCANZAR",
                                   "NO DETERMINADO" = "L22_PAREDES_ADECUADAS_NO_DETERMINADO",
                                   "NO APLICA" = "L22_PAREDES_ADECUADAS_NO_APLICA")


#### LOGRO 23 NO HACINAMIENTO ####

L23_I_NO_HACINAMIENTO = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, `logro23_I_no hacinamiento`) %>% pivot_wider(names_from  = `logro23_I_no hacinamiento`,
                                                                                                                                                                   values_from = `logro23_I_no hacinamiento`,
                                                                                                                                                                   values_fill = 0,
                                                                                                                                                                   values_fn   = list(`logro23_I_no hacinamiento` = length))


L23_I_NO_HACINAMIENTO = rename_all(L23_I_NO_HACINAMIENTO, recode, ALCANZADO = "L23_I_NO_HACINAMIENTO_ALCANZADO",
                                   GESTIONADO = "L23_I_NO_HACINAMIENTO_GESTIONADO",
                                   "POR ALCANZAR" = "L23_I_NO_HACINAMIENTO_POR_ALCANZAR",
                                   "NO DETERMINADO" = "L23_I_NO_HACINAMIENTO_NO_DETERMINADO",
                                   "NO APLICA" = "L23_I_NO_HACINAMIENTO_NO_APLICA")


L23_NO_HACINAMIENTO = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, idHogar, `logro23_no hacinamiento`) 
L23_NO_HACINAMIENTO = reshape2::dcast(data = L23_NO_HACINAMIENTO[!duplicated(paste(L23_NO_HACINAMIENTO$CodigoMunicipio, L23_NO_HACINAMIENTO$idHogar)), ],
                                      CodigoMunicipio ~ `logro23_no hacinamiento`,
                                      fun.aggregate = length,
                                      value.var = "logro23_no hacinamiento")

L23_NO_HACINAMIENTO = rename_all(L23_NO_HACINAMIENTO, recode, ALCANZADO = "L23_NO_HACINAMIENTO_ALCANZADO",
                                 GESTIONADO = "L23_NO_HACINAMIENTO_GESTIONADO",
                                 "POR ALCANZAR" = "L23_NO_HACINAMIENTO_POR_ALCANZAR",
                                 "NO DETERMINADO" = "L23_NO_HACINAMIENTO_NO_DETERMINADO",
                                 "NO APLICA" = "L23_NO_HACINAMIENTO_NO_APLICA")


#### LOGRO 25 ACTIVIDAD PRODUCTIVA ####


L25_I_ACTIVIDAD_PRODUCTIVA = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, `logro25_I_actividad productiva`) %>% pivot_wider(names_from  = `logro25_I_actividad productiva`,
                                                                                                                                                                             values_from = `logro25_I_actividad productiva`,
                                                                                                                                                                             values_fill = 0,
                                                                                                                                                                             values_fn   = list(`logro25_I_actividad productiva` = length))

L25_I_ACTIVIDAD_PRODUCTIVA = rename_all(L25_I_ACTIVIDAD_PRODUCTIVA, recode, ALCANZADO = "L25_I_ACTIVIDAD_PRODUCTIVA_ALCANZADO",
                                        GESTIONADO = "L25_I_ACTIVIDAD_PRODUCTIVA_GESTIONADO",
                                        "POR ALCANZAR" = "L25_I_ACTIVIDAD_PRODUCTIVA_POR_ALCANZAR",
                                        "NO DETERMINADO" = "L25_I_ACTIVIDAD_PRODUCTIVA_NO_DETERMINADO",
                                        "NO APLICA" = "L25_I_ACTIVIDAD_PRODUCTIVA_NO_APLICA")


L25_ACTIVIDAD_PRODUCTIVA = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, idHogar, `logro25_actividad productiva`)

L25_ACTIVIDAD_PRODUCTIVA = reshape2::dcast(data = L25_ACTIVIDAD_PRODUCTIVA[!duplicated(paste(L25_ACTIVIDAD_PRODUCTIVA$CodigoMunicipio, L25_ACTIVIDAD_PRODUCTIVA$idHogar)), ],
                                           CodigoMunicipio ~ `logro25_actividad productiva`,
                                           fun.aggregate = length,
                                           value.var = "logro25_actividad productiva")

L25_ACTIVIDAD_PRODUCTIVA = rename_all(L25_ACTIVIDAD_PRODUCTIVA, recode, ALCANZADO = "L25_ACTIVIDAD_PRODUCTIVA_ALCANZADO",
                                      GESTIONADO = "L25_ACTIVIDAD_PRODUCTIVA_GESTIONADO",
                                      "POR ALCANZAR" = "L25_ACTIVIDAD_PRODUCTIVA_POR_ALCANZAR",
                                      "NO DETERMINADO" = "L25_ACTIVIDAD_PRODUCTIVA_NO_DETERMINADO",
                                      "NO APLICA" = "L25_ACTIVIDAD_PRODUCTIVA_NO_APLICA")

#### LOGRO 27 MFA ####

L27_I_MFA = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, `logro27_I_familias en acción`) %>% pivot_wider(names_from  = `logro27_I_familias en acción`,
                                                                                                                                                          values_from = `logro27_I_familias en acción`,
                                                                                                                                                          values_fill = 0,
                                                                                                                                                          values_fn   = list(`logro27_I_familias en acción` = length))

L27_I_MFA = rename_all(L27_I_MFA, recode, ALCANZADO = "L27_I_MFA_ALCANZADO",
                       GESTIONADO = "L27_I_MFA_GESTIONADO",
                       "POR ALCANZAR" = "L27_I_MFA_POR_ALCANZAR",
                       "NO DETERMINADO" = "L27_I_MFA_NO_DETERMINADO",
                       "NO APLICA" = "L27_I_MFA_NO_APLICA")

L27_MFA = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, idHogar, `logro27_familias en acción`)

L27_MFA = reshape2::dcast(data = L27_MFA[!duplicated(paste(L27_MFA$CodigoMunicipio, L27_MFA$idHogar)), ],
                          CodigoMunicipio ~ `logro27_familias en acción`,
                          fun.aggregate = length,
                          value.var = "logro27_familias en acción")

L27_MFA = rename_all(L27_MFA, recode, ALCANZADO = "L27_MFA_ALCANZADO",
                     GESTIONADO = "L27_MFA_GESTIONADO",
                     "POR ALCANZAR" = "L27_MFA_POR_ALCANZAR",
                     "NO DETERMINADO" = "L27_MFA_NO_DETERMINADO",
                     "NO APLICA" = "L27_MFA_NO_APLICA")

#### IPM ####


IPM = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio, idHogar, denominacionIPM)

IPM = reshape2::dcast(data = IPM[!duplicated(paste(IPM$CodigoMunicipio, IPM$idHogar)), ],
                      CodigoMunicipio ~ denominacionIPM,
                      fun.aggregate = length,
                      value.var = "denominacionIPM")


IPM = IPM %>% rename(IPM_POBRE = POBRE,
                     IPM_NO_POBRE = "NO POBRE",
                     IPM_NO_DETERMINADO = "NO DETERMINADO")

#### MUNICIPIOS ####

Municipios = Precargue %>% select(A03, A03_1) %>% distinct()

#### UNIFICAR DATA FRAMES ####


BaseGestionFrecuencias = BaseGestion_Hogares_Acompa_2021 %>% select(Departamento, CodigoDepartamento, CodigoMunicipio) %>% distinct()


BaseGestionFrecuencias = merge(BaseGestionFrecuencias, Municipios[, c("A03", "A03_1")], by.x =  "CodigoMunicipio", by.y = "A03_1", all.x = T)
BaseGestionFrecuencias = BaseGestionFrecuencias %>% rename(MUNICIPIO = A03)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, Discapacidad, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, DiscapacidadVer, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, DiscapacidadOir, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, DiscapacidadHablar, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, DiscapacidadMoverse, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, DiscapacidadBañarse, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, DiscapacidadSalirCalle, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, DiscapacidadEntender, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, SEXO, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, CICLO_VITAL, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)


BaseGestionFrecuencias = merge(BaseGestionFrecuencias, TOTAL_INTEGRANTES, by =  "CodigoMunicipio", all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, TOTAL_HOGARES,     by =  "CodigoMunicipio", all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, ZONA, by = "CodigoMunicipio", all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L02_AFILIACION_SALUD,   by =  "CodigoMunicipio", all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L02_I_AFILIACION_SALUD, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L06_EDUCACION_INICIAL,   by =  "CodigoMunicipio", all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L06_I_EDUCACION_INICIAL, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L07_ESCOLARIZACION,   by =  "CodigoMunicipio", all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L07_I_ESCOLARIZACION, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L08_NO_TRABAJO_INFANTIL,   by =  "CodigoMunicipio", all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L08_I_NO_TRABAJO_INFANTIL, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L09_ACCESO_A_AGUA,   by =  "CodigoMunicipio", all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L09_I_ACCESO_A_AGUA, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L10_SANEAMIENTO_BASICO,   by =  "CodigoMunicipio", all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L10_I_SANEAMIENTO_BASICO, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L17_LEER_ESCRIBIR,   by =  "CodigoMunicipio", all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L17_I_LEER_ESCRIBIR, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L18_ESTUDIOS_POSTSECUNDARIOS,   by =  "CodigoMunicipio", all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L18_I_ESTUDIOS_POSTSECUNDARIOS, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L21_NO_PISOS_TIERRA,   by =  "CodigoMunicipio", all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L21_I_NO_PISOS_TIERRA, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L22_PAREDES_ADECUADAS,   by =  "CodigoMunicipio", all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L22_I_PAREDES_ADECUADAS, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L23_NO_HACINAMIENTO,   by = "CodigoMunicipio", all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L23_I_NO_HACINAMIENTO, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L25_ACTIVIDAD_PRODUCTIVA,   by =  "CodigoMunicipio", all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L25_I_ACTIVIDAD_PRODUCTIVA, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L27_MFA,   by =  "CodigoMunicipio", all.x = T)
BaseGestionFrecuencias = merge(BaseGestionFrecuencias, L27_I_MFA, by = c("Departamento", "CodigoDepartamento", "CodigoMunicipio"), all.x = T)

BaseGestionFrecuencias = merge(BaseGestionFrecuencias, IPM, by =  "CodigoMunicipio", all.x = T)



BaseGestionFrecuencias = BaseGestionFrecuencias %>% 
  
  select(any_of(c("Departamento", "CodigoDepartamento", "CodigoMunicipio", "MUNICIPIO",
                  
                  "DISCAPACIDAD_SI", "DISCAPACIDAD_NO", "DISCAPACIDAD_SIN_DATO",
                  
                  "DISCAPACIDAD_VER_SI", "DISCAPACIDAD_OIR_SI", "DISCAPACIDAD_MOVERSE_SI", "DISCAPACIDAD_HABLAR_SI", "DISCAPACIDAD_BañaRSE_VESTIRSE_SI", "DISCAPACIDAD_SALIR_A_LA_CALLE_SI", "DISCAPACIDAD_ENTENDER_APRENDER_SI",
                  
                  "SEXO_HOMBRE", "SEXO_MUJER",
                  
                  "1. Primera Infancia", "2. Niñez", "3. Adolescencia", "4. Juventud", "5. Adulto", "6. Adulto Mayor",
                  
                  "TOTAL_INTEGRANTES",	"TOTAL_HOGARES",
                  
                  "Cabecera Municipal",	"Centro Poblado",	"Rural Disperso",
                  
                  
                  "L02_AFILIACION_SALUD_ALCANZADO", "L02_AFILIACION_SALUD_GESTIONADO", "L02_AFILIACION_SALUD_POR_ALCANZAR", "L02_AFILIACION_SALUD_NO_DETERMINADO", "L02_AFILIACION_SALUD_NO_APLICA",
                  "L02_I_AFILIACION_SALUD_ALCANZADO", "L02_I_AFILIACION_SALUD_GESTIONADO", "L02_I_AFILIACION_SALUD_POR_ALCANZAR", "L02_I_AFILIACION_SALUD_NO_DETERMINADO", "L02_I_AFILIACION_SALUD_NO_APLICA",
                  
                  "L06_EDUCACION_INICIAL_ALCANZADO", "L06_EDUCACION_INICIAL_GESTIONADO", "L06_EDUCACION_INICIAL_POR_ALCANZAR", "L06_EDUCACION_INICIAL_NO_DETERMINADO", "L06_EDUCACION_INICIAL_NO_APLICA",
                  "L06_I_EDUCACION_INICIAL_ALCANZADO", "L06_I_EDUCACION_INICIAL_GESTIONADO", "L06_I_EDUCACION_INICIAL_POR_ALCANZAR", "L06_I_EDUCACION_INICIAL_NO_DETERMINADO", "L06_I_EDUCACION_INICIAL_NO_APLICA",
                  
                  "L07_ESCOLARIZACION_ALCANZADO", "L07_ESCOLARIZACION_GESTIONADO", "L07_ESCOLARIZACION_POR_ALCANZAR", "L07_ESCOLARIZACION_NO_DETERMINADO", "L07_ESCOLARIZACION_NO_APLICA",
                  "L07_I_ESCOLARIZACION_ALCANZADO", "L07_I_ESCOLARIZACION_GESTIONADO", "L07_I_ESCOLARIZACION_POR_ALCANZAR", "L07_I_ESCOLARIZACION_NO_DETERMINADO", "L07_I_ESCOLARIZACION_NO_APLICA",              
                  
                  "L08_NO_TRABAJO_INFANTIL_ALCANZADO", "L08_NO_TRABAJO_INFANTIL_GESTIONADO", "L08_NO_TRABAJO_INFANTIL_POR_ALCANZAR", "L08_NO_TRABAJO_INFANTIL_NO_DETERMINADO", "L08_NO_TRABAJO_INFANTIL_NO_APLICA",
                  "L08_I_NO_TRABAJO_INFANTIL_ALCANZADO", "L08_I_NO_TRABAJO_INFANTIL_GESTIONADO", "L08_I_NO_TRABAJO_INFANTIL_POR_ALCANZAR", "L08_I_NO_TRABAJO_INFANTIL_NO_DETERMINADO", "L08_I_NO_TRABAJO_INFANTIL_NO_APLICA",              
                  
                  "L09_ACCESO_A_AGUA_ALCANZADO", "L09_ACCESO_A_AGUA_GESTIONADO", "L09_ACCESO_A_AGUA_POR_ALCANZAR", "L09_ACCESO_A_AGUA_NO_DETERMINADO", "L09_ACCESO_A_AGUA_NO_APLICA",
                  "L09_I_ACCESO_A_AGUA_ALCANZADO", "L09_I_ACCESO_A_AGUA_GESTIONADO", "L09_I_ACCESO_A_AGUA_POR_ALCANZAR", "L09_I_ACCESO_A_AGUA_NO_DETERMINADO", "L09_I_ACCESO_A_AGUA_NO_APLICA",              
                  
                  "L10_SANEAMIENTO_BASICO_ALCANZADO", "L10_SANEAMIENTO_BASICO_GESTIONADO", "L10_SANEAMIENTO_BASICO_POR_ALCANZAR", "L10_SANEAMIENTO_BASICO_NO_DETERMINADO", "L10_SANEAMIENTO_BASICO_NO_APLICA",
                  "L10_I_SANEAMIENTO_BASICO_ALCANZADO", "L10_I_SANEAMIENTO_BASICO_GESTIONADO", "L10_I_SANEAMIENTO_BASICO_POR_ALCANZAR", "L10_I_SANEAMIENTO_BASICO_NO_DETERMINADO", "L10_I_SANEAMIENTO_BASICO_NO_APLICA", 
                  
                  "L17_LEER_ESCRIBIR_ALCANZADO", "L17_LEER_ESCRIBIR_GESTIONADO", "L17_LEER_ESCRIBIR_POR_ALCANZAR", "L17_LEER_ESCRIBIR_NO_DETERMINADO", "L17_LEER_ESCRIBIR_NO_APLICA",
                  "L17_I_LEER_ESCRIBIR_ALCANZADO", "L17_I_LEER_ESCRIBIR_GESTIONADO", "L17_I_LEER_ESCRIBIR_POR_ALCANZAR", "L17_I_LEER_ESCRIBIR_NO_DETERMINADO", "L17_I_LEER_ESCRIBIR_NO_APLICA",  
                  
                  "L18_ESTUDIOS_POSTSECUNDARIOS_ALCANZADO", "L18_ESTUDIOS_POSTSECUNDARIOS_GESTIONADO", "L18_ESTUDIOS_POSTSECUNDARIOS_POR_ALCANZAR", "L18_ESTUDIOS_POSTSECUNDARIOS_NO_DETERMINADO", "L18_ESTUDIOS_POSTSECUNDARIOS_NO_APLICA",
                  "L18_I_ESTUDIOS_POSTSECUNDARIOS_ALCANZADO", "L18_I_ESTUDIOS_POSTSECUNDARIOS_GESTIONADO", "L18_I_ESTUDIOS_POSTSECUNDARIOS_POR_ALCANZAR", "L18_I_ESTUDIOS_POSTSECUNDARIOS_NO_DETERMINADO", "L18_I_ESTUDIOS_POSTSECUNDARIOS_NO_APLICA",  
                  
                  "L21_NO_PISOS_TIERRA_ALCANZADO", "L21_NO_PISOS_TIERRA_GESTIONADO", "L21_NO_PISOS_TIERRA_POR_ALCANZAR", "L21_NO_PISOS_TIERRA_NO_DETERMINADO", "L21_NO_PISOS_TIERRA_NO_APLICA",
                  "L21_I_NO_PISOS_TIERRA_ALCANZADO", "L21_I_NO_PISOS_TIERRA_GESTIONADO", "L21_I_NO_PISOS_TIERRA_POR_ALCANZAR", "L21_I_NO_PISOS_TIERRA_NO_DETERMINADO", "L21_I_NO_PISOS_TIERRA_NO_APLICA",   
                  
                  "L22_PAREDES_ADECUADAS_ALCANZADO", "L22_PAREDES_ADECUADAS_GESTIONADO", "L22_PAREDES_ADECUADAS_POR_ALCANZAR", "L22_PAREDES_ADECUADAS_NO_DETERMINADO", "L22_PAREDES_ADECUADAS_NO_APLICA",
                  "L22_I_PAREDES_ADECUADAS_ALCANZADO", "L22_I_PAREDES_ADECUADAS_GESTIONADO", "L22_I_PAREDES_ADECUADAS_POR_ALCANZAR", "L22_I_PAREDES_ADECUADAS_NO_DETERMINADO", "L22_I_PAREDES_ADECUADAS_NO_APLICA",   
                  
                  "L23_NO_HACINAMIENTO_ALCANZADO", "L23_NO_HACINAMIENTO_GESTIONADO", "L23_NO_HACINAMIENTO_POR_ALCANZAR", "L23_NO_HACINAMIENTO_NO_DETERMINADO", "L23_NO_HACINAMIENTO_NO_APLICA",
                  "L23_I_NO_HACINAMIENTO_ALCANZADO", "L23_I_NO_HACINAMIENTO_GESTIONADO", "L23_I_NO_HACINAMIENTO_POR_ALCANZAR", "L23_I_NO_HACINAMIENTO_NO_DETERMINADO", "L23_I_NO_HACINAMIENTO_NO_APLICA",   
                  
                  "L25_ACTIVIDAD_PRODUCTIVA_ALCANZADO", "L25_ACTIVIDAD_PRODUCTIVA_GESTIONADO", "L25_ACTIVIDAD_PRODUCTIVA_POR_ALCANZAR", "L25_ACTIVIDAD_PRODUCTIVA_NO_DETERMINADO", "L25_ACTIVIDAD_PRODUCTIVA_NO_APLICA",
                  "L25_I_ACTIVIDAD_PRODUCTIVA_ALCANZADO", "L25_I_ACTIVIDAD_PRODUCTIVA_GESTIONADO", "L25_I_ACTIVIDAD_PRODUCTIVA_POR_ALCANZAR", "L25_I_ACTIVIDAD_PRODUCTIVA_NO_DETERMINADO", "L25_I_ACTIVIDAD_PRODUCTIVA_NO_APLICA",   
                  
                  "L27_MFA_ALCANZADO", "L27_MFA_GESTIONADO", "L27_MFA_POR_ALCANZAR", "L27_MFA_NO_DETERMINADO", "L27_MFA_NO_APLICA",
                  "L27_I_MFA_ALCANZADO", "L27_I_MFA_GESTIONADO", "L27_I_MFA_POR_ALCANZAR", "L27_I_MFA_NO_DETERMINADO", "L27_I_MFA_NO_APLICA",
                  
                  "IPM_POBRE", "IPM_NO_POBRE", "IPM_NO_DETERMINADO")))


BaseGestionFrecuencias = BaseGestionFrecuencias %>% arrange(CodigoDepartamento, CodigoMunicipio) 

setwd(paste(Salidas,"Cortes Certificados", sep = slash))
write.table(BaseGestionFrecuencias, file = "Frecuencias Base  Gestion Hogares Acompañados - Diciembre 2021.csv", sep = ";", row.names = F)

# view(dfSummary(BaseGestionFrecuencias))
