# Caracterización estadística de la actividad sísmica Chile–Japón (2000–2025) - Asesoría Estadística I • Ingeniería Estadística • USACH
Este repositorio contiene el desarrollo completo de la Asesoría Estadística I realizada en Ingeniería Estadística (USACH), cuyo objetivo es caracterizar y comparar estadísticamente la actividad sísmica observada en Chile continental y Japón durante el período 2000–2025, utilizando información proveniente del catálogo oficial de terremotos de la United States Geological Survey (USGS).
El proyecto fue diseñado bajo principios de ciencia reproducible, incorporando documentación metodológica, procesamiento íntegro de datos, generación automática de resultados y un dashboard interactivo que permite explorar los principales hallazgos del estudio.

## Reproducibilidad y transparencia metodológica

Con el propósito de garantizar la reproducibilidad, trazabilidad y transparencia de los resultados presentados en el preinforme, este repositorio reúne la totalidad del código fuente desarrollado en R.
El flujo de trabajo implementado permite reproducir completamente cada etapa del estudio, incluyendo:
- obtención y depuración de los catálogos sísmicos;
- homogeneización de magnitudes hacia la escala Mw;
- clasificación de profundidad y sistemas de subducción;
- clasificación por zona de subducción;
- análisis exploratorio y estadístico;
- generación automática de tablas, gráficos y figuras;
- construcción del dashboard interactivo.
Todos los resultados presentados en el informe fueron generados directamente a partir de estos scripts.

## ¿Qué encontrarás en este repositorio?

- Preinforme técnico

Documento donde se describe la metodología, decisiones estadísticas, resultados preliminares y propuestas para las siguientes etapas del estudio.

- Dashboard interactivo

Aplicación desarrollada en Shiny que permite explorar dinámicamente la actividad sísmica de ambas regiones mediante mapas, tablas y visualizaciones estadísticas.
https://lambdaanalytics.shinyapps.io/dashboard/

- Código en R

Scripts completamente documentados para la descarga, limpieza, procesamiento y análisis de datos. Los scripts fueron optimizados para mejorar su legibilidad, eficiencia y reproducibilidad. Las herramientas de inteligencia artificial (Gemini IA y Claude AI), fueron utilizadas únicamente como apoyo en tareas de programación y documentación, manteniendo la autoría del diseño metodológico, el análisis estadístico y la interpretación de los resultados.

- Resultados reproducibles

Todas las tablas, figuras y análisis del informe pueden regenerarse ejecutando el código disponible en este repositorio.
