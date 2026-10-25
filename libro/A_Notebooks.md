# Notebooks del curso

Los capítulos del libro usan datos sintéticos. Los notebooks del curso, en la carpeta [`NOTEBOOKS`](https://github.com/edieraristizabal/MachineLearning/tree/master/NOTEBOOKS) del repositorio, aplican los mismos métodos a datos reales (inventarios de deslizamientos, bases de datos de torrencialidad, conjuntos de referencia) y profundizan en temas que el libro solo introduce, como los modelos jerárquicos, los modelos aditivos, los métodos bayesianos y los procesos gaussianos. Los enlaces abren cada notebook en Google Colab; los que usan datos de la carpeta [`data`](https://github.com/edieraristizabal/MachineLearning/tree/master/data) los leen directamente desde el repositorio.

## Python

| Notebook | Tema | Capítulo del libro |
|---|---|---|
| [01](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/01_Ambiente_de_trabajo.ipynb) | Ambiente de trabajo: Python, Conda, Jupyter, Docker | {doc}`01_Introduccion` |
| [02](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/02_Importar.ipynb) | Importar datos: archivos, API, web scraping | {doc}`02_Datos` |
| [03](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/03_DataFrame.ipynb) | Estructuras de datos con pandas | {doc}`02_Datos` |
| [04](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/04_Procesamiento_datos.ipynb) | Procesamiento de datos e ingeniería de variables | {doc}`02_Datos` |
| [05 EDA](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/05_EDA.ipynb) | Análisis exploratorio de datos | {doc}`03_EDA` |
| [05 Estadística](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/05_Statistic.ipynb) | Estadística descriptiva y distribuciones | {doc}`03_EDA` |
| [06](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/06_SeleccionVariables.ipynb) | Selección de variables y pruebas estadísticas | {doc}`04_Inferencia`, {doc}`08_SeleccionVariables` |
| [07](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/07_Desbalance.ipynb) | Datos desbalanceados: submuestreo, sobremuestreo, SMOTE | {doc}`07_Metricas` |
| [08](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/08_Validation.ipynb) | Validación cruzada | {doc}`06_Validacion` |
| [09](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/09_Algorithm_Tunning.ipynb) | Selección de hiperparámetros | {doc}`09_Optimizacion` |
| [10](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/10_Metric.ipynb) | Métricas de evaluación | {doc}`07_Metricas` |
| [11](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/11_Bias-Variance_TradeOff.ipynb) | Compromiso sesgo-varianza | {doc}`05_SesgoVarianza` |
| [12](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/12_Cluster.ipynb) | Agrupamiento | {doc}`10_Agrupamiento` |
| [13](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/13_PCA.ipynb) | Análisis de componentes principales | {doc}`11_ReduccionDimensionalidad` |
| [14](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/14_Discriminante.ipynb) | Análisis discriminante lineal | {doc}`11_ReduccionDimensionalidad` |
| [15](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/15_Regresion_lineal.ipynb) | Regresión lineal | {doc}`12_RegresionLineal` |
| [15 LR](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/15_LR.ipynb) | Regresión lineal por MCO, descenso por gradiente y MCMC | {doc}`09_Optimizacion` |
| [16](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/16_Regresion_Logistica.ipynb) | Regresión logística | {doc}`13_GLM` |
| [17](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/17_KNN.ipynb) | Vecinos más cercanos | {doc}`14_KNN_SVM` |
| [18](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/18_SVM.ipynb) | Máquinas de soporte vectorial | {doc}`14_KNN_SVM` |
| [19](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/19_RedesNeuronales.ipynb) | Redes neuronales | {doc}`16_RedesNeuronales` |
| [19 PINN](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/19_PINNs.ipynb) | Redes neuronales informadas por la física | {doc}`16_RedesNeuronales` |
| [20](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/20_Ensembles.ipynb) | Métodos de ensamble | {doc}`15_Arboles_Ensambles` |
| [21](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/21_HLRMs.ipynb) | Modelos de regresión lineal jerárquicos | {doc}`13_GLM` |
| [22](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/22_GAMs.ipynb) | Modelos aditivos generalizados | {doc}`13_GLM` |
| [23](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/23_BLM.ipynb) | Regresión lineal bayesiana | {doc}`12_RegresionLineal` |
| [24](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/24_GaussianProcess.ipynb) | Procesos gaussianos | |
| [25](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/25_SHAP.ipynb) | Explicaciones de Shapley (SHAP) | {doc}`17_Interpretabilidad` |
| [26](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/Python/26_GaussianNB.ipynb) | Bayes ingenuo gaussiano | |

## R

Los notebooks de R desarrollan los modelos lineales generalizados, jerárquicos, aditivos y bayesianos con `glm`, `lme4`, `mgcv` y `brms`. Para abrirlos en Colab hay que elegir el entorno de ejecución de R.

| Notebook | Tema |
|---|---|
| [15 Regresión lineal](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/R/15_Regresion_lineal_R.ipynb) | Regresión lineal en R |
| [15 GLM](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/R/15_GLMS_R.ipynb) | Modelos lineales generalizados (multinivel) con `glm` y `lme4` |
| [15 GLM bayesianos](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/R/15_BHGLMS_R.ipynb) | Modelos generalizados jerárquicos bayesianos con `brms` |
| [15 Jerárquicos lme4](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/R/15_HLRMs_lme4.ipynb) | Regresión lineal jerárquica con `lme4` |
| [15 Jerárquicos brms](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/R/15_HLRMs_BRMS.ipynb) | Regresión lineal jerárquica bayesiana con `brms` y la lista WAMBS |
| [15 GAM](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/R/15_GAMs_R.ipynb) | Modelos aditivos generalizados con `mgcv` |
| [15 GAM jerárquicos](https://colab.research.google.com/github/edieraristizabal/MachineLearning/blob/master/NOTEBOOKS/R/15_GAMs_R_paper.ipynb) | GAM jerárquicos en ecología |

También hay versiones en R Markdown (`.Rmd`) de la regresión lineal, la logística, los modelos mixtos y los modelos bayesianos en la misma carpeta.
