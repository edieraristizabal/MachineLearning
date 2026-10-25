# Aprendizaje automático en geociencias

Este libro acompaña el curso de Machine Learning de la Facultad de Minas de la Universidad Nacional de Colombia, sede Medellín, orientado a estudiantes de posgrado en geociencias e ingeniería. Recorre el mismo camino del curso: cómo se formula un problema y se preparan los datos, cómo se construye, valida y evalúa un modelo, y cuáles son los principales métodos no supervisados y supervisados, desde la regresión lineal hasta las redes neuronales.

Cada capítulo es un notebook de Jupyter ejecutable escrito en Python con `numpy`, `pandas`, `scipy`, `matplotlib` y `scikit-learn`. Para que todo funcione sin descargas, los ejemplos usan datos sintéticos generados en el mismo notebook, casi siempre un inventario simulado de deslizamientos con variables como la pendiente, la curvatura, la distancia a los cauces, la lluvia y la litología. Usar datos sintéticos tiene una ventaja didáctica: como se conoce el proceso que los generó, se puede comprobar si un modelo recupera la relación verdadera, cuándo sobreajusta y qué tan engañosa puede ser una métrica. Los notebooks del curso, con datos reales, se listan en el apéndice {doc}`A_Notebooks`.

El texto se apoya en los libros guía del curso, en especial {cite}`james2021` y {cite}`hastie2009` para el aprendizaje estadístico, {cite}`kuhn2013` para el modelado predictivo aplicado, {cite}`raschka2017` y {cite}`muller2016` para la implementación en Python, {cite}`gelman2007` para la regresión y los modelos multinivel, y {cite}`goodfellow2016` para las redes neuronales. Para las aplicaciones a amenazas naturales se recomiendan {cite}`pourghasemi2019` y {cite}`merghadi2020`.

## Cómo está organizado

La primera parte, Fundamentos, presenta qué es el aprendizaje automático y cómo se diferencia de la estadística clásica, cómo se preparan y exploran los datos y cómo se hace inferencia a partir de una muestra. La segunda, Construcción y evaluación de modelos, trata el compromiso entre sesgo y varianza, la validación (incluida la validación espacial, indispensable en geociencias), las métricas, la selección de variables y la optimización. La tercera cubre el aprendizaje no supervisado: agrupamiento y reducción de dimensionalidad. La cuarta recorre los modelos supervisados: regresión lineal y regularización, modelos lineales generalizados, vecinos más cercanos, máquinas de soporte vectorial, árboles y ensambles, redes neuronales y, al final, las herramientas para interpretar modelos de caja negra.

## Recursos del curso

- [Página del curso](https://edieraristizabal.github.io/MachineLearning/)
- [Diapositivas](https://edieraristizabal.github.io/MachineLearning/html/MachineLearning.html)
- [Programa del curso](https://edieraristizabal.github.io/MachineLearning/Programa_MachineLearning.pdf)

```{tableofcontents}
```
