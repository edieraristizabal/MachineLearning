# CLAUDE.md

Guía para Claude Code (y para cualquier persona) que actualice este repositorio. **Cada cambio que se haga en el repositorio debe registrarse al final, en "Registro de cambios".**

## Qué es

Curso **Machine Learning** (Universidad Nacional de Colombia, sede Medellín, Prof. Edier Aristizábal). Un solo repositorio contiene tres productos:

| Producto | Fuente en `master` | URL publicada |
|---|---|---|
| Página del curso (Jekyll, tema minimal) | `index.md`, `_config.yml`, programa en PDF | https://edieraristizabal.github.io/MachineLearning/ |
| Diapositivas (Reveal.js) | `html/MachineLearning.html` y sus recursos en `html/` | https://edieraristizabal.github.io/MachineLearning/html/MachineLearning.html |
| Libro (Jupyter Book 1.x) | `libro/` | https://edieraristizabal.github.io/MachineLearning/libro/ |

Remoto: https://github.com/edieraristizabal/MachineLearning

## Resumen del contenido

El curso **Machine Learning** está orientado para estudiantes de posgrados que deseen adquirir conocimientos sobre modelos multivariados, para establecer patrones y análisis de inferencia estadística y predicción de una variable dependiente a partir de variables independentes o predictoras, con herramientas de aprendizaje automático (*Machine learning*), minería de datos (*Data Mining*) y *Big Data*, bajo un ambiente de trabajo *Python* en *Notebooks* sobre *Jupyter Lab*.

El libro (Aprendizaje automático en geociencias) se creó en 2026 a partir de la presentación (`html/MachineLearning.html`), de las clases en `CLASES_pptx/`, de los notebooks del curso y de los libros guía de `LIBROS/` (James et al., Hastie et al., Kuhn y Johnson, Raschka, Müller y Guido, Gelman y Hill, Goodfellow et al.). Tiene 17 capítulos en cuatro partes (fundamentos; construcción y evaluación de modelos; aprendizaje no supervisado; aprendizaje supervisado) más el apéndice `A_Notebooks.md`, que enlaza en Colab los notebooks de `NOTEBOOKS/Python` y `NOTEBOOKS/R`. Cada capítulo es un notebook ejecutable que usa numpy, pandas, scipy, matplotlib y scikit-learn con datos sintéticos (casi siempre un inventario simulado de deslizamientos), y se publica con las salidas guardadas. Los `.ipynb` son la fuente: para cambiar un capítulo se edita el notebook y se vuelve a ejecutar localmente (`jupyter nbconvert --to notebook --execute --inplace libro/NN_*.ipynb`) antes del commit, porque la Action no ejecuta. Referencias en `libro/references.bib`; `libro/requirements-notebooks.txt` lista las librerías de los capítulos.

Contenido del libro (orden de `libro/_toc.yml`; el título es el primer encabezado de cada archivo):

- `intro`: Aprendizaje automático en geociencias

**Fundamentos**

- `01_Introduccion`: Aprendizaje automático en geociencias
- `02_Datos`: Datos: tipos, recolección y preparación
- `03_EDA`: Análisis exploratorio de datos y distribuciones
- `04_Inferencia`: Inferencia estadística y pruebas de hipótesis

**Construcción y evaluación de modelos**

- `05_SesgoVarianza`: Sesgo, varianza y sobreajuste
- `06_Validacion`: Validación de modelos
- `07_Metricas`: Métricas de evaluación y clases desbalanceadas
- `08_SeleccionVariables`: Selección de variables
- `09_Optimizacion`: Funciones de pérdida, optimización e hiperparámetros

**Aprendizaje no supervisado**

- `10_Agrupamiento`: Agrupamiento
- `11_ReduccionDimensionalidad`: Reducción de dimensionalidad

**Aprendizaje supervisado**

- `12_RegresionLineal`: Regresión lineal y regularización
- `13_GLM`: Modelos lineales generalizados y regresión logística
- `14_KNN_SVM`: Vecinos más cercanos y máquinas de soporte vectorial
- `15_Arboles_Ensambles`: Árboles de decisión y métodos de ensamble
- `16_RedesNeuronales`: Redes neuronales
- `17_Interpretabilidad`: Interpretabilidad de los modelos

**Apéndices**

- `A_Notebooks`: Notebooks del curso
- `bibliografia`: Bibliografía

## Ramas: dónde hacer pull y push

**Siempre se trabaja en `master`.** Ese es el único lugar donde se edita, se hace `git pull` y `git push`, tanto para cambios de la página como de las diapositivas o del libro.

**Nunca editar ni hacer push a `gh-pages` a mano.** Esa rama la genera el workflow `.github/workflows/deploy.yml` en cada push a `master`: construye Jekyll en `_site/`, construye el libro y lo copia en `_site/libro/`, y publica todo en `gh-pages` con `force_orphan` (cualquier cambio manual en `gh-pages` se pierde en el siguiente despliegue).

GitHub Pages sirve un solo sitio por repositorio; por eso la página y el libro se combinan en el Action. En **Settings → Pages** la fuente debe ser *Deploy from a branch*, rama `gh-pages`, carpeta `/ (root)`.

Flujo normal:

```bash
git pull origin master
# ... editar ...
# ... agregar una línea al Registro de cambios de este archivo ...
git add -A
git commit -m "Descripción del cambio"
git push origin master      # el Action publica en 2 a 5 minutos (pestaña Actions)
```

## Estructura

```
.github/
    workflows/
.gitignore
Cursos_AnalisisGeoespacial
NOTEBOOKS/
    Python/
    R/
Programa_MachineLearning.pdf
Programa_MachineLearning.tex
README.md
_config.yml
_layouts/
    default.html
data/
    Machine learning en geociencias _ MachineLearning.html
    Machine learning en geociencias _ MachineLearning_files/
html/
    MachineLearning.html
index.md
libro/
```

Material fuente que **no** se sube (libros guía, presentaciones originales, documentos con derechos): está en `C:\Users\edier\Documents\CATEDRA\MACHINE LEARNING`, fuera de este repositorio.

## Cómo actualizar

**Página del curso.** Editar `index.md`. Los enlaces internos deben ser relativos (`Programa.pdf`, `html/...`), nunca empezar por `/`, porque el sitio vive en `/MachineLearning/`. El libro se enlaza como `https://edieraristizabal.github.io/MachineLearning/libro/<capítulo>.html`. Si cambia la evaluación, actualizarla también en el programa (`.tex` + PDF) y en las diapositivas.

**Diapositivas.** Editar `html/MachineLearning.html` directamente (Reveal.js; ecuaciones con MathJax). Previsualizar con `python -m http.server` desde la raíz y abrir `http://localhost:8000/html/MachineLearning.html`.

**Libro.** Capítulos en MyST Markdown (`.md`) o notebooks (`.ipynb`) dentro de `libro/`; un capítulo nuevo debe agregarse a `libro/_toc.yml`; las citas `{cite:t}` / `{cite:p}` deben existir en el archivo `.bib` del libro.

- **El Action no ejecuta los notebooks** (`execute_notebooks: "off"`): publica las salidas guardadas en cada `.ipynb`. Si se cambia código, ejecutar el notebook localmente y guardarlo con sus salidas antes del commit.
- `libro/requirements.txt` contiene solo lo necesario para construir el libro; `libro/requirements-notebooks.txt` (si existe) lista las librerías para ejecutar los notebooks.
- Si git falla con `bad object refs/desktop.ini` o `failed to perform geometric repack`, es porque Windows/Google Drive creó archivos `desktop.ini` dentro de `.git/`: borrarlos con `find .git -name desktop.ini -delete` y repetir el comando.
- Compilar localmente: `C:\Users\edier\.jbenv\Scripts\jupyter-book.exe build libro` y abrir `libro/_build/html/index.html`.

## Notebooks del curso

`NOTEBOOKS/Python` (01 a 26) y `NOTEBOOKS/R` (GLM, modelos jerárquicos, GAM y bayesianos, en `.ipynb` y `.Rmd`) son el material práctico con datos reales de `data/`. `index.md` los enlaza en `NOTEBOOKS/Python/`.

## Preferencias del autor

- Proponer los cambios de contenido del curso y esperar aprobación antes de editarlos.
- Prosa en párrafos, español, sin rayas (em dash): usar comas o paréntesis.
- Las figuras nuevas se buscan primero en los tableros de Pinterest del autor.

## Pendientes conocidos

- Cambiar en Settings → Pages la fuente a la rama `gh-pages` (hasta entonces sigue publicando la rama `master` sin el libro).
- `NOTEBOOKS/Python/prueba.ipynb` parece un archivo de prueba que podría eliminarse.
- `data/` pesa unos 109 MB; si crece, conviene moverla a Git LFS o a un repositorio de datos.

## Registro de cambios

Cada cambio al repositorio se anota aquí, el más reciente primero, con el formato `- **AAAA-MM-DD**: qué se cambió y por qué (archivos principales).` Esta sección es la memoria del curso entre sesiones: leerla antes de empezar a trabajar.

- **2026-10-24**: Commit de respaldo de los cambios locales pendientes antes de unificar.
- **2026-10-24**: Interfaz en español: `_layouts/default.html` traducido; encabezados de créditos de los notebooks traducidos.
- **2026-10-24**: Revisión de idioma: traducidos al español unos 250 bloques de texto y 185 títulos de diapositivas de `html/MachineLearning.html` (se corrigió además que Lasso es L1 y Ridge es L2, estaban invertidos), las celdas de texto en inglés de los notebooks de Python (01, 02, 05, 06, 07, 12, 13, 15, 18, 21, 22, 23) y de R (BHGLMS, GLMS, HLRMs_BRMS, HLRMs_lme4, GAMs, LRM, BM), unos 300 comentarios de código y 18 títulos de notebooks. Quedan en inglés los títulos de obras, las referencias y los nombres técnicos de uso habitual (bagging, boosting, kernel, SHAP).
- **2026-10-24**: Libro nuevo en `libro/` (Jupyter Book 1.x, `language: es`, `execute_notebooks: off`): 17 capítulos ejecutables, apéndice de notebooks y bibliografía; ejecutado localmente sin errores ni advertencias y compilado sin warnings.
- **2026-10-24**: Unificación en un solo repositorio: `.github/workflows/deploy.yml` (Jekyll + Jupyter Book → `gh-pages`); `_config.yml` con `url`/`baseurl: /MachineLearning` y exclusiones; en `index.md` enlaces relativos, enlaces a los notebooks corregidos (faltaba `Python/` en la ruta), sección del libro y 'Lecture' traducido.
