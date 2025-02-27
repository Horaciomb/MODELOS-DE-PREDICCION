Los **outliers** son observaciones que se desvían significativamente del comportamiento general del resto de los datos. Estos valores atípicos pueden distorsionar análisis estadísticos y modelos predictivos, por lo que su detección es clave para mejorar la calidad del análisis.

#### **Causas Comunes**

- **Errores de medición:** Fallos en la captura o registro de datos.
- **Valores extremos:** Resultados que, por su naturaleza, se alejan de la tendencia general (por ejemplo, ingresos excepcionalmente altos o bajos).
- **Casos especiales:** Fenómenos raros o situaciones particulares que no siguen el patrón general.

#### **Importancia de Detectar Outliers**

- **Mejorar la calidad del análisis:** La presencia de outliers puede sesgar medidas estadísticas como la media y la desviación estándar.
- **Optimizar modelos predictivos:** Al eliminar o tratar adecuadamente los outliers, se pueden obtener modelos más robustos y precisos.
- **Identificar errores o fenómenos interesantes:** A veces, los outliers revelan problemas en la recolección de datos o hallazgos significativos que merecen un análisis más profundo.

#### **Métodos para Detectar Outliers**

- **Visualización:**
    - **Boxplot:** Permite visualizar la distribución de los datos y detectar valores atípicos.
    - **Scatterplot:** Facilita la identificación visual de puntos que se alejan del patrón general.
- **Regla de Tukey:**
    - Se considera que un valor es outlier si se encuentra fuera del rango:
        
        Lıˊmite inferior=Q1−1.5×IQR\text{Límite inferior} = Q1 - 1.5 \times IQRLıˊmite inferior=Q1−1.5×IQR Lıˊmite superior=Q3+1.5×IQR\text{Límite superior} = Q3 + 1.5 \times IQRLıˊmite superior=Q3+1.5×IQR
    - **Donde:**
        
        - Q1Q1Q1 es el primer cuartil (25º percentil).
        - Q3Q3Q3 es el tercer cuartil (75º percentil).
        - **IQR (Interquartile Range):** Es la diferencia entre Q3Q3Q3 y Q1Q1Q1, es decir, IQR=Q3−Q1IQR = Q3 - Q1IQR=Q3−Q1.

Estos métodos permiten identificar sistemáticamente los outliers, facilitando su análisis y el mejoramiento de los procesos de modelización y toma de decisiones.

### Z-scores para Detectar Outliers

El **z-score** es una medida que indica cuántas desviaciones estándar se encuentra un dato respecto a la media de la variable. Se calcula mediante la siguiente fórmula:

z=x−μσz = \frac{x - \mu}{\sigma}z=σx−μ​

donde:

- xxx es el valor de la observación.
- μ\muμ es la media de la variable.
- σ\sigmaσ es la desviación estándar.

En la práctica, se considera que un valor es un **outlier** si su z-score es mayor a 3 o menor a -3 (es decir, si ∣z∣>3|z| > 3∣z∣>3). Esto significa que la observación se encuentra muy alejada de la media, lo que podría indicar un error de medición, un valor extremo o un caso especial.

Detectar outliers mediante z-scores es fundamental, ya que permite:

- **Identificar valores atípicos:** que podrían afectar el análisis estadístico.
- **Mejorar la calidad de los modelos predictivos:** eliminando o ajustando estos valores se obtienen modelos más robustos.
- **Detectar errores en la recopilación de datos:** ayudando a corregir problemas en la fuente de datos.

Análisis de outliers en multiples dimensiones 
malahabobis distance, una metrica para identificar outliers en un espacion multidimensiona
donde s es la matirz de covarianzar.

### Análisis de Outliers en Múltiples Dimensiones

Cuando se analizan datos multidimensionales, es fundamental considerar las relaciones entre las variables para detectar outliers de forma efectiva. La **distancia de Mahalanobis** es una métrica que permite identificar observaciones atípicas en un espacio multidimensional, ya que tiene en cuenta la correlación entre las variables.

#### **Distancia de Mahalanobis**

La distancia de Mahalanobis mide cuántas desviaciones estándar multivariadas se encuentra un punto respecto a la media del conjunto de datos, considerando la dispersión y la correlación de las variables. Su fórmula es:

DM(x)=(x−μ)T S−1 (x−μ)D_M(\mathbf{x}) = \sqrt{(\mathbf{x} - \boldsymbol{\mu})^T\, \mathbf{S}^{-1}\, (\mathbf{x} - \boldsymbol{\mu})}DM​(x)=(x−μ)TS−1(x−μ)​

donde:

- x\mathbf{x}x es el vector de características de la observación.
- μ\boldsymbol{\mu}μ es el vector de medias de cada variable.
- S\mathbf{S}S es la matriz de covarianza de las variables.
- S−1\mathbf{S}^{-1}S−1 es la inversa de la matriz de covarianza.

#### **Interpretación y Uso**

- **Interpretación:**  
    Un valor elevado de DM(x)D_M(\mathbf{x})DM​(x) indica que la observación x\mathbf{x}x se encuentra muy alejada de la media, considerando la estructura de correlación entre las variables. Esto puede señalar que x\mathbf{x}x es un outlier en el contexto multivariado.
    
- **Establecer umbrales:**  
    Dado que la distancia de Mahalanobis se distribuye, bajo ciertos supuestos, según una distribución chi-cuadrado con ppp grados de libertad (donde ppp es el número de variables), es común establecer un umbral a partir de un valor crítico de dicha distribución. Por ejemplo, para un nivel de significancia del 5%, se consideran outliers aquellos puntos para los cuales:
    
    DM2(x)>χp,0.952D_M^2(\mathbf{x}) > \chi^2_{p, 0.95}DM2​(x)>χp,0.952​
- **Aplicación práctica:**
    
    1. **Calcular la media y la matriz de covarianza:** Obtén μ\boldsymbol{\mu}μ y S\mathbf{S}S de tus datos.
    2. **Calcular la distancia de Mahalanobis para cada observación:** Aplica la fórmula para cada punto.
    3. **Definir un umbral:** Utiliza la distribución chi-cuadrado para determinar un valor crítico.
    4. **Identificar outliers:** Aquellas observaciones cuya distancia al cuadrado supera el umbral se consideran outliers.

El uso de la distancia de Mahalanobis es especialmente útil cuando las variables están correlacionadas, ya que a diferencia de la distancia Euclidiana, esta métrica ajusta la escala y la correlación entre variables, proporcionando una detección más precisa de outliers en entornos multidimensionales.

