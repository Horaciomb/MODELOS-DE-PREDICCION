### DBSCAN (Density-Based Spatial Clustering of Applications with Noise)

DBSCAN es un algoritmo de clustering basado en densidad que tiene la capacidad de identificar clústeres de cualquier forma y detectar outliers (puntos de ruido). No requiere que se especifique el número de clústeres de antemano, lo que lo hace muy útil en análisis exploratorios.

#### **Conceptos Clave**

- **Punto núcleo:**  
    Es un punto que cuenta con al menos un número mínimo de puntos (MinPts) dentro de su vecindad definida por un radio ϵ\epsilonϵ (eps). Es el “corazón” del clúster.
    
- **Punto de borde:**  
    Es un punto que no cumple la condición de punto núcleo (es decir, tiene menos de MinPts vecinos), pero se encuentra dentro del radio ϵ\epsilonϵ de un punto núcleo. Pertenece al clúster, pero no tiene suficiente densidad por sí mismo.
    
- **Punto de ruido (outlier):**  
    Es un punto que no es ni núcleo ni de borde; se encuentra en regiones de baja densidad y se considera que no pertenece a ningún clúster.
    

#### **Parámetros Fundamentales**

1. **ϵ\epsilonϵ (epsilon o radio de densidad):**  
    Define la distancia máxima entre dos puntos para que sean considerados vecinos. Es decir, determina el radio de la vecindad de cada punto.
    
2. **MinPts (mínimo número de puntos):**  
    Es el número mínimo de puntos que deben encontrarse dentro del radio ϵ\epsilonϵ para que un punto sea considerado núcleo. Este valor suele fijarse en función de la dimensionalidad del conjunto de datos (por ejemplo, para datos bidimensionales se recomienda un valor mínimo de 4).
    

#### **Pasos del Algoritmo**

1. **Selección y etiquetado:**  
    Se elige un punto no visitado y se marca como visitado.
    
2. **Búsqueda de vecinos:**  
    Se obtienen todos los puntos dentro del radio ϵ\epsilonϵ del punto seleccionado.
    
3. **Formación de clúster:**
    
    - Si el número de puntos vecinos es mayor o igual a MinPts, el punto se clasifica como núcleo y se inicia un nuevo clúster.
    - Se agregan todos los puntos vecinos a dicho clúster.
    - Para cada uno de esos vecinos, si son también puntos núcleo, se repite recursivamente el proceso para expandir el clúster.
4. **Etiquetado de ruido:**
    
    - Si el número de vecinos es menor a MinPts, el punto se etiqueta inicialmente como ruido.
    - Posteriormente, si se descubre que es vecino de un punto núcleo, se re-clasifica como punto de borde.
5. **Iteración:**  
    Se repite el proceso hasta que se han visitado todos los puntos del conjunto de datos.
    

#### **Ventajas y Desventajas**

- **Ventajas:**
    
    - _Flexibilidad en la forma de los clústeres:_ Puede detectar clústeres de formas arbitrarias, a diferencia de métodos como K-means, que asumen formas convexas o esféricas.
    - _No requiere especificar el número de clústeres:_ Los clústeres se definen a partir de la densidad de los datos.
    - _Detección de outliers:_ Identifica automáticamente los puntos de ruido que no pertenecen a ningún clúster.
- **Desventajas:**
    
    - _Sensibilidad a los parámetros:_ La elección de ϵ\epsilonϵ y MinPts es crucial; valores inadecuados pueden llevar a resultados subóptimos.
    - _Dificultad en datos con densidades variables:_ Si los clústeres tienen densidades muy distintas, un único ϵ\epsilonϵ puede no ser suficiente para agruparlos adecuadamente.