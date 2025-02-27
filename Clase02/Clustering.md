### **K-Means Clustering**

K-Means es un algoritmo iterativo de aprendizaje no supervisado que particiona un conjunto de datos en _k_ grupos (_clusters_). Cada observación pertenece al grupo cuyo centroide es el más cercano.

#### **Proceso del algoritmo:**

1. Se eligen _k_ centroides iniciales de manera aleatoria.
2. Se asigna cada punto de datos al centroide más cercano.
3. Se recalculan los centroides como el promedio de los puntos asignados a cada cluster.
4. Se repiten los pasos 2 y 3 hasta que los centroides ya no cambien significativamente o se alcance un criterio de convergencia.

#### **Notación:**

- CkC_kCk​ representa el centroide del clúster kkk.
- GkG_kGk​ es el conjunto de puntos que pertenecen al clúster kkk.
- La función objetivo del algoritmo minimiza la suma de las distancias cuadradas entre los puntos y sus centroides:

J=∑k=1K∑xi∈Gk∣∣xi−Ck∣∣2J = \sum_{k=1}^{K} \sum_{x_i \in G_k} ||x_i - C_k||^2J=k=1∑K​xi​∈Gk​∑​∣∣xi​−Ck​∣∣2

donde ∣∣xi−Ck∣∣2||x_i - C_k||^2∣∣xi​−Ck​∣∣2 es la distancia euclidiana al cuadrado entre cada punto xix_ixi​ y su centroide CkC_kCk​.

#### **Consideraciones:**

- La elección de _k_ puede afectar significativamente el rendimiento del algoritmo.
- El algoritmo puede converger a mínimos locales dependiendo de la inicialización de los centroides.
- Se pueden utilizar métodos como _Elbow Method_ o _Silhouette Score_ para determinar un _k_ óptimo.

#### **Ventajas y Desventajas**

✅ **Ventajas:**

- Fácil de implementar y comprender.
- Escala bien para grandes conjuntos de datos.
- Convergencia rápida en la mayoría de los casos.
- Eficiente en términos computacionales, especialmente con implementaciones optimizadas.

❌ **Desventajas:**

- Sensible a la inicialización de los centroides, lo que puede afectar los resultados.
- Puede converger en mínimos locales en lugar del óptimo global.
- Requiere definir _k_ previamente, lo que puede ser complicado sin un conocimiento previo de los datos.
- No funciona bien con datos que tienen formas no esféricas o tamaños de clústeres muy diferentes.

### **Tipos de clustering jerárquico:**

🔹 **Aglomerativo (Bottom-Up)**

- Comienza con cada punto como su propio clúster.
- En cada iteración, los clústeres más cercanos se fusionan hasta que queda un solo clúster que agrupa todos los puntos.
- Se basa en métricas de distancia como _single-linkage_ (mínima distancia), _complete-linkage_ (máxima distancia) o _average-linkage_ (promedio de distancias).

🔹 **Divisivo (Top-Down)**

- Comienza con un solo clúster que incluye todos los puntos.
- En cada iteración, el clúster más grande se divide recursivamente en subclústeres hasta que cada punto es su propio clúster o se alcanza un criterio de parada.

## Dendrograma

Un **dendrograma** es un diagrama en forma de árbol que se utiliza para visualizar la estructura jerárquica resultante de un proceso de agrupamiento (clustering), especialmente en el clustering jerárquico. Este gráfico ilustra cómo se van fusionando (en el caso aglomerativo) o dividiendo (en el divisivo) los grupos de datos, mostrando la relación y similitud entre ellos.

**Características principales:**

- **Eje vertical:** Representa la distancia o disimilitud entre los clústeres. La altura a la que se unen dos grupos indica cuán similares (o diferentes) son; una unión a mayor altura indica mayor disimilitud.
- **Eje horizontal:** Enumera las observaciones individuales o los clústeres resultantes, permitiendo visualizar cómo se agrupan a medida que se asciende en la jerarquía.
- **Corte del dendrograma:** Se puede trazar una línea horizontal para "cortar" el árbol y definir el número de clústeres finales. Todos los grupos unidos por debajo de esta línea se consideran parte de un mismo clúster.
En resumen, el dendrograma es una herramienta visual fundamental para interpretar la estructura jerárquica de los datos, facilitando la identificación de grupos y ayudando a determinar un número óptimo de clústeres.


[DBSCAN]
[Outliers]