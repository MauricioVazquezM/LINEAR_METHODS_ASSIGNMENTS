## Tarea 2: Regresion multiple


img <- readJPEG("C:/Users/mauva/OneDrive/Documents/ITAM/9no Semestre/METODOS LINEALES/REPOSITORIO/LINEAR_METHODS_ASSIGNMENTS_FALL2024/ASSIGNMENT 2/img1.jpg")

ggplot() +
  annotation_custom(
    rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc")),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  theme_minimal()

img <- readJPEG("C:/Users/mauva/OneDrive/Documents/ITAM/9no Semestre/METODOS LINEALES/REPOSITORIO/LINEAR_METHODS_ASSIGNMENTS_FALL2024/ASSIGNMENT 2/img2.jpg")

ggplot() +
  annotation_custom(
    rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc")),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  theme_minimal()


/*
$$
  \begin{align*}
\text{Casualties} = &\ \beta_0 - 0.22\cdot \text{ln(Pop)} + 36.462 \cdot \text{ln(QY1)} -58.876 \cdot \text{ln(QY2)} \\
& + 26.470 \cdot \text{ln(QY3)} - 15.870 \cdot \text{ln(QY4)} + 12.863 \cdot \text{ln(QY5)} + 1.416 \cdot \text{mUnemp} \\
& - 2.955 \cdot \text{highPR} + 2.582 \cdot \text{lowPR} + 0.0233 \cdot \text{areakmsq10K} + 7.094 \cdot \text{tropicAr} \\
& - 20.749 \cdot \text{elev100m} - 20.749 \cdot \text{langfrac} + 24.907 \cdot \text{langfracsq} + \epsilon
\end{align*}
$$
*/