# ============================================================
#  Лабораторна робота 4 — Аналіз головних компонент (PCA)
#  Датасет: Stack Overflow Questions (kutayahin, 2025)
# ============================================================

# ------ 0. Пакети ------
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggrepel)

# ------ 1. Завантаження та підготовка даних ------
df_raw <- read_csv("stackoverflow_questions.csv")

numeric_vars <- c(
  "view_count", "score", "answer_count",
  "accepted_answer_score", "code_block_count",
  "title_word_count", "title_char_count",
  "body_word_count", "body_char_count",
  "difficulty_score", "quality_score",
  "owner_reputation", "first_response_time_hours",
  "top_answer_score", "top_answer_body_length", "tag_count"
)

df_num <- df_raw %>%
  select(all_of(numeric_vars)) %>%
  drop_na()

cat("Розмір вибірки після очищення:", nrow(df_num), "x", ncol(df_num), "\n")

# ------ 2. Логарифмічне перетворення ------
log1p_vars <- c(
  "view_count", "score", "answer_count",
  "owner_reputation", "body_word_count", "body_char_count",
  "top_answer_score", "top_answer_body_length",
  "first_response_time_hours", "accepted_answer_score"
)

df_transformed <- df_num %>%
  mutate(across(all_of(log1p_vars), ~ log1p(abs(.x))))

df_scaled <- scale(df_transformed)

# ------ 3. PCA ------
pca_res <- PCA(df_scaled, scale.unit = FALSE, ncp = 10, graph = FALSE)
eig_vals <- get_eigenvalue(pca_res)
print(round(eig_vals, 3))

# ------ 4. Scree plot ------
p_scree <- fviz_eig(
  pca_res, addlabels = TRUE, ylim = c(0, 40),
  barfill = "#4E79A7", barcolor = "#2C5F8A", linecolor = "#E15759",
  ggtheme = theme_minimal(base_size = 13)
) +
  labs(
    title    = "Scree Plot — власні числа та відсоток дисперсії",
    subtitle = "Критерій «ліктя» вказує на 3–4 головні компоненти",
    x = "Головна компонента", y = "% поясненої дисперсії"
  ) +
  geom_hline(yintercept = 100 / ncol(df_transformed),
             linetype = "dashed", colour = "grey50") +
  annotate("text", x = ncol(df_transformed) - 1,
           y = 100 / ncol(df_transformed) + 1.2,
           label = "Середнє (рівномірний розподіл)", size = 3.2, colour = "grey40")

ggsave("scree_plot.png", p_scree, width = 9, height = 5, dpi = 150)

# ------ 5. Biplot PC1 vs PC2 ------
p_biplot_12 <- fviz_pca_biplot(
  pca_res, axes = c(1, 2), geom.ind = "point",
  col.ind = "contrib",
  gradient.cols = c("#AED6F1", "#2ECC71", "#E74C3C"),
  col.var = "#2C3E50", repel = TRUE, alpha.ind = 0.25,
  ggtheme = theme_minimal(base_size = 12)
) +
  labs(title    = "Biplot: PC1 vs PC2",
       subtitle = "Стрілки — змінні; точки — спостереження (колір = внесок)")

ggsave("biplot_PC1_PC2.png", p_biplot_12, width = 10, height = 8, dpi = 150)

# ------ 6. Biplot PC1 vs PC3 ------
p_biplot_13 <- fviz_pca_biplot(
  pca_res, axes = c(1, 3), geom.ind = "point",
  col.ind = "contrib",
  gradient.cols = c("#AED6F1", "#2ECC71", "#E74C3C"),
  col.var = "#2C3E50", repel = TRUE, alpha.ind = 0.25,
  ggtheme = theme_minimal(base_size = 12)
) +
  labs(title    = "Biplot: PC1 vs PC3",
       subtitle = "Стрілки — змінні; точки — спостереження (колір = внесок)")

ggsave("biplot_PC1_PC3.png", p_biplot_13, width = 10, height = 8, dpi = 150)

# ------ 7. Loading plot ------
p_var_12 <- fviz_pca_var(
  pca_res, axes = c(1, 2), col.var = "cos2",
  gradient.cols = c("#F7DC6F", "#E67E22", "#C0392B"),
  repel = TRUE, ggtheme = theme_minimal(base_size = 12)
) +
  labs(
    title    = "Проєкції змінних на PC1–PC2 (cos²)",
    subtitle = "Довжина стрілки та колір — якість представлення змінної"
  )

ggsave("var_plot_PC1_PC2.png", p_var_12, width = 8, height = 7, dpi = 150)

# ------ 8. Heatmap внесків ------
contrib_mat <- pca_res$var$contrib[, 1:4]
contrib_df  <- as.data.frame(contrib_mat) %>%
  rownames_to_column("variable") %>%
  pivot_longer(-variable, names_to = "PC", values_to = "contribution")

p_contrib_heat <- ggplot(contrib_df,
       aes(x = PC, y = reorder(variable, contribution), fill = contribution)) +
  geom_tile(colour = "white") +
  scale_fill_gradient2(low = "#EBF5FB", mid = "#3498DB", high = "#1A5276",
                       midpoint = 100 / nrow(contrib_mat), name = "Внесок (%)") +
  geom_text(aes(label = round(contribution, 1)), size = 3) +
  theme_minimal(base_size = 12) +
  labs(title    = "Внески змінних у перші 4 головні компоненти",
       subtitle = "Значення > середнього виділені темнішим кольором",
       x = NULL, y = NULL)

ggsave("contrib_heatmap.png", p_contrib_heat, width = 9, height = 7, dpi = 150)

# ------ 9. Топ-20 спостережень ------
ind_coords  <- as.data.frame(pca_res$ind$coord)
ind_contrib <- as.data.frame(pca_res$ind$contrib)
w <- eig_vals$`variance.percent`[1:3] / sum(eig_vals$`variance.percent`[1:3])
ind_contrib$total_contrib <- as.matrix(ind_contrib[, 1:3]) %*% w
top20_idx <- order(ind_contrib$total_contrib, decreasing = TRUE)[1:20]

top20_df <- df_raw[top20_idx, ] %>%
  select(question_id, title, programming_language,
         owner_reputation, view_count, first_response_time_hours, quality_score) %>%
  mutate(total_contrib = ind_contrib$total_contrib[top20_idx],
         PC1 = ind_coords$Dim.1[top20_idx],
         PC2 = ind_coords$Dim.2[top20_idx])

print(top20_df)

# Графік спостережень
set.seed(42)
sample_idx <- sample(nrow(ind_coords), min(5000, nrow(ind_coords)))

plot_df <- ind_coords[sample_idx, ] %>%
  mutate(contrib = ind_contrib$total_contrib[sample_idx])

p_obs <- ggplot(plot_df, aes(x = Dim.1, y = Dim.2)) +
  geom_point(aes(colour = contrib), alpha = 0.35, size = 0.8) +
  scale_colour_gradient(low = "#D5DBDB", high = "#E74C3C", name = "Внесок") +
  geom_point(data = ind_coords[top20_idx, ],
             colour = "#C0392B", size = 3, shape = 17) +
  ggrepel::geom_text_repel(
    data = bind_cols(ind_coords[top20_idx, ],
                     top20_df %>% select(question_id)),
    aes(x = Dim.1, y = Dim.2, label = question_id),
    size = 2.8, colour = "#922B21", max.overlaps = 20
  ) +
  theme_minimal(base_size = 12) +
  labs(title    = "Спостереження у просторі PC1–PC2",
       subtitle = "Трикутники — топ-20 за внеском у дисперсію",
       x = paste0("PC1 (", round(eig_vals$`variance.percent`[1], 1), "%)"),
       y = paste0("PC2 (", round(eig_vals$`variance.percent`[2], 1), "%)"))

ggsave("obs_plot.png", p_obs, width = 10, height = 7, dpi = 150)

# ------ 10. Підсумок ------
n_80 <- which(eig_vals$`cumulative.variance.percent` >= 80)[1]
n_90 <- which(eig_vals$`cumulative.variance.percent` >= 90)[1]
cat(sprintf("Для 80%% дисперсії потрібно %d PC\n", n_80))
cat(sprintf("Для 90%% дисперсії потрібно %d PC\n", n_90))

loadings_df <- as.data.frame(pca_res$var$coord[, 1:4]) %>%
  rownames_to_column("Змінна") %>%
  rename(PC1 = Dim.1, PC2 = Dim.2, PC3 = Dim.3, PC4 = Dim.4)

write_csv(loadings_df, "pca_loadings.csv")
write_csv(as.data.frame(eig_vals) %>% rownames_to_column("PC"), "pca_eigenvalues.csv")
write_csv(top20_df, "pca_top20_observations.csv")

cat("Всі файли збережено.\n")