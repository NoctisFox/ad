# Встановлення та підключення необхідних бібліотек
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggcorrplot")) install.packages("ggcorrplot")
if (!require("lubridate")) install.packages("lubridate")
if (!require("scales")) install.packages("scales")

library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(lubridate)
library(scales)

# Створення папки для збереження графіків, якщо вона не існує
if(!dir.exists("plots_3")) {
  dir.create("plots_3")
}

# Завантаження датасету
df <- read.csv("cleaned_df.csv", stringsAsFactors = FALSE)

# Обробка та перетворення часових і категоріальних змінних
df <- df %>%
  mutate(
    creation_date = ymd_hms(creation_date),
    creation_month_year = floor_date(creation_date, "month"),
    creation_weekday = factor(creation_weekday,
                              levels = 0:6,
                              labels = c("Нд", "Пн", "Вт", "Ср", "Чт", "Пт", "Сб"))
  )

# Загальна тема для всіх графіків
my_theme <- theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 10)),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
  )


# 1. Зв’язок довжини питання та його оцінки
p1 <- ggplot(df, aes(x = body_word_count, y = score)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) + 
  scale_x_log10(labels = comma) +
  scale_y_continuous(trans = pseudo_log_trans(base = 10), breaks = c(-10, 0, 10, 100, 1000)) +
  labs(
    title = "Короткі та довгі запитання мають однакові шанси на високу оцінку",
    x = "Кількість слів у запитанні (логарифмічна шкала)",
    y = "Оцінка запитання (псевдо-лог шкала)"
  ) +
  my_theme

ggsave("plots_3/1_scatter_words_vs_score.png", plot = p1, width = 10, height = 6, dpi = 300)

# 2. Зв’язок довжини відповіді та її оцінки
p2 <- ggplot(filter(df, !is.na(top_answer_body_length) & !is.na(top_answer_score)),
             aes(x = top_answer_body_length, y = top_answer_score)) +
  geom_point(alpha = 0.4, color = "forestgreen") +
  scale_x_log10(labels = comma) +
  scale_y_continuous(trans = pseudo_log_trans(base = 10)) +
  labs(
    title = "Спостерігається позитивний зв'язок між об'ємом відповіді та її оцінкою",
    x = "Довжина топ-відповіді (символів, лог шкала)",
    y = "Оцінка топ-відповіді"
  ) +
  my_theme

ggsave("plots_3/2_scatter_answer_length_vs_score.png", plot = p2, width = 10, height = 6, dpi = 300)

# 3. Динаміка переглядів у часі
trend_data <- df %>%
  group_by(creation_month_year) %>%
  summarise(median_views = median(view_count, na.rm = TRUE),
            total_questions = n()) %>%
  filter(!is.na(creation_month_year))

p3 <- ggplot(trend_data, aes(x = creation_month_year, y = median_views)) +
  geom_line(color = "darkorange", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "З роками медіанна кількість переглядів запитань суттєво знизилася",
    x = "Місяць та рік створення",
    y = "Медіанна кількість переглядів"
  ) +
  my_theme

ggsave("plots_3/3_line_trend_views_over_time.png", plot = p3, width = 10, height = 6, dpi = 300)


# 4. Час до першої відповіді залежно від дня тижня
p4 <- ggplot(filter(df, !is.na(first_response_time_hours)),
             aes(x = creation_weekday, y = first_response_time_hours, fill = creation_weekday)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.alpha = 0.3) +
  scale_y_log10(labels = comma) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Швидкість отримання першої відповіді не залежить від дня тижня",
    x = "День тижня оприлюднення",
    y = "Час до першої відповіді (години, лог шкала)"
  ) +
  my_theme +
  theme(legend.position = "none")

ggsave("plots_3/4_boxplot_response_time_vs_weekday.png", plot = p4, width = 10, height = 6, dpi = 300)

# 5. Кореляційна матриця числових змінних
numeric_vars <- df %>%
  select(view_count, score, answer_count, title_word_count,
         body_word_count, difficulty_score, quality_score,
         owner_reputation, first_response_time_hours, top_answer_score) %>%
  na.omit()

corr_matrix <- cor(numeric_vars, use = "complete.obs")
# Підписи змінних для зручності інтерпретації
rownames(corr_matrix) <- c("Перегляди", "Оцінка", "К-сть відповідей", "Слів у заголовку",
                           "Слів у тілі", "Складність", "Якість", "Репутація автора",
                           "Час 1-ї відповіді", "Оцінка топ-відповіді")
colnames(corr_matrix) <- rownames(corr_matrix)

p5 <- ggcorrplot(corr_matrix,
                 method = "square",
                 type = "lower",
                 lab = TRUE,
                 lab_size = 3,
                 colors = c("#6D9EC1", "white", "#E46726"),
                 title = "Найбільша кореляція спостерігається між оцінкою запитання \nта оцінкою топової відповіді",
                 legend.title = "Кореляція") +
  theme(
    plot.title = element_text(size = 14, face = "bold", margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11)
  )

ggsave("plots_3/5_correlation_matrix.png", plot = p5, width = 10, height = 10, dpi = 300)


df_filtered <- df %>%
  filter(!is.na(programming_language) & programming_language != "")

# 6. Репутація авторів залежно від мови програмування
p6 <- ggplot(df_filtered, aes(x = programming_language, y = owner_reputation, fill = programming_language)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.alpha = 0.3) +
  scale_y_log10(labels = comma) +
  labs(
    title =  "Автори, що пишуть на Haskell, \nмають дещо вищу репутацію порівняно з іншими",
    x = "Мова програмування",
    y = "Репутація автора (логарифмічна шкала)"
  ) +
  my_theme +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("plots_3/6_boxplot_reputation_vs_language.png", plot = p6, width = 10, height = 6, dpi = 300)
