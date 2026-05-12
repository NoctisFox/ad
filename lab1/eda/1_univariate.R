if (!require("tidyverse")) install.packages("tidyverse")
if (!require("scales")) install.packages("scales")
if (!require("tidytext")) install.packages("tidytext")

library(tidyverse)
library(scales)
library(tidytext)

df <- read_csv("cleaned_df.csv")

if (!dir.exists("plots_2")) {
  dir.create("plots_2")
}

plot_views_hist <- ggplot(df, aes(x = view_count)) +
  geom_histogram(bins = 50, fill = "red", color = "white", alpha = 0.8) +
  scale_x_log10(labels = scales::comma) +
  labs(
    x = "Кількість переглядів (логарифмічна шкала)",
    y = "Частота"
  ) +
  theme_minimal()

ggsave("plots_2/plot_views_hist.png", plot = plot_views_hist, width = 8, height = 5)



plot_answers_hist <- ggplot(df, aes(x = answer_count)) +
  geom_histogram(binwidth = 0.1, fill = "darkorange", color = "white") +
  scale_x_continuous(trans = scales::pseudo_log_trans(base = 10),
                     breaks = c(0, 1, 10, 100, 1000)) +
  labs(
    x = "Кількість відповідей",
    y = "Частота"
  ) +
  theme_minimal()

ggsave("plots_2/plot_answers_hist.png", plot = plot_answers_hist, width = 8, height = 5)



plot_score_box <- ggplot(df, aes(y = score)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red", outlier.alpha = 0.5) +
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 10)) +
  labs(
    x = "",
    y = "Рейтинг (псевдо-логарифмічна шкала)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggsave("plots_2/plot_score_box.png", plot = plot_score_box, width = 8, height = 5)



plot_languages <- df %>%
  filter(!is.na(programming_language)) %>%
  ggplot(aes(y = fct_rev(fct_infreq(programming_language)))) +
  geom_bar(fill = "purple", alpha = 0.7) +
  labs(
    x = "Кількість питань",
    y = "Мова програмування"
  ) +
  theme_minimal()

ggsave("plots_2/plot_languages.png", plot = plot_languages, width = 8, height = 5)



plot_reputation <- ggplot(df, aes(x = owner_reputation)) +
  geom_histogram(bins = 40, fill = "darkcyan", color = "white", alpha = 0.8) +
  scale_x_log10(labels = scales::comma) +
  labs(
    x = "Репутація користувача (Log10)",
    y = "Кількість питань"
  ) +
  theme_minimal()

ggsave("plots_2/plot_reputation.png", plot = plot_reputation, width = 8, height = 5)



plot_is_answered <- df %>%
  filter(!is.na(is_answered)) %>%
  ggplot(aes(x = as.factor(is_answered), fill = as.factor(is_answered))) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = c("FALSE" = "tomato", "TRUE" = "seagreen")) +
  labs(
    x = "Чи має питання бодай одну відповідь?",
    y = "Кількість питань"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("plots_2/plot_is_answered.png", plot = plot_is_answered, width = 8, height = 5)



plot_word_count <- ggplot(df, aes(x = body_word_count)) +
  geom_histogram(fill = "coral", color = "black", bins = 50) +
  scale_x_continuous(trans = scales::pseudo_log_trans(base = 10)) +
  labs(
    x = "Кількість слів у тілі питання (псевдо-логарифмічна шкала)",
    y = "Частота"
  ) +
  theme_minimal()

ggsave("plots_2/plot_word_count.png", plot = plot_word_count, width = 8, height = 5)



plot_weekday <- df %>%
  filter(!is.na(creation_weekday)) %>%
  ggplot(aes(x = factor(
    creation_weekday,
    levels = 1:7,
    labels = c("Пн", "Вт", "Ср", "Чт", "Пт", "Сб", "Нд")
  ))) +
  geom_bar(fill = "mediumpurple", color = "white", alpha = 0.8) +
  labs(
    x = "День тижня",
    y = "Кількість опублікованих питань"
  ) +
  theme_minimal()

ggsave("plots_2/plot_weekday.png", plot = plot_weekday, width = 8, height = 5)



plot_top_tags <- df %>%
  filter(!is.na(tags)) %>%
  unnest_tokens(tag, tags, token = "words") %>%
  count(tag, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = n, y = fct_reorder(tag, n))) +
  geom_col(fill = "mediumseagreen", alpha = 0.8) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    x = "Кількість згадувань",
    y = "Тег"
  ) +
  theme_minimal()

ggsave("plots_2/plot_top_tags.png", plot = plot_top_tags, width = 9, height = 6)



data("stop_words")

custom_stop_words <- bind_rows(
  stop_words,
  tibble(word = c("python", "error", "using", "how", "to", "in", "c", "file", "code"),
         lexicon = "custom")
)

plot_title_words <- df %>%
  filter(!is.na(title)) %>%
  unnest_tokens(word, title) %>%
  anti_join(custom_stop_words, by = "word") %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = n, y = fct_reorder(word, n))) +
  geom_col(fill = "indianred", alpha = 0.8) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    x = "Частота слова",
    y = "Слово"
  ) +
  theme_minimal()

ggsave("plots_2/plot_title_words.png", plot = plot_title_words, width = 9, height = 6)



plot_bigrams <- df %>%
  filter(!is.na(title)) %>%
  unnest_tokens(bigram, title, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE) %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x = n, y = fct_reorder(bigram, n))) +
  geom_col(fill = "dodgerblue", alpha = 0.8) +
  labs(
    x = "Кількість зустрічань",
    y = "Пара слів"
  ) +
  theme_minimal()

ggsave("plots_2/plot_bigrams.png", plot = plot_bigrams, width = 9, height = 6)



numeric_cols <- c("view_count", "answer_count", "score", "body_word_count")

outlier_stats <- df %>%
  select(all_of(numeric_cols)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE),
    IQR = IQR(value, na.rm = TRUE),
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR
  )

df_outliers_all <- df %>%
  mutate(row_id = row_number()) %>%
  select(row_id, all_of(numeric_cols)) %>%
  pivot_longer(cols = all_of(numeric_cols), names_to = "variable", values_to = "value") %>%
  left_join(outlier_stats, by = "variable") %>%
  mutate(
    is_outlier = case_when(
      value > upper_bound ~ "Позитивний викид",
      value < lower_bound ~ "Негативний викид",
      TRUE ~ "Норма"
    )
  )

plot_all_outliers <- ggplot(df_outliers_all, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red", outlier.size = 1.5, alpha = 0.7) +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 10)) +
  labs(
    x = "",
    y = "Значення (Псевдо-логарифмічна шкала)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 12, face = "bold")
  )

ggsave("plots_2/plot_all_outliers.png", plot = plot_all_outliers, width = 10, height = 6)