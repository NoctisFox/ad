set.seed(2025)
dir.create("plots", showWarnings = FALSE)

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("scales")) install.packages("scales")
if (!require("ggridges")) install.packages("ggridges")
if (!require("hexbin")) install.packages("hexbin")
if (!require("patchwork")) install.packages("patchwork")
if (!require("viridis")) install.packages("viridis")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("lubridate")) install.packages("lubridate")
if (!require("forcats")) install.packages("forcats")
if (!require("ggdist")) install.packages("ggdist")

library(tidyverse)
library(ggplot2)
library(scales)
library(ggridges)
library(hexbin)
library(patchwork)
library(viridis)
library(RColorBrewer)
library(ggrepel)
library(lubridate)
library(forcats)
library(ggdist)

df_raw <- read_csv("cleaned_df.csv", show_col_types = FALSE)

df <- df_raw %>%
  mutate(
    creation_date        = as.Date(creation_date),
    creation_year        = as.integer(creation_year),
    creation_month       = as.integer(creation_month),
    programming_language = as.factor(programming_language),
    is_answered          = as.logical(is_answered),
    has_accepted_answer  = as.logical(has_accepted_answer),
    has_code             = as.logical(has_code),
    creation_weekday = factor(
      creation_weekday,
      levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
      ordered = TRUE
    ),
    ai_era = case_when(
      creation_year < 2023 ~ "До ChatGPT (2021–2022)",
      TRUE                 ~ "Після ChatGPT (2023–2025)"
    ) %>% factor(levels = c("До ChatGPT (2021–2022)", "Після ChatGPT (2023–2025)")),
    difficulty_tier = ntile(difficulty_score, 4) %>%
      factor(labels = c("Легкі","Нижче середнього","Вище середнього","Складні")),
    response_time_clean = ifelse(
      first_response_time_hours > quantile(first_response_time_hours, 0.99, na.rm = TRUE),
      NA_real_, first_response_time_hours
    ),
    reputation_tier = ntile(owner_reputation, 3) %>%
      factor(labels = c("Низька репутація","Середня репутація","Висока репутація"))
  ) %>%
  filter(!is.na(programming_language), programming_language != "")

top_langs <- df %>%
  count(programming_language, sort = TRUE) %>%
  slice_head(n = 12) %>%
  pull(programming_language)

df_top <- df %>%
  filter(programming_language %in% top_langs) %>%
  mutate(programming_language = fct_reorder(
    programming_language, quality_score, median, na.rm = TRUE
  ))

hyp <- list(
  h3 = "Г3: Що впливає на оцінку складності запитання?",
  h5 = "Г5: Як вплинуло відкриття загального доступу до ШІ на складність/якість питань?",
  h6 = "Г6: Що впливає на швидкість відповіді на запитання?",
  h7 = "Г7: Як мова програмування впливає на час відповіді / оцінку якості питання?",
  h8 = "Г8: Чи є залежність між довжиною топ-відповіді та її оцінкою?"
)

tag_theme <- function() {
  theme(
    plot.tag          = element_text(size = 11, color = "black",
                                     face = "bold.italic", hjust = 0),
    plot.tag.position = "top"
  )
}

g_h5_rain <- df %>%
  filter(!is.na(difficulty_score), !is.na(ai_era)) %>%
  sample_n(min(n(), 30000)) %>%
  ggplot(aes(x = ai_era, y = difficulty_score, fill = ai_era, color = ai_era)) +
  stat_halfeye(adjust = 0.6, width = 0.55, .width = 0,
               justification = -0.22, point_colour = NA, alpha = 0.82) +
  geom_boxplot(width = 0.14, outlier.shape = NA,
               color = "grey25", fill = "white", alpha = 0.75) +
  geom_jitter(width = 0.04, alpha = 0.035, size = 0.7, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4.5, color = "black") +
  scale_fill_manual(
    values = c("До ChatGPT (2021–2022)" = "#1565C0",
               "Після ChatGPT (2023–2025)" = "#C62828"),
    guide = "none"
  ) +
  scale_color_manual(
    values = c("До ChatGPT (2021–2022)" = "#1565C0",
               "Після ChatGPT (2023–2025)" = "#C62828"),
    guide = "none"
  ) +
  coord_flip() +
  labs(
    tag   = hyp$h5,
    title = "Після ChatGPT весь розподіл складності зміщується вліво",
    x     = NULL,
    y     = "Оцінка складності запитання"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 12),
        panel.grid.minor = element_blank()) +
  tag_theme()

ggsave("plots/plot_h5_raincloud_difficulty.png", g_h5_rain,
       width = 10, height = 5, dpi = 150, create.dir = TRUE)


monthly_stats <- df %>%
  filter(!is.na(creation_year), !is.na(creation_month)) %>%
  mutate(ym = make_date(creation_year, creation_month, 1)) %>%
  group_by(ym) %>%
  summarise(
    med_difficulty = median(difficulty_score, na.rm = TRUE),
    med_quality    = median(quality_score,    na.rm = TRUE),
    med_response   = median(first_response_time_hours, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(ym >= as.Date("2021-01-01"), ym <= as.Date("2025-12-31"))

vline_date <- as.Date("2022-11-30")

make_trend <- function(data, y_var, y_label, col) {
  ggplot(data, aes(x = ym, y = .data[[y_var]])) +
    geom_line(color = col, linewidth = 1.1) +
    geom_smooth(method = "loess", se = TRUE, color = "grey40",
                fill = "grey85", linewidth = 0.6, linetype = "dashed") +
    geom_vline(xintercept = as.numeric(vline_date),
               linetype = "dotted", color = "black", linewidth = 0.8) +
    annotate("text", x = vline_date + 25,
             y = max(data[[y_var]], na.rm = TRUE) * 0.96,
             label = "ChatGPT", hjust = 0, size = 3, color = "black") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(x = NULL, y = y_label) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
}

p1 <- make_trend(monthly_stats, "med_difficulty", "Медіана складності", "#E91E63")
p2 <- make_trend(monthly_stats, "med_quality",    "Медіана якості",     "#2E7D32")
p3 <- make_trend(monthly_stats, "med_response",   "Медіана часу (год)", "#E65100")

g_h5_patch <- (p1 / p2 / p3) +
  plot_annotation(
    tag_levels = list(c("Складність", "Якість", "Час відповіді")),
    title   = hyp$h5,
    caption = "Три незалежні метрики — три однакові тренди після листопада 2022 р.",
    theme   = theme(
      plot.title   = element_text(face = "italic", size = 10, color = "black"),
      plot.caption = element_text(face = "bold", size = 12)
    )
  )

ggsave("plots/plot_h5_patchwork_trends.png", g_h5_patch,
       width = 10, height = 10, dpi = 150, create.dir = TRUE)


slope_data <- df_top %>%
  filter(!is.na(quality_score)) %>%
  group_by(programming_language, ai_era) %>%
  summarise(avg_quality = mean(quality_score, na.rm = TRUE), .groups = "drop") %>%
  mutate(era_num = if_else(ai_era == "До ChatGPT (2021–2022)", 1, 2))

slope_wide <- slope_data %>%
  pivot_wider(names_from = ai_era, values_from = avg_quality,
              id_cols = programming_language) %>%
  mutate(delta = `Після ChatGPT (2023–2025)` - `До ChatGPT (2021–2022)`)

highlight_langs <- slope_wide %>%
  arrange(delta) %>%
  slice(c(1:3, (n()-2):n())) %>%
  pull(programming_language)

slope_data_full <- slope_data %>%
  left_join(slope_wide %>% select(programming_language, delta),
            by = "programming_language") %>%
  mutate(
    is_hl = programming_language %in% highlight_langs,
    alp   = if_else(is_hl, 1.0, 0.55),
    lwd   = if_else(is_hl, 1.6, 0.7)
  )

g_h5_slope <- slope_data_full %>%
  ggplot(aes(x = era_num, y = avg_quality,
             group = programming_language, color = programming_language)) +
  geom_line(aes(alpha = alp, linewidth = lwd)) +
  geom_point(size = 4) +
  geom_text_repel(
    data = . %>% filter(era_num == 2),
    aes(label = as.character(programming_language)),
    nudge_x = 0.12, hjust = 0, size = 4.5, fontface = "bold",
    color = "black", segment.color = "grey50",
    max.overlaps = 30, direction = "y", box.padding = 0.3
  ) +
  geom_text_repel(
    data = . %>% filter(era_num == 1),
    aes(label = as.character(programming_language)),
    nudge_x = -0.12, hjust = 1, size = 4.5, fontface = "bold",
    color = "black", segment.color = "grey50",
    max.overlaps = 30, direction = "y", box.padding = 0.3
  ) +
  scale_x_continuous(breaks = 1:2,
                     labels = c("До ChatGPT\n(2021–2022)",
                                "Після ChatGPT\n(2023–2025)"),
                     limits = c(0.3, 2.9)) +
  scale_alpha_identity() +
  scale_linewidth_identity() +
  scale_color_manual(values = scales::hue_pal()(length(top_langs)), guide = "none") +
  labs(
    tag      = hyp$h5,
    title    = "Всі мови втратили в якості — але Rust і Go впали найменше",
    subtitle = "Виділено мови з найбільшим та найменшим падінням якості",
    x        = NULL,
    y        = "Середня оцінка якості запитань"
  ) +
  theme_minimal(base_size = 15) +
  theme(plot.title         = element_text(face = "bold", size = 15, color = "black"),
        plot.subtitle      = element_text(size = 12, color = "black"),
        axis.text          = element_text(size = 13, color = "black", face = "bold"),
        axis.title.y       = element_text(size = 13, color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank()) +
  tag_theme()

ggsave("plots/plot_h5_slope_quality_lang.png", g_h5_slope,
       width = 10, height = 6.5, dpi = 150, create.dir = TRUE)


heatmap_data <- df_top %>%
  filter(!is.na(creation_year)) %>%
  group_by(programming_language, creation_year) %>%
  summarise(
    avg_difficulty = mean(difficulty_score, na.rm = TRUE),
    avg_quality    = mean(quality_score,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(avg_difficulty, avg_quality),
               names_to = "metric", values_to = "value") %>%
  mutate(
    metric      = recode(metric,
                         avg_difficulty = "Складність",
                         avg_quality    = "Якість"),
    lang_sorted = fct_reorder(programming_language, value, mean, na.rm = TRUE)
  )

g_h7_heat <- heatmap_data %>%
  ggplot(aes(x = factor(creation_year), y = lang_sorted, fill = value)) +
  geom_tile(color = "white", linewidth = 0.55) +
  geom_text(aes(label = round(value, 1)), size = 2.9,
            color = "white", fontface = "bold") +
  geom_vline(xintercept = 2.5, linetype = "dashed",
             color = "black", linewidth = 0.85) +
  facet_wrap(~metric, ncol = 2) +
  scale_fill_distiller(palette = "RdYlBu", direction = 1,
                       name = "Середнє\nзначення") +
  labs(
    tag   = hyp$h7,
    title = "Складність мов стабільна, але якість питань падає після 2022 р. в усіх мовах",
    x     = "Рік",
    y     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 11),
    panel.grid       = element_blank(),
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey93", color = NA)
  ) +
  tag_theme()

ggsave("plots/plot_h7_heatmap_lang_year.png", g_h7_heat,
       width = 13, height = 7, dpi = 150, create.dir = TRUE)


g_h7_violin <- df_top %>%
  filter(!is.na(response_time_clean), response_time_clean > 0) %>%
  mutate(lang_ord = fct_reorder(programming_language,
                                response_time_clean, median, na.rm = TRUE)) %>%
  ggplot(aes(x = lang_ord, y = response_time_clean,
             fill = ai_era, color = ai_era)) +
  geom_violin(alpha = 0.6, scale = "width",
              draw_quantiles = c(0.25, 0.5, 0.75), linewidth = 0.3) +
  geom_jitter(size = 0.25, alpha = 0.06, width = 0.18, show.legend = FALSE) +
  facet_wrap(~ai_era, ncol = 1) +
  scale_y_log10(labels = label_number(accuracy = 0.1),
                breaks  = c(0.01, 0.1, 1, 10, 100, 1000)) +
  scale_fill_manual(
    values = c("До ChatGPT (2021–2022)"    = "#1565C0",
               "Після ChatGPT (2023–2025)" = "#B71C1C"),
    guide = "none"
  ) +
  scale_color_manual(
    values = c("До ChatGPT (2021–2022)"    = "#1565C0",
               "Після ChatGPT (2023–2025)" = "#B71C1C"),
    guide = "none"
  ) +
  coord_flip() +
  labs(
    tag   = hyp$h7,
    title = "Час очікування відповіді зріс після ChatGPT — особливо для Haskell і Rust",
    x     = NULL,
    y     = "Час до першої відповіді (год, лог-шкала)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey93", color = NA)
  ) +
  tag_theme()

ggsave("plots/plot_h7_violin_response_era.png", g_h7_violin,
       width = 12, height = 9, dpi = 150, create.dir = TRUE)


lang_resp <- df_top %>%
  group_by(programming_language) %>%
  summarise(med_time = median(first_response_time_hours, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(above_med = med_time > median(med_time),
         lang_ord  = fct_reorder(programming_language, med_time))

g_h7_lollipop <- lang_resp %>%
  ggplot(aes(x = lang_ord, y = med_time, color = above_med)) +
  geom_segment(aes(xend = lang_ord, y = 0, yend = med_time), linewidth = 1.3) +
  geom_point(size = 5.5) +
  geom_hline(yintercept = median(lang_resp$med_time),
             linetype = "dashed", color = "black", linewidth = 0.8) +
  annotate("text", x = 1.6, y = median(lang_resp$med_time) + 0.08,
           label = "Загальна медіана", size = 3, color = "black", hjust = 0) +
  scale_color_manual(
    values = c("FALSE" = "#1976D2", "TRUE" = "#C62828"),
    labels = c("Швидша за медіану", "Повільніша за медіану"),
    name   = NULL
  ) +
  scale_y_continuous(labels = label_number(suffix = " год")) +
  coord_flip() +
  labs(
    tag   = hyp$h7,
    title = "Python і JavaScript — найшвидші відповіді; Haskell і Rust чекають найдовше",
    x     = NULL,
    y     = "Медіанний час до першої відповіді (год)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    legend.position    = "bottom"
  ) +
  tag_theme()

ggsave("plots/plot_h7_lollipop_response.png", g_h7_lollipop,
       width = 10, height = 6, dpi = 150, create.dir = TRUE)


g_h6_hex_rep <- df %>%
  filter(!is.na(response_time_clean), response_time_clean > 0,
         !is.na(owner_reputation), owner_reputation > 0,
         !is.na(has_code)) %>%
  sample_n(min(n(), 40000)) %>%
  ggplot(aes(x = owner_reputation, y = response_time_clean)) +
  geom_hex(bins = 45, color = NA) +
  geom_smooth(method = "loess", se = TRUE, color = "#e63946",
              linewidth = 1.1, fill = "#e6394633") +
  facet_wrap(~has_code,
             labeller = labeller(has_code = c("FALSE" = "Без коду у питанні",
                                              "TRUE"  = "З кодом у питанні"))) +
  scale_x_log10(labels = label_number(big.mark = " "),
                breaks  = c(1, 10, 100, 1000, 10000, 100000)) +
  scale_y_log10(labels = label_number(accuracy = 0.1),
                breaks  = c(0.01, 0.1, 1, 10, 100)) +
  scale_fill_viridis_c(option = "plasma", trans = "log10",
                       name = "К-сть\nзапитань",
                       labels = label_number(big.mark = " ")) +
  labs(
    tag   = hyp$h6,
    title = "Репутація прискорює відповідь лише для питань з кодом",
    x     = "Репутація автора (лог-шкала)",
    y     = "Час до першої відповіді (год, лог-шкала)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey93", color = NA)
  ) +
  tag_theme()

ggsave("plots/plot_h6_hexbin_rep_code.png", g_h6_hex_rep,
       width = 12, height = 6, dpi = 150, create.dir = TRUE)


lang_diff_all <- df %>%
  filter(!is.na(difficulty_score), !is.na(programming_language),
         programming_language != "") %>%
  group_by(programming_language) %>%
  summarise(
    med_diff = median(difficulty_score, na.rm = TRUE),
    q25      = quantile(difficulty_score, 0.25, na.rm = TRUE),
    q75      = quantile(difficulty_score, 0.75, na.rm = TRUE),
    n        = n(),
    .groups  = "drop"
  ) %>%
  filter(n >= 10) %>%
  mutate(
    lang_ord     = fct_reorder(programming_language, med_diff),
    above_median = med_diff > median(med_diff)
  )

overall_med <- median(lang_diff_all$med_diff)

g_h3_dot <- lang_diff_all %>%
  ggplot(aes(x = med_diff, y = lang_ord, color = above_median)) +
  geom_vline(xintercept = overall_med, linetype = "dashed",
             color = "grey50", linewidth = 0.8) +
  geom_segment(aes(x = q25, xend = q75, yend = lang_ord),
               linewidth = 2.2, alpha = 0.28) +
  geom_point(aes(size = n), alpha = 0.9) +
  scale_color_manual(
    values = c("FALSE" = "#1565C0", "TRUE" = "#C62828"),
    labels = c("Нижче загальної медіани", "Вище загальної медіани"),
    name   = NULL
  ) +
  scale_size_continuous(range = c(2, 9), name = "К-сть питань",
                        labels = label_number(big.mark = " ")) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.04))) +
  labs(
    tag     = hyp$h3,
    title   = "Мова програмування — головний чинник складності: Rust і Haskell найскладніші",
    x       = "Медіана оцінки складності  (відрізок = IQR)          ↑ пунктир = загальна медіана",
    y       = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 13, color = "black"),
    axis.text          = element_text(size = 12, color = "black", face = "bold"),
    axis.title.x       = element_text(size = 11, color = "black"),
    legend.text        = element_text(size = 11, color = "black"),
    legend.title       = element_text(size = 11, color = "black"),
    panel.grid.major.y = element_line(color = "grey93"),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom"
  ) +
  tag_theme()

ggsave("plots/plot_h3_dotplot_difficulty.png", g_h3_dot,
       width = 10, height = 9, dpi = 150, create.dir = TRUE)


g_h3_rep_diff <- df %>%
  filter(!is.na(difficulty_score), !is.na(owner_reputation),
         owner_reputation > 0, !is.na(has_code)) %>%
  sample_n(min(n(), 40000)) %>%
  ggplot(aes(x = owner_reputation, y = difficulty_score)) +
  stat_density_2d_filled(aes(fill = after_stat(level)),
                         alpha = 0.85, contour_var = "ndensity", bins = 12) +
  geom_density_2d(color = "white", linewidth = 0.25, alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, color = "#FF1744",
              linewidth = 1.1, linetype = "solid") +
  facet_wrap(~has_code,
             labeller = labeller(has_code = c("FALSE" = "Без коду у питанні",
                                              "TRUE"  = "З кодом у питанні"))) +
  scale_x_log10(labels = label_number(big.mark = " "),
                breaks  = c(1, 10, 100, 1000, 10000, 100000)) +
  scale_fill_brewer(palette = "Blues", name = "Відносна\nщільність") +
  labs(
    tag   = hyp$h3,
    title = "Досвідчені автори пишуть простіші питання — але лише коли додають код",
    x     = "Репутація автора (лог-шкала)",
    y     = "Оцінка складності запитання"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey93", color = NA)
  ) +
  tag_theme()

ggsave("plots/plot_h3_2d_rep_difficulty_code.png", g_h3_rep_diff,
       width = 12, height = 6, dpi = 150, create.dir = TRUE)


g_h8_density <- df %>%
  filter(!is.na(top_answer_body_length), !is.na(top_answer_score),
         top_answer_body_length > 0, top_answer_score > 0,
         top_answer_score < quantile(top_answer_score, 0.99, na.rm = TRUE)) %>%
  ggplot(aes(x = top_answer_body_length, y = top_answer_score)) +
  stat_density_2d_filled(aes(fill = after_stat(level)),
                         alpha = 0.85, contour_var = "ndensity", bins = 14) +
  geom_density_2d(color = "white", linewidth = 0.28, alpha = 0.5) +
  scale_x_log10(labels = label_number(big.mark = " "),
                breaks  = c(50, 200, 1000, 5000, 20000)) +
  scale_y_log10(labels = label_number(big.mark = " "),
                breaks  = c(1, 5, 20, 100, 500)) +
  scale_fill_brewer(palette = "YlOrRd", name = "Відносна\nщільність") +
  labs(
    tag   = hyp$h8,
    title = "Ядро успішних відповідей: 500–3000 символів і оцінка 5–50",
    x     = "Довжина топ-відповіді (символів, лог-шкала)",
    y     = "Оцінка топ-відповіді (лог-шкала)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()) +
  tag_theme()

ggsave("plots/plot_h8_2d_density_answer.png", g_h8_density,
       width = 10, height = 6.5, dpi = 150, create.dir = TRUE)


g_h8_facet <- df %>%
  filter(!is.na(top_answer_body_length), !is.na(top_answer_score),
         !is.na(difficulty_tier),
         top_answer_body_length > 0, top_answer_score > 0,
         top_answer_score < quantile(top_answer_score, 0.99, na.rm = TRUE)) %>%
  sample_n(min(n(), 30000)) %>%
  ggplot(aes(x = top_answer_body_length, y = top_answer_score)) +
  geom_hex(bins = 30, color = NA) +
  geom_smooth(method = "loess", se = FALSE, color = "#FF1744",
              linewidth = 1.1) +
  facet_wrap(~difficulty_tier, nrow = 1) +
  scale_x_log10(labels = label_number(big.mark = " "),
                breaks  = c(100, 1000, 10000)) +
  scale_y_log10(labels = label_number(big.mark = " "),
                breaks  = c(1, 10, 100)) +
  scale_fill_viridis_c(option = "mako", trans = "log10",
                       name = "К-сть\nзапитань",
                       labels = label_number(big.mark = " ")) +
  labs(
    tag   = hyp$h8,
    title = "Для складних питань розгорнута відповідь важливіша — нахил кривої крутіший",
    x     = "Довжина топ-відповіді (символів, лог-шкала)",
    y     = "Оцінка топ-відповіді (лог-шкала)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "grey93", color = NA)
  ) +
  tag_theme()

ggsave("plots/plot_h8_hexbin_facet_difficulty.png", g_h8_facet,
       width = 14, height = 5.5, dpi = 150, create.dir = TRUE)


g_h7_hex_lang <- df_top %>%
  filter(body_word_count > 0, body_word_count < 3000,
         score > -10, score < 100) %>%
  ggplot(aes(x = body_word_count, y = score)) +
  geom_hex(bins = 25, color = NA) +
  geom_smooth(method = "loess", se = FALSE, color = "#FF5722",
              linewidth = 0.9) +
  facet_wrap(~programming_language, ncol = 4, scales = "fixed") +
  scale_x_log10(breaks = c(10, 100, 1000),
                labels = c("10", "100", "1K")) +
  scale_fill_viridis_c(option = "inferno", trans = "log10",
                       name = "К-сть\nзапитань",
                       labels = label_number(big.mark = " ")) +
  labs(
    tag   = hyp$h7,
    title = "Залежність довжини питання від оцінки відрізняється по мовах: Rust реагує, Python — ні",
    x     = "Слів у запитанні (лог-шкала)",
    y     = "Оцінка запитання"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = "grey93", color = NA)
  ) +
  tag_theme()

ggsave("plots/plot_h7_hexbin_words_bylang.png", g_h7_hex_lang,
       width = 14, height = 8, dpi = 150, create.dir = TRUE)