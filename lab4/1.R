# =============================================================================
# Лабораторна робота №4 — УЧАСНИК 1: Ядрова регресія
# Оцінка Надараї-Вотсона та локальної лінійної регресії
#
# Датасет:  Stack Overflow 2020–2025 (cleaned_df.csv)
# Залежна:  ln(first_response_time_hours + 1)
# Регресори (ядрова): ln_reputation, ln_word_count, has_code
# =============================================================================

# ---- 0. Пакети ---------------------------------------------------------------
# install.packages(c("tidyverse", "np"))
library(tidyverse)
library(np)


# =============================================================================
# 1. ПІДГОТОВКА ДАНИХ
#    (відтворює конструювання змінних з Лаби 3, файл 2.r)
# =============================================================================

df_raw <- read_csv("cleaned_df.csv", show_col_types = FALSE)

cat("Розмір датасету:", nrow(df_raw), "рядків\n")
cat("NA у first_response_time_hours:",
    sum(is.na(df_raw$first_response_time_hours)),
    sprintf("(%.1f%%) — цензуровані спостереження\n",
            mean(is.na(df_raw$first_response_time_hours)) * 100))

df <- df_raw %>%
  mutate(
    ln_response_time  = log(first_response_time_hours + 1),
    has_code          = as.integer(has_code),
    ln_reputation     = log(owner_reputation + 1),
    ln_word_count     = log(body_word_count),
    ln_reputation_sq  = ln_reputation^2,
    ln_word_count_sq  = ln_word_count^2,
    code_x_rep        = has_code * ln_reputation,
    is_veteran        = as.integer(owner_reputation > 200),
    is_weekend        = as.integer(creation_weekday %in% c(5L, 6L)),
    post_chatgpt      = as.integer(creation_year >= 2023),
    programming_language = relevel(factor(programming_language),
                                   ref = "javascript")
  ) %>%
  filter(!is.na(first_response_time_hours))

cat("Робоча вибірка:", nrow(df), "\n")


# ---- 1.1 Розбивка на навчальну та валідаційну вибірки -----------------------
# Важливо: виконується ДО будь-якого моделювання, щоб уникнути data leakage
set.seed(42)
train_idx <- sample(seq_len(nrow(df)), size = floor(0.8 * nrow(df)))
df_train  <- df[train_idx, ]
df_val    <- df[-train_idx, ]

cat("Навчальна вибірка:", nrow(df_train), "| Валідаційна:", nrow(df_val), "\n")


# ---- 1.2 Підвибірка для ядрової регресії ------------------------------------
# Обгрунтування: npregbw на повних 53+ тис. спостереженнях та крос-валідацією
# рахується кілька годин або не завершується. Підвибірка з 3000 рядків
# обрана через обчислювальні обмеження CV; вона залишається репрезентативною
# завдяки випадковому відбору (set.seed гарантує відтворюваність).
set.seed(42)
df_kernel <- df_train %>% slice_sample(n = 3000)

cat("Підвибірка для ядрової регресії:", nrow(df_kernel), "\n")


# =============================================================================
# 2. ВИБІР РЕГРЕСОРІВ
#
# Обрано три змінні: ln_reputation, ln_word_count, has_code
#
# Обгрунтування:
#   - ln_reputation та ln_word_count — два ключових неперервних регресори,
#     для яких LOESS у Лабі 3 показав нелінійні зв'язки (рис. 2a, 2b).
#     Саме вони найцікавіші для непараметричного аналізу.
#   - has_code — головна бінарна змінна дослідницького питання (H1).
#   - Решта змінних (post_chatgpt, is_weekend, difficulty_score, quality_score,
#     dummy мов) виключена: їхній вплив добре описується лінійно (стабільні
#     коефіцієнти у всіх специфікаціях Лаби 3), а їх включення збільшило б
#     час крос-валідації на порядок.
# =============================================================================

cat("\nОбрані регресори для ядрової регресії:\n")
cat("  ln_reputation, ln_word_count, has_code\n\n")


# =============================================================================
# 3. ПІДБІР ШИРИН ВІКОН ЧЕРЕЗ КРОС-ВАЛІДАЦІЮ
#    Метод: cv.ls (cross-validated least squares) — мінімізує незміщену оцінку
#    прогнозного ризику, як описано в лекції.
#
#    УВАГА: npregbw може рахуватися від 20 хв до кількох годин.
#    Результати зберігаються у .rds-файлах; при повторному запуску завантажуються.
# =============================================================================

# ---- 3.1 Надараї-Вотсон (local constant, regtype = "lc") -------------------
if (file.exists("bw_nw.rds")) {
  cat("Завантаження збережених ширин вікон NW з bw_nw.rds...\n")
  bw_nw <- readRDS("bw_nw.rds")
} else {
  cat("Підбір ширин вікон NW (CV.LS)... Це може тривати довго.\n")
  bw_nw <- npregbw(
    formula  = ln_response_time ~ ln_reputation + ln_word_count + has_code,
    data     = df_kernel,
    regtype  = "lc",       # local constant = оцінка Надараї-Вотсона
    bwmethod = "cv.ls"     # крос-валідація за LS критерієм
  )
  saveRDS(bw_nw, "bw_nw.rds")
  cat("Збережено: bw_nw.rds\n")
}

cat("Ширини вікон NW (Надараї-Вотсон):", round(bw_nw$bw, 4), "\n")

# ---- 3.2 Локальна лінійна регресія (regtype = "ll") -----------------------
if (file.exists("bw_ll.rds")) {
  cat("Завантаження збережених ширин вікон LL з bw_ll.rds...\n")
  bw_ll <- readRDS("bw_ll.rds")
} else {
  cat("Підбір ширин вікон LL (CV.LS)... Це може тривати довго.\n")
  bw_ll <- npregbw(
    formula  = ln_response_time ~ ln_reputation + ln_word_count + has_code,
    data     = df_kernel,
    regtype  = "ll",       # local linear = локальна лінійна регресія
    bwmethod = "cv.ls"
  )
  saveRDS(bw_ll, "bw_ll.rds")
  cat("Збережено: bw_ll.rds\n")
}

cat("Ширини вікон LL (Локальна лінійна):", round(bw_ll$bw, 4), "\n")


# =============================================================================
# 4. ОЦІНКА МОДЕЛЕЙ
# =============================================================================

cat("\nОцінка моделей...\n")
model_nw <- npreg(bw_nw)
model_ll <- npreg(bw_ll)

cat("\n=== Надараї-Вотсон (NW) ===\n")
print(summary(model_nw))

cat("\n=== Локальна лінійна (LL) ===\n")
print(summary(model_ll))

# R² та MSE на навчальній вибірці
r2_nw <- model_nw$R2
r2_ll <- model_ll$R2
mse_nw_train <- mean(model_nw$resid^2)
mse_ll_train <- mean(model_ll$resid^2)

cat(sprintf("\nR² NW (навч.): %.4f  |  MSE NW (навч.): %.4f\n", r2_nw, mse_nw_train))
cat(sprintf("R² LL (навч.): %.4f  |  MSE LL (навч.): %.4f\n", r2_ll, mse_ll_train))


# =============================================================================
# 5. ПОБУДОВА СІТКИ ЗНАЧЕНЬ І ГРАФІКІВ
#
# Підхід: кожен графік — залежність Y від одного регресора, решта зафіксована
# на медіані (неперервні) або моді (бінарні).
#
# Обгрунтування фіксації на медіані:
#   Медіана стійка до викидів і відповідає типовому спостереженню.
#   Особливо важливо для ln_reputation, де розподіл скошений:
#   медіана ≈ 4.54, але є спостереження з ln_reputation > 10.
#   Мода для has_code (бінарна) обирається як округлене середнє.
# =============================================================================

med_rep   <- median(df_kernel$ln_reputation)
med_wc    <- median(df_kernel$ln_word_count)
mode_code <- as.integer(round(mean(df_kernel$has_code)))

cat(sprintf("\nРівні фіксації:\n"))
cat(sprintf("  ln_reputation (медіана) = %.4f\n", med_rep))
cat(sprintf("  ln_word_count (медіана) = %.4f\n", med_wc))
cat(sprintf("  has_code (мода)         = %d\n", mode_code))


# ---- 5.1 Графік 1: залежність від ln_reputation ----------------------------
# (ln_word_count зафіксовано на медіані, has_code на моді)

grid_rep <- seq(min(df_kernel$ln_reputation),
                max(df_kernel$ln_reputation),
                length.out = 200)

newdata_rep <- data.frame(
  ln_reputation = grid_rep,
  ln_word_count = med_wc,
  has_code      = mode_code
)

pred_nw_rep <- predict(model_nw, newdata = newdata_rep, se.fit = TRUE)
pred_ll_rep <- predict(model_ll, newdata = newdata_rep, se.fit = TRUE)

plot_rep_df <- tibble(
  x     = grid_rep,
  nw    = pred_nw_rep$fit,
  nw_lo = pred_nw_rep$fit - 1.96 * pred_nw_rep$se.fit,
  nw_hi = pred_nw_rep$fit + 1.96 * pred_nw_rep$se.fit,
  ll    = pred_ll_rep$fit,
  ll_lo = pred_ll_rep$fit - 1.96 * pred_ll_rep$se.fit,
  ll_hi = pred_ll_rep$fit + 1.96 * pred_ll_rep$se.fit
)

p_rep <- ggplot(plot_rep_df, aes(x = x)) +
  geom_ribbon(aes(ymin = nw_lo, ymax = nw_hi),
              fill = "#3182bd", alpha = 0.2) +
  geom_line(aes(y = nw, colour = "Надараї-Вотсон"), linewidth = 1.2) +
  geom_ribbon(aes(ymin = ll_lo, ymax = ll_hi),
              fill = "#e6550d", alpha = 0.2) +
  geom_line(aes(y = ll, colour = "Локальна лінійна"), linewidth = 1.2) +
  scale_colour_manual(
    values = c("Надараї-Вотсон" = "#3182bd",
               "Локальна лінійна" = "#e6550d")
  ) +
  labs(
    title    = "Ядрова регресія: ln(час відповіді) від репутації автора",
    subtitle = paste0(
      "ln_word_count зафіксовано на медіані = ", round(med_wc, 2),
      "; has_code = ", mode_code,
      "\n(95% довірчі смуги)"
    ),
    x      = "ln(owner_reputation + 1)",
    y      = "ln(first_response_time + 1)",
    colour = "Метод"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("fig_kernel_reputation.png", p_rep, width = 10, height = 5.5, dpi = 150)
cat("Збережено: fig_kernel_reputation.png\n")


# ---- 5.2 Графік 2: залежність від ln_word_count ----------------------------
# (ln_reputation зафіксовано на медіані, has_code на моді)

grid_wc <- seq(min(df_kernel$ln_word_count),
               max(df_kernel$ln_word_count),
               length.out = 200)

newdata_wc <- data.frame(
  ln_reputation = med_rep,
  ln_word_count = grid_wc,
  has_code      = mode_code
)

pred_nw_wc <- predict(model_nw, newdata = newdata_wc, se.fit = TRUE)
pred_ll_wc <- predict(model_ll, newdata = newdata_wc, se.fit = TRUE)

plot_wc_df <- tibble(
  x     = grid_wc,
  nw    = pred_nw_wc$fit,
  nw_lo = pred_nw_wc$fit - 1.96 * pred_nw_wc$se.fit,
  nw_hi = pred_nw_wc$fit + 1.96 * pred_nw_wc$se.fit,
  ll    = pred_ll_wc$fit,
  ll_lo = pred_ll_wc$fit - 1.96 * pred_ll_wc$se.fit,
  ll_hi = pred_ll_wc$fit + 1.96 * pred_ll_wc$se.fit
)

p_wc <- ggplot(plot_wc_df, aes(x = x)) +
  geom_ribbon(aes(ymin = nw_lo, ymax = nw_hi),
              fill = "#3182bd", alpha = 0.2) +
  geom_line(aes(y = nw, colour = "Надараї-Вотсон"), linewidth = 1.2) +
  geom_ribbon(aes(ymin = ll_lo, ymax = ll_hi),
              fill = "#e6550d", alpha = 0.2) +
  geom_line(aes(y = ll, colour = "Локальна лінійна"), linewidth = 1.2) +
  scale_colour_manual(
    values = c("Надараї-Вотсон" = "#3182bd",
               "Локальна лінійна" = "#e6550d")
  ) +
  labs(
    title    = "Ядрова регресія: ln(час відповіді) від обсягу тексту питання",
    subtitle = paste0(
      "ln_reputation зафіксовано на медіані = ", round(med_rep, 2),
      "; has_code = ", mode_code,
      "\n(95% довірчі смуги)"
    ),
    x      = "ln(body_word_count)",
    y      = "ln(first_response_time + 1)",
    colour = "Метод"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave("fig_kernel_wordcount.png", p_wc, width = 10, height = 5.5, dpi = 150)
cat("Збережено: fig_kernel_wordcount.png\n")


# ---- 5.3 Графік 3: порівняння NW та LL (обидва регресори, один ряд) --------
# Зручний підсумковий графік для звіту

p_combined <- cowplot::plot_grid(p_rep, p_wc, ncol = 2, labels = c("A", "B"))
ggsave("fig_kernel_combined.png", p_combined,
       width = 18, height = 5.5, dpi = 150)
cat("Збережено: fig_kernel_combined.png\n")


# =============================================================================
# 6. MSE НА ВАЛІДАЦІЙНІЙ ВИБІРЦІ
#    (потрібно для порівняльної таблиці — Учасник 5)
#
#    УВАГА: np::predict на великій валідаційній вибірці може тривати кілька
#    хвилин. Якщо потрібна швидша перевірка — можна взяти підвибірку df_val.
# =============================================================================

cat("\nРозрахунок MSE на валідаційній вибірці...\n")
pred_nw_val <- predict(model_nw, newdata = df_val)
pred_ll_val <- predict(model_ll, newdata = df_val)

mse_nw_val <- mean((df_val$ln_response_time - pred_nw_val)^2, na.rm = TRUE)
mse_ll_val <- mean((df_val$ln_response_time - pred_ll_val)^2, na.rm = TRUE)

cat(sprintf("MSE Надараї-Вотсон  (val): %.4f\n", mse_nw_val))
cat(sprintf("MSE Локальна лінійна (val): %.4f\n", mse_ll_val))


# =============================================================================
# 7. ПІДСУМКОВА ТАБЛИЦЯ РЕЗУЛЬТАТІВ
# =============================================================================

cat("\n", strrep("=", 65), "\n", sep = "")
cat("ПІДСУМОК ЯДРОВОЇ РЕГРЕСІЇ\n")
cat(strrep("=", 65), "\n", sep = "")

results_table <- data.frame(
  Метод       = c("Надараї-Вотсон (NW)", "Локальна лінійна (LL)"),
  R2_train    = round(c(r2_nw, r2_ll), 4),
  MSE_train   = round(c(mse_nw_train, mse_ll_train), 4),
  MSE_val     = round(c(mse_nw_val, mse_ll_val), 4),
  bw_rep      = round(c(bw_nw$bw[1], bw_ll$bw[1]), 4),
  bw_wc       = round(c(bw_nw$bw[2], bw_ll$bw[2]), 4),
  bw_code     = round(c(bw_nw$bw[3], bw_ll$bw[3]), 4)
)
names(results_table) <- c("Метод", "R² (навч.)", "MSE (навч.)",
                          "MSE (вал.)",
                          "h_ln_rep", "h_ln_wc", "h_has_code")
print(results_table, row.names = FALSE)

cat("\nОб'єкти для інших учасників:\n")
cat("  df_train, df_val  -> для всіх учасників\n")
cat("  df_kernel         -> підвибірка ядрової регресії\n")
cat("  mse_nw_val, mse_ll_val -> для порівняльної таблиці (Учасник 5)\n")

cat("\nЗбережені файли:\n")
for (f in c("bw_nw.rds", "bw_ll.rds",
            "fig_kernel_reputation.png",
            "fig_kernel_wordcount.png",
            "fig_kernel_combined.png")) {
  cat(" ", f, "\n")
}
cat(strrep("=", 65), "\n", sep = "")