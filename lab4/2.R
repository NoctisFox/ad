# install.packages(c("tidyverse", "np", "cowplot"))
library(tidyverse)
library(np)
library(cowplot)



csv_candidates <- c("cleaned_df.csv", "cleaned_df копія.csv")
csv_path <- csv_candidates[file.exists(csv_candidates)][1]
if (is.na(csv_path)) {
  stop("Не знайдено cleaned_df.csv. Покладіть файл у робочу директорію.")
}
cat("Читаю датасет з файлу:", csv_path, "\n")

df_raw <- read_csv(csv_path, show_col_types = FALSE)

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
    is_weekend        = as.integer(creation_weekday %in% c(5L, 6L)),
    post_chatgpt      = as.integer(creation_year >= 2023),
    programming_language = relevel(factor(programming_language),
                                   ref = "javascript")
  ) %>%
  filter(!is.na(first_response_time_hours))

cat("Робоча вибірка:", nrow(df), "\n")


# 1.1 Розбивка train/val
set.seed(42)
train_idx <- sample(seq_len(nrow(df)), size = floor(0.8 * nrow(df)))
df_train  <- df[train_idx, ]
df_val    <- df[-train_idx, ]

cat("Навчальна вибірка:", nrow(df_train), "| Валідаційна:", nrow(df_val), "\n")


# Беремо 5000 — для PLR це обчислювально доцільно:
#   - CV у npplregbw здійснюється лише за 2 ширинами (ln_rep, ln_wc),
#     а не 3, як у ядрової регресії => простір пошуку менший.
#   - Лінійна частина має ~30 регресорів (включно з 24 dummy мов).
#     На 3000 спостережень на одну рідкісну мову припадає ~125 рядків,
#     на 5000 ≈ 208 — оцінки коефіцієнтів суттєво стабільніші.
#   - Параметрична частина має √n-збіжність, тому збільшення n значущо
#     покращує точність β̂ — головну метрику для порівняння з Лаб.3.
set.seed(42)
df_plr <- df_train %>% slice_sample(n = 5000)

cat("Підвибірка для PLR:", nrow(df_plr), "\n")


# Непараметрична частина m(ln_reputation, ln_word_count):
#   - ln_reputation: Лаб.3 показала, що квадратичний терм значущий
#     (F-тест p=0.033), є ефект "спадної віддачі" репутації.
#     Лінійна форма недостатня — потрібна гнучка функціональна форма.
#   - ln_word_count: Лаб.3 показала суто лінійний вплив (квадрат: p=0.639).
#     Включаємо в непараметричну частину для перевірки робастності
#     цього висновку без явного припущення про лінійність.
#   - Спільне моделювання двох регресорів m(·,·) дає змогу автоматично
#     врахувати їхню взаємодію — це перевага PLR над окремими сплайнами.
#
# Лінійна частина (контрольні + ключова бінарна):
#   - has_code: бінарна; згладжування за бінарною змінною дає лише два
#     значення m̂, тож сенсу включати в непараметричну частину немає.
#     Крім того, її коефіцієнт — головний об'єкт порівняння з Лаб.3
#     (Гіпотеза H1: has_code ⇒ повільніша відповідь).
#   - has_code:ln_reputation: взаємодія значуща в Лаб.3 (p=0.002).
#     Лишаємо як ЛІНІЙНИЙ терм (β2·has_code·ln_rep), що дозволяє
#     зберегти цей ефект і пряму співставність з Моделлю 5 Лаб.3.
#   - difficulty_score, quality_score: безперервні контролі, у Лаб.3
#     стабільно лінійні, без ознак нелінійності => в лінійну частину.
#   - is_weekend, post_chatgpt: бінарні часові контролі.
#   - programming_language (24 dummy, базова "javascript"): аналог
#     фіксованих ефектів, як рекомендовано в завданні.


# Групуємо рідкісні мови у "other" у ПІДВИБІРЦІ для PLR.
# Обгрунтування: на 5000 спостережень мови з <50 рядками дають
# непропорційно нестійкі коефіцієнти, а після partialling-out можуть
# спричиняти числову сингулярність (помилка chol.default).
# Базова категорія "javascript" повинна залишитись окремою.
MIN_LANG_OBS <- 50

lang_freq_plr <- table(df_plr$programming_language)
rare_langs <- names(lang_freq_plr)[lang_freq_plr < MIN_LANG_OBS]
rare_langs <- setdiff(rare_langs, "javascript")
cat("Рідкісні мови (<", MIN_LANG_OBS, "обс. у підвибірці), згруповані у 'other':\n  ",
    paste(rare_langs, collapse = ", "), "\n", sep = "")

df_plr <- df_plr %>%
  mutate(programming_language = as.character(programming_language),
         programming_language = ifelse(programming_language %in% rare_langs,
                                       "other", programming_language),
         programming_language = relevel(factor(programming_language),
                                        ref = "javascript"))

df_val <- df_val %>%
  mutate(programming_language = as.character(programming_language),
         programming_language = ifelse(programming_language %in% rare_langs,
                                       "other", programming_language),
         programming_language = factor(programming_language,
                                       levels = levels(df_plr$programming_language)))

# Створюємо матрицю мовних dummy явно (виключаємо базову категорію javascript)
# make.names() очищує спецсимволи (c++, c#, objective-c → c.., c., objective.c)
# — необхідно, щоб формула R коректно парсилася.
lang_dummies <- model.matrix(~ programming_language, data = df_plr)[, -1]
colnames(lang_dummies) <- make.names(
  gsub("programming_language", "lang_", colnames(lang_dummies))
)

# Об'єднуємо все необхідне в єдиний data frame для зручності
df_plr_full <- bind_cols(
  df_plr %>%
    select(ln_response_time, ln_reputation, ln_word_count,
           has_code, difficulty_score, quality_score,
           is_weekend, post_chatgpt),
  as_tibble(lang_dummies)
) %>%
  mutate(code_x_rep = has_code * ln_reputation)

# Те саме для валідаційної вибірки
lang_dummies_val <- model.matrix(~ programming_language, data = df_val)[, -1]
colnames(lang_dummies_val) <- make.names(
  gsub("programming_language", "lang_", colnames(lang_dummies_val))
)

df_val_full <- bind_cols(
  df_val %>%
    select(ln_response_time, ln_reputation, ln_word_count,
           has_code, difficulty_score, quality_score,
           is_weekend, post_chatgpt),
  as_tibble(lang_dummies_val)
) %>%
  mutate(code_x_rep = has_code * ln_reputation)

lang_cols <- grep("^lang_", names(df_plr_full), value = TRUE)

linear_vars <- c("has_code", "code_x_rep",
                 "difficulty_score", "quality_score",
                 "is_weekend", "post_chatgpt",
                 lang_cols)

cat("\nРегресори в непараметричній частині (2):\n  ln_reputation, ln_word_count\n")
cat("Регресори в лінійній частині (", length(linear_vars), "):\n", sep = "")
cat("  ", paste(linear_vars[1:6], collapse = ", "), "\n")
cat("  +", length(lang_cols), "dummy мов програмування\n\n")


# 3. ОЦІНЮВАННЯ PLR — ОСНОВНА МОДЕЛЬ
#    Метод Robinson: спочатку CV.LS для ширин вікон m̂(z),
#    потім "partialling-out": регресуємо Y та X на Z непараметрично,
#    а на залишках виконуємо OLS.
# 3.1 Формула PLR (np-синтаксис)
# у np::npplreg формула має вигляд: y ~ x1 + x2 | z1 + z2
# де ліворуч від | — ЛІНІЙНІ регресори, праворуч — НЕПАРАМЕТРИЧНІ.
formula_plr <- as.formula(
  paste0("ln_response_time ~ ",
         paste(linear_vars, collapse = " + "),
         " | ln_reputation + ln_word_count")
)

# 3.2 Підбір ширин вікон через CV 
# Стратегія:
#   - Перший запуск (немає bw_plr.rds): повноцінне CV.LS, далі зберігаємо.
#   - Усі наступні запуски (є bw_plr.rds): використовуємо ВИТЯГНУТІ з нього
#     h-значення для E[Y|Z] як єдині (uniform) бандвідси для всіх
#     partialling-out регресій. Це пропускає CV і робить запуск миттєвим.
#     Аргументація: коефіцієнти лінійної частини мають √n-збіжність і
#     малочутливі до помірного відхилення бандвідсів від оптимальних
#     для кожного індивідуального E[X|Z].
extract_yz_bandwidth <- function(bw_obj) {
  # bw_obj$bw — це список; перший елемент — об'єкт бандвідсу для E[Y|Z],
  # який має $bw з двома числовими значеннями (h для двох z-регресорів).
  h <- tryCatch(as.numeric(bw_obj$bw[[1]]$bw), error = function(e) NULL)
  if (is.null(h) || length(h) != 2 || any(!is.finite(h))) return(NULL)
  h
}

if (file.exists("bw_plr.rds")) {
  bw_old  <- readRDS("bw_plr.rds")
  h_saved <- extract_yz_bandwidth(bw_old)

  if (!is.null(h_saved)) {
    cat("Знайдено bw_plr.rds — перевикористовую збережені h без CV.\n")
    cat(sprintf("  h(ln_reputation) = %.4f\n  h(ln_word_count) = %.4f\n",
                h_saved[1], h_saved[2]))
    # npplregbw чекає МАТРИЦЮ бандвідсів: один рядок на регресію
    # (1 для E[Y|Z] + по 1 для кожного E[X_j|Z]). Для пропуску CV
    # передаємо однакові h на всіх рядках — це uniform-bandwidth апроксимація
    # індивідуальних бандвідсів, прийнятна для оцінки лінійних коефіцієнтів.
    n_lin <- length(linear_vars)
    bws_matrix <- matrix(rep(h_saved, n_lin + 1L),
                         nrow = n_lin + 1L, ncol = 2L, byrow = TRUE)
    bw_plr <- npplregbw(
      formula           = formula_plr,
      data              = df_plr_full,
      bws               = bws_matrix,
      bandwidth.compute = FALSE,
      regtype           = "ll"
    )
  } else {
    cat("Не вдалось витягти h з bw_plr.rds — запускаю CV заново.\n")
    bw_plr <- npplregbw(
      formula  = formula_plr,
      data     = df_plr_full,
      regtype  = "ll",
      bwmethod = "cv.ls"
    )
    saveRDS(bw_plr, "bw_plr.rds")
    cat("Збережено: bw_plr.rds\n")
  }
} else {
  cat("Підбір ширин вікон PLR (CV.LS)... Це може тривати ~10-30 хв.\n")
  bw_plr <- npplregbw(
    formula  = formula_plr,
    data     = df_plr_full,
    regtype  = "ll",       # локальна лінійна — стабільніша на краях
    bwmethod = "cv.ls"
  )
  saveRDS(bw_plr, "bw_plr.rds")
  cat("Збережено: bw_plr.rds\n")
}

# 3.3 Оцінка моделі
cat("\nОцінка PLR-моделі...\n")
model_plr <- npplreg(bw_plr)

cat("\n=== Зведення PLR-моделі ===\n")
print(summary(model_plr))


# 4. КОЕФІЦІЄНТИ ЛІНІЙНОЇ ЧАСТИНИ ТА ПОРІВНЯННЯ З ЛАБ.3

plr_coef <- as.numeric(model_plr$xcoef)
plr_se   <- as.numeric(model_plr$xcoeferr)
names(plr_coef) <- linear_vars
names(plr_se)   <- linear_vars

# 95% довірчий інтервал та z-стат
plr_z  <- plr_coef / plr_se
plr_p  <- 2 * pnorm(-abs(plr_z))
plr_lo <- plr_coef - 1.96 * plr_se
plr_hi <- plr_coef + 1.96 * plr_se

# Коефіцієнти з Моделі 5 Лаб.3
lab3_m5 <- c(
  has_code         =  0.410,
  code_x_rep       = -0.038,
  difficulty_score =  2.176,
  quality_score    = -4.676,
  is_weekend       =  0.118,
  post_chatgpt     =  0.151
)

comparison <- tibble(
  Регресор = names(lab3_m5),
  `Лаб.3 M5 (OLS, n=66415)` = round(lab3_m5, 4),
  `Лаб.4 PLR (n=5000)`      = round(plr_coef[names(lab3_m5)], 4),
  `PLR SE`                  = round(plr_se[names(lab3_m5)], 4),
  `95% CI (low)`            = round(plr_lo[names(lab3_m5)], 4),
  `95% CI (high)`           = round(plr_hi[names(lab3_m5)], 4),
  `p-value PLR`             = round(plr_p[names(lab3_m5)], 4)
)

cat("\n", strrep("=", 80), "\n", sep = "")
cat("ПОРІВНЯННЯ КОЕФІЦІЄНТІВ: PLR (Лаб.4) vs OLS (Лаб.3, Модель 5)\n")
cat(strrep("=", 80), "\n", sep = "")
print(comparison, n = Inf)

# Зберігаємо порівняння у CSV для зручної вставки у звіт
write_csv(comparison, "plr_vs_lab3_coefs.csv")
cat("\nЗбережено: plr_vs_lab3_coefs.csv\n")


# 5. ГРАФІКИ ЗАЛЕЖНОСТІ Y ВІД НЕПАРАМЕТРИЧНИХ РЕГРЕСОРІВ
#
# Підхід: будуємо m̂(z₁, z₂) на сітці значень одного регресора, фіксуючи
# другий на медіані. Усі ЛІНІЙНІ регресори також фіксуємо на медіані
# (неперервні) або моді (бінарні), щоб ізолювати лише непараметричний вплив.
# Така ж фіксація — як в Учасниці 1 — для прямої співставності графіків.
#
# Довірчі смуги: для PLR довірча смуга для m̂(z) будується за залишковою
# дисперсією після часткового виключення лінійної частини
# (підхід, описаний у лекції наприкінці теми про PLR).

med_rep   <- median(df_plr$ln_reputation)
med_wc    <- median(df_plr$ln_word_count)
mode_code <- as.integer(round(mean(df_plr$has_code)))
med_diff  <- median(df_plr$difficulty_score)
med_qual  <- median(df_plr$quality_score)
mode_wknd <- as.integer(round(mean(df_plr$is_weekend)))
mode_chat <- as.integer(round(mean(df_plr$post_chatgpt)))

# Для мовних dummy — фіксуємо всі на 0 (базова категорія = javascript як модальна)
lang_fixed <- setNames(rep(0, length(lang_cols)), lang_cols)

cat(sprintf("\nРівні фіксації:\n"))
cat(sprintf("  ln_reputation (медіана)   = %.4f\n", med_rep))
cat(sprintf("  ln_word_count (медіана)   = %.4f\n", med_wc))
cat(sprintf("  has_code (мода)           = %d\n", mode_code))
cat(sprintf("  difficulty_score (мед.)   = %.4f\n", med_diff))
cat(sprintf("  quality_score (мед.)      = %.4f\n", med_qual))
cat(sprintf("  is_weekend (мода)         = %d\n", mode_wknd))
cat(sprintf("  post_chatgpt (мода)       = %d\n", mode_chat))
cat(sprintf("  programming_language      = javascript (базова)\n\n"))


# Допоміжна функція побудови newdata для графіка
build_newdata <- function(grid_x, x_name) {
  base <- tibble(
    ln_reputation = if (x_name == "ln_reputation") grid_x else med_rep,
    ln_word_count = if (x_name == "ln_word_count") grid_x else med_wc,
    has_code      = mode_code,
    code_x_rep    = mode_code * (if (x_name == "ln_reputation") grid_x else med_rep),
    difficulty_score = med_diff,
    quality_score    = med_qual,
    is_weekend       = mode_wknd,
    post_chatgpt     = mode_chat
  )
  # додаємо мовні dummy
  for (lc in lang_cols) base[[lc]] <- 0
  base
}


# 5.1 Графік 1: залежність від ln_reputation
grid_rep <- seq(min(df_plr$ln_reputation),
                max(df_plr$ln_reputation),
                length.out = 200)

newdata_rep <- build_newdata(grid_rep, "ln_reputation")
pred_rep <- predict(model_plr, newdata = newdata_rep, se.fit = TRUE)

plot_rep_df <- tibble(
  x  = grid_rep,
  yhat = pred_rep$fit,
  lo = pred_rep$fit - 1.96 * pred_rep$se.fit,
  hi = pred_rep$fit + 1.96 * pred_rep$se.fit
)

p_rep <- ggplot(plot_rep_df, aes(x = x, y = yhat)) +
  geom_ribbon(aes(ymin = lo, ymax = hi),
              fill = "#2c7fb8", alpha = 0.25) +
  geom_line(colour = "#2c7fb8", linewidth = 1.2) +
  labs(
    title    = "PLR: ln(час відповіді) від репутації автора",
    subtitle = paste0(
      "Лінійні регресори зафіксовано на медіані/моді; ln_word_count = ",
      round(med_wc, 2), "; has_code = ", mode_code,
      "\n(95% довірча смуга)"
    ),
    x = "ln(owner_reputation + 1)",
    y = "ln(first_response_time + 1)"
  ) +
  theme_minimal(base_size = 13)

ggsave("fig_plr_reputation.png", p_rep, width = 10, height = 5.5, dpi = 150)
cat("Збережено: fig_plr_reputation.png\n")


# 5.2 Графік: залежність від ln_word_count
grid_wc <- seq(min(df_plr$ln_word_count),
               max(df_plr$ln_word_count),
               length.out = 200)

newdata_wc <- build_newdata(grid_wc, "ln_word_count")
pred_wc <- predict(model_plr, newdata = newdata_wc, se.fit = TRUE)

plot_wc_df <- tibble(
  x  = grid_wc,
  yhat = pred_wc$fit,
  lo = pred_wc$fit - 1.96 * pred_wc$se.fit,
  hi = pred_wc$fit + 1.96 * pred_wc$se.fit
)

p_wc <- ggplot(plot_wc_df, aes(x = x, y = yhat)) +
  geom_ribbon(aes(ymin = lo, ymax = hi),
              fill = "#2c7fb8", alpha = 0.25) +
  geom_line(colour = "#2c7fb8", linewidth = 1.2) +
  labs(
    title    = "PLR: ln(час відповіді) від обсягу тексту питання",
    subtitle = paste0(
      "Лінійні регресори зафіксовано на медіані/моді; ln_reputation = ",
      round(med_rep, 2), "; has_code = ", mode_code,
      "\n(95% довірча смуга)"
    ),
    x = "ln(body_word_count)",
    y = "ln(first_response_time + 1)"
  ) +
  theme_minimal(base_size = 13)

ggsave("fig_plr_wordcount.png", p_wc, width = 10, height = 5.5, dpi = 150)
cat("Збережено: fig_plr_wordcount.png\n")


# 5.3 Об'єднаний графік A/B (для звіту)
p_combined <- cowplot::plot_grid(p_rep, p_wc, ncol = 2, labels = c("A", "B"))
ggsave("fig_plr_combined.png", p_combined,
       width = 18, height = 5.5, dpi = 150)
cat("Збережено: fig_plr_combined.png\n")


# 6. MSE НА ВАЛІДАЦІЙНІЙ ВИБІРЦІ

cat("\nРозрахунок MSE на валідаційній вибірці...\n")
pred_plr_val <- predict(model_plr, newdata = df_val_full)
mse_plr_val  <- mean((df_val_full$ln_response_time - pred_plr_val)^2, na.rm = TRUE)

# Залишки рахуємо вручну з підігнаних значень (model_plr$mean) — стандартний
# спосіб у np, бо $resid може містити NA для крайових точок ядра.
y_train_hat <- model_plr$mean
y_train     <- df_plr_full$ln_response_time
resid_train <- y_train - y_train_hat
mse_plr_train <- mean(resid_train^2, na.rm = TRUE)
ss_tot <- sum((y_train - mean(y_train))^2, na.rm = TRUE)
ss_res <- sum(resid_train^2, na.rm = TRUE)
r2_plr <- 1 - ss_res / ss_tot

cat(sprintf("R² PLR (навч.):  %.4f\n", r2_plr))
cat(sprintf("MSE PLR (навч.): %.4f\n", mse_plr_train))
cat(sprintf("MSE PLR (вал.):  %.4f\n", mse_plr_val))


# 7. ПІДСУМКОВА ТАБЛИЦЯ

cat("\n", strrep("=", 65), "\n", sep = "")
cat("ПІДСУМОК PLR\n")
cat(strrep("=", 65), "\n", sep = "")

# Бандвідси: bw_plr$bw — складний об'єкт; для E[Y|Z] числа лежать у [[1]]$bw
h_used <- tryCatch(as.numeric(bw_plr$bw[[1]]$bw),
                   error = function(e) c(NA_real_, NA_real_))

summary_plr <- data.frame(
  Показник = c("n (підвибірка)",
               "Число лінійних регресорів",
               "Число непарам. регресорів",
               "h(ln_reputation)",
               "h(ln_word_count)",
               "R² (навч.)",
               "MSE (навч.)",
               "MSE (вал.)"),
  Значення = c(as.character(nrow(df_plr_full)),
               as.character(length(linear_vars)),
               "2",
               sprintf("%.4f", h_used[1]),
               sprintf("%.4f", h_used[2]),
               sprintf("%.4f", r2_plr),
               sprintf("%.4f", mse_plr_train),
               sprintf("%.4f", mse_plr_val))
)
print(summary_plr, row.names = FALSE)

cat("\nОб'єкти для команди:\n")
cat("  model_plr      -> модель PLR\n")
cat("  comparison     -> таблиця порівняння β з Лаб.3 (для звіту)\n")
cat("  mse_plr_val    -> для зведеної таблиці (Учасник 5)\n")

cat("\nЗбережені файли:\n")
for (f in c("bw_plr.rds",
            "plr_vs_lab3_coefs.csv",
            "fig_plr_reputation.png",
            "fig_plr_wordcount.png",
            "fig_plr_combined.png")) {
  cat(" ", f, "\n")
}
cat(strrep("=", 65), "\n", sep = "")
