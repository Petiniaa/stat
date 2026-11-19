# =====================================================================
#  Киног "blockbuster" болгодог хүчин зүйлс — статистик шинжилгээ
#  Өгөгдөл: movie_blockbusters.csv (5,000 кино, 1980–2024)
#  Шаардлагатай багц: nortest, dunn.test
#    install.packages(c("nortest", "dunn.test"))
# =====================================================================
library(nortest)
library(dunn.test)
set.seed(2026)
dir.create("figures", showWarnings = FALSE)
alpha <- 0.05

# ---------------------------------------------------------------------
# 1. ӨГӨГДӨЛ БЭЛТГЭЛ БА ЦЭВЭРЛЭГЭЭ
# ---------------------------------------------------------------------
raw <- read.csv("movie_blockbusters.csv", stringsAsFactors = FALSE,
                na.strings = c("", "NA", "N/A", "null"))
cat("Анхны хэмжээ:", nrow(raw), "мөр,", ncol(raw), "багана\n")
str(raw)

# 1.1 Дутуу утга (NA, хоосон текст, Inf)
na_tbl <- sapply(raw, function(x) sum(is.na(x) | (is.numeric(x) & !is.finite(x))))
print(na_tbl)
cat("Нийт дутуу утга:", sum(na_tbl), "\n")

# 1.2 Давхардал
cat("Бүтэн мөрийн давхардал:", sum(duplicated(raw)), "\n")
cat("movie_id давхардал:", sum(duplicated(raw$movie_id)), "\n")
cat("movie_id-гүйгээр давхардал:", sum(duplicated(raw[, -1])), "\n")

# 1.3 Текст хувьсагчийн цэгцлэл (илүү зай, том/жижиг үсэг)
raw$genre  <- trimws(raw$genre)
raw$decade <- trimws(raw$decade)
print(table(raw$genre))

# 1.4 Логик уялдаа ба утгын муж
chk <- c(
  profit      = sum(abs(raw$profit_usd - (raw$revenue_usd - raw$budget_usd)) > 1),
  roi         = sum(abs(raw$roi - raw$profit_usd / raw$budget_usd) > 0.001),
  blockbuster = sum(raw$blockbuster !=
                    as.integer(raw$revenue_usd > 3 * raw$budget_usd & raw$revenue_usd > 1e8)),
  decade      = sum(raw$decade != paste0(floor(raw$year / 10) * 10, "s")),
  rating_1_10 = sum(raw$rating < 0 | raw$rating > 10),
  year_range  = sum(raw$year < 1980 | raw$year > 2024),
  nonpositive = sum(raw$budget_usd <= 0 | raw$revenue_usd <= 0 | raw$runtime_min <= 0)
)
print(chk)   # бүгд 0 байх ёстой

# 1.5 Хэт хазайсан утга: IQR дүрэм (жинхэнэ ба логарифм хуваарь)
iqr_out <- function(x) {
  q <- quantile(x, c(.25, .75)); i <- diff(q)
  sum(x < q[1] - 1.5 * i | x > q[2] + 1.5 * i)
}
out_tbl <- data.frame(
  variable = c("budget_usd", "revenue_usd", "vote_count", "popularity", "runtime_min", "rating"),
  raw_iqr  = c(iqr_out(raw$budget_usd), iqr_out(raw$revenue_usd), iqr_out(raw$vote_count),
               iqr_out(raw$popularity), iqr_out(raw$runtime_min), iqr_out(raw$rating)),
  log_iqr  = c(iqr_out(log10(raw$budget_usd)), iqr_out(log10(raw$revenue_usd)),
               iqr_out(log10(raw$vote_count)), iqr_out(log10(raw$popularity)), NA, NA))
print(out_tbl)

# 1.6 Бодит байдалд боломжгүй санхүүгийн утга
#   Түүхэн хамгийн өндөр төсөв ≈ $425 сая (Avatar, 2009),
#   хамгийн өндөр орлого ≈ $2.92 тэрбум (Avatar) -> босго: $500 сая ба $3 тэрбум
raw$implausible <- raw$budget_usd > 5e8 | raw$revenue_usd > 3e9
cat("Боломжгүй утгатай мөр:", sum(raw$implausible),
    sprintf("(%.2f%%)", 100 * mean(raw$implausible)), "\n")

# 1.7 Төрөл хөрвүүлэх ба шинэ хувьсагч
movies <- subset(raw, !implausible)
movies$movie_id    <- as.character(movies$movie_id)
movies$genre       <- factor(movies$genre)
movies$decade      <- factor(movies$decade, levels = c("1980s", "1990s", "2000s", "2010s", "2020s"),
                             ordered = TRUE)
movies$blockbuster <- factor(movies$blockbuster, levels = c(0, 1), labels = c("Үгүй", "Тийм"))
movies$budget_m    <- movies$budget_usd / 1e6           # сая $
movies$revenue_m   <- movies$revenue_usd / 1e6
movies$log_budget  <- log10(movies$budget_usd)
movies$log_revenue <- log10(movies$revenue_usd)
movies$log_multiple <- log10(movies$revenue_usd / movies$budget_usd)  # = log10(1 + ROI)
movies$implausible <- NULL
cat("Цэвэрлэсэн өгөгдөл:", nrow(movies), "мөр\n")
str(movies)
write.csv(movies, "movie_blockbusters_clean.csv", row.names = FALSE)

# ---------------------------------------------------------------------
# 2. ТАРХАЛТ БА НОРМАЛ БАЙДАЛ
# ---------------------------------------------------------------------
skew <- function(x) { m <- mean(x); s <- sd(x); mean((x - m)^3) / s^3 }
kurt <- function(x) { m <- mean(x); s <- sd(x); mean((x - m)^4) / s^4 - 3 }
mode_val <- function(x) { t <- table(x); as.numeric(names(t)[which.max(t)]) }
describe <- function(x) c(
  n = length(x), mean = mean(x), median = median(x), mode = mode_val(round(x, 1)),
  sd = sd(x), var = var(x), IQR = IQR(x), min = min(x), max = max(x),
  range = diff(range(x)), CV = sd(x) / mean(x) * 100, skew = skew(x), kurt = kurt(x))
vars <- c("runtime_min", "rating", "budget_m", "revenue_m", "roi",
          "log_budget", "log_revenue", "log_multiple")
desc_tbl <- t(sapply(movies[vars], describe))
print(round(desc_tbl, 3))

norm_tbl <- t(sapply(movies[vars], function(x) c(
  SW_W = unname(shapiro.test(x)$statistic), SW_p = shapiro.test(x)$p.value,
  Lillie_D = unname(lillie.test(x)$statistic), Lillie_p = lillie.test(x)$p.value,
  AD_A = unname(ad.test(x)$statistic), AD_p = ad.test(x)$p.value)))
print(signif(norm_tbl, 3))

# Графикууд: гистограмм + QQ
plot_hq <- function(dat, name, col) {
  m <- mean(dat); s <- sd(dat)
  hist(dat, breaks = 40, freq = FALSE, col = col, border = "white",
       main = name, xlab = name, ylab = "Нягт")
  curve(dnorm(x, m, s), add = TRUE, col = "red", lwd = 2)   # ижил дундаж, SD-тэй хэвийн муруй
  qqnorm(dat, main = paste(name, ": Q-Q"), xlab = "Онолын квантиль",
         ylab = "Түүврийн квантиль", pch = 20, col = col)
  qqline(dat, col = "red", lwd = 2)
}
png("figures/fig1_money.png", 1800, 1250, res = 160, type = "cairo")
par(mfrow = c(2, 4), mar = c(4.2, 4.2, 2.8, 1))
plot_hq(movies$budget_m,    "Төсөв (сая $)",       "steelblue")
plot_hq(movies$log_budget,  "log10(төсөв)",        "steelblue")
plot_hq(movies$revenue_m,   "Орлого (сая $)",      "darkorange")
plot_hq(movies$log_revenue, "log10(орлого)",       "darkorange")
dev.off()
png("figures/fig2_rating_runtime.png", 1800, 650, res = 160, type = "cairo")
par(mfrow = c(1, 4), mar = c(4.2, 4.2, 2.8, 1))
plot_hq(movies$rating,      "Үнэлгээ",             "seagreen")
plot_hq(movies$runtime_min, "Үргэлжлэх хугацаа",   "mediumpurple")
dev.off()

# ---------------------------------------------------------------------
# 3. БҮЛГҮҮДИЙГ ХАРЬЦУУЛАХ
# ---------------------------------------------------------------------
# Levene (Brown-Forsythe, медиан төвтэй) = car::leveneTest(..., center = median)
levene_bf <- function(y, g) {
  d <- abs(y - ave(y, g, FUN = median)); a <- anova(lm(d ~ g))
  c(F = a$`F value`[1], df1 = a$Df[1], df2 = a$Df[2], p = a$`Pr(>F)`[1])
}

# ---- 3.1 Асуулт 1: Blockbuster ба бусад киноны үнэлгээ ялгаатай юу? ----
# H0: хоёр бүлгийн үнэлгээний тархалт (медиан) ижил; H1: ялгаатай
r_yes <- movies$rating[movies$blockbuster == "Тийм"]
r_no  <- movies$rating[movies$blockbuster == "Үгүй"]
print(rbind(Тийм = describe(r_yes), Үгүй = describe(r_no))[, c("n", "mean", "median", "sd", "skew", "kurt")])
shapiro.test(r_yes); shapiro.test(r_no)
print(levene_bf(movies$rating, movies$blockbuster))
mw <- wilcox.test(r_yes, r_no, conf.int = TRUE); print(mw)
z  <- qnorm(mw$p.value / 2); cat("Эффект r =", abs(z) / sqrt(length(r_yes) + length(r_no)), "\n")
wt <- t.test(r_yes, r_no); print(wt)                       # Welch (харьцуулах)
sp <- sqrt(((length(r_yes) - 1) * var(r_yes) + (length(r_no) - 1) * var(r_no)) /
             (length(r_yes) + length(r_no) - 2))
cat("Cohen's d =", (mean(r_yes) - mean(r_no)) / sp, "\n")

png("figures/fig3_rating_bb.png", 1700, 650, res = 160, type = "cairo")
par(mfrow = c(1, 2), mar = c(4.2, 4.5, 2.8, 1))
boxplot(rating ~ blockbuster, data = movies, col = c("grey80", "gold"),
        xlab = "Blockbuster", ylab = "Үнэлгээ (1–10)", main = "Үнэлгээ: blockbuster эсэхээр")
points(1:2, c(mean(r_no), mean(r_yes)), pch = 18, cex = 2, col = "red")
plot(ecdf(r_no), do.points = FALSE, verticals = TRUE, col = "grey40", lwd = 2,
     main = "Үнэлгээний ECDF", xlab = "Үнэлгээ", ylab = "F(x)")
plot(ecdf(r_yes), do.points = FALSE, verticals = TRUE, col = "goldenrod3", lwd = 2, add = TRUE)
legend("topleft", c("Үгүй", "Тийм"), col = c("grey40", "goldenrod3"), lwd = 2, bty = "n")
dev.off()

# ---- 3.2 Асуулт 2: Киноны орлого жанраас хамаарч ялгаатай юу? ----
# H0: бүх жанрын log10(орлого)-ийн дундаж тэнцүү; H1: дор хаяж нэг нь ялгаатай
grp_tbl <- t(sapply(split(movies$log_revenue, movies$genre), function(x)
  c(n = length(x), mean = mean(x), sd = sd(x), median = median(x),
    geo_mean_m = 10^mean(x) / 1e6, SW_p = shapiro.test(x)$p.value)))
print(round(grp_tbl, 4))
fit <- aov(log_revenue ~ genre, data = movies)
shapiro.test(residuals(fit))
cat("Үлдэгдлийн skew/kurt:", skew(residuals(fit)), kurt(residuals(fit)), "\n")
print(levene_bf(movies$log_revenue, movies$genre))
bartlett.test(log_revenue ~ genre, data = movies)
cat("SD харьцаа (max/min):", max(grp_tbl[, "sd"]) / min(grp_tbl[, "sd"]), "\n")
summary(fit)
ss <- summary(fit)[[1]]$`Sum Sq`; cat("eta^2 =", ss[1] / sum(ss), "\n")
oneway.test(log_revenue ~ genre, data = movies)            # Welch ANOVA

# Games-Howell функц (дисперс тэнцүү биш үед хэрэглэдэг post-hoc)
games_howell <- function(y, g) {
  g <- droplevels(g); lv <- levels(g); k <- length(lv)
  m <- tapply(y, g, mean); v <- tapply(y, g, var); n <- tapply(y, g, length)
  out <- NULL
  for (i in 1:(k - 1)) for (j in (i + 1):k) {
    se <- sqrt(v[i] / n[i] + v[j] / n[j])
    df <- (v[i] / n[i] + v[j] / n[j])^2 /
          ((v[i] / n[i])^2 / (n[i] - 1) + (v[j] / n[j])^2 / (n[j] - 1))
    d  <- m[j] - m[i]; q <- abs(d) / se * sqrt(2)
    hw <- qtukey(0.95, k, df) / sqrt(2) * se
    out <- rbind(out, data.frame(pair = paste(lv[j], "-", lv[i]), diff = d,
                                 lwr = d - hw, upr = d + hw,
                                 p_adj = ptukey(q, k, df, lower.tail = FALSE)))
  }
  rownames(out) <- NULL; out
}
# TukeyHSD-ийн мөрийн нэрийг ("Sci-Fi-Action") хоёр жанр болгон салгах
split_pair <- function(name, lv) {
  for (a in lv) for (b in lv) if (a != b && name == paste(a, b, sep = "-")) return(c(a, b))
}
# Үндсэн post-hoc: Tukey HSD (дисперс тэнцүү нөхцөл хангагдсан)
tk <- TukeyHSD(fit)$genre
print(round(tk[order(tk[, "p adj"]), ], 4))
cat("Tukey: ялгаатай хос", sum(tk[, "p adj"] < alpha), "/", nrow(tk), "\n")
# Шалгах: Games-Howell
gh <- games_howell(movies$log_revenue, movies$genre)
cat("Games-Howell: ялгаатай хос", sum(gh$p_adj < alpha), "/", nrow(gh), "\n")

# Параметргүй хувилбар: Kruskal-Wallis + Dunn (Holm)
kw <- kruskal.test(log_revenue ~ genre, data = movies); print(kw)
cat("epsilon^2 =", unname(kw$statistic) / (nrow(movies) - 1), "\n")
dn <- dunn.test(movies$log_revenue, movies$genre, method = "holm", kw = FALSE,
                table = FALSE, altp = TRUE)
cat("Dunn (Holm): ялгаатай хос", sum(dn$altP.adjusted < alpha), "/", length(dn$altP.adjusted), "\n")
# Гурван аргын дүгнэлт давхцаж байгаа эсэх
key <- function(a, b) paste(sort(c(a, b)), collapse = "|")
sig_tk <- sapply(rownames(tk), function(r) { p <- split_pair(r, levels(movies$genre)); key(p[1], p[2]) })[tk[, "p adj"] < alpha]
sig_gh <- sapply(strsplit(gh$pair, " - "), function(p) key(p[1], p[2]))[gh$p_adj < alpha]
sig_dn <- sapply(strsplit(dn$comparisons, " - "), function(p) key(p[1], p[2]))[dn$altP.adjusted < alpha]
cat("Tukey = Games-Howell:", setequal(sig_tk, sig_gh), "; Tukey = Dunn:", setequal(sig_tk, sig_dn), "\n")
print(setdiff(union(sig_tk, sig_dn), intersect(sig_tk, sig_dn)))

png("figures/fig4_revenue_genre.png", 1800, 700, res = 160, type = "cairo")
par(mfrow = c(1, 2), mar = c(4.2, 7.5, 2.8, 1))
ord <- names(sort(tapply(movies$log_revenue, movies$genre, mean)))
boxplot(log_revenue ~ factor(genre, levels = ord), data = movies, horizontal = TRUE,
        las = 1, col = "lightsteelblue", xlab = "log10(орлого, $)", ylab = "",
        main = "Орлого жанраар", outcex = 0.4)
abline(v = 8, lty = 2, col = "grey50")
ci <- t(sapply(ord, function(gn) { x <- movies$log_revenue[movies$genre == gn]
  mean(x) + c(0, -1, 1) * qt(.975, length(x) - 1) * sd(x) / sqrt(length(x)) }))
plot(10^ci[, 1] / 1e6, 1:10, log = "x", xlim = range(10^ci / 1e6), yaxt = "n", pch = 19,
     ylab = "", xlab = "Геометр дундаж орлого, сая $ (log хуваарь)",
     main = "Геометр дундаж ба 95% ИИ", col = "navy")
arrows(10^ci[, 2] / 1e6, 1:10, 10^ci[, 3] / 1e6, 1:10, angle = 90, code = 3, length = .04, col = "navy")
axis(2, at = 1:10, labels = ord, las = 1)
dev.off()

# Tukey HSD p-утгын дулааны зураг
pm <- matrix(NA, 10, 10, dimnames = list(levels(movies$genre), levels(movies$genre)))
for (r in rownames(tk)) { p <- split_pair(r, levels(movies$genre))
  pm[p[1], p[2]] <- pm[p[2], p[1]] <- tk[r, "p adj"] }
png("figures/fig5_posthoc.png", 1500, 1150, res = 160, type = "cairo")
par(mar = c(7, 7.5, 3, 1))
lv <- ord; pmo <- pm[lv, lv]
image(1:10, 1:10, t(ifelse(pmo < alpha, 1, 0))[, 10:1], col = c("grey92", "#f4a261"),
      axes = FALSE, xlab = "", ylab = "", main = "Tukey HSD: засварласан p-утга (өнгөтэй = p < 0.05)")
for (i in 1:10) for (j in 1:10) if (i != j)
  text(j, 11 - i, ifelse(pmo[i, j] < 0.001, "<.001", sprintf("%.3f", pmo[i, j])), cex = .7)
axis(1, 1:10, lv, las = 2, cex.axis = .85); axis(2, 10:1, lv, las = 1, cex.axis = .85)
dev.off()

# ---- 3.3 Асуулт 3: Blockbuster болох магадлал жанраас хамаарах уу? ----
# H0: жанр ба blockbuster хамааралгүй; H1: хамааралтай
tab <- table(movies$genre, movies$blockbuster); print(tab)
rate <- prop.table(tab, 1)[, "Тийм"]; print(round(sort(rate, decreasing = TRUE) * 100, 1))
ch <- chisq.test(tab); print(ch)
cat("Хүлээгдэж буй давтамж < 5:", sum(ch$expected < 5), "\n")
cat("Cramér's V =", sqrt(unname(ch$statistic) / (sum(tab) * (min(dim(tab)) - 1))), "\n")
ch_mc <- chisq.test(tab, simulate.p.value = TRUE, B = 10000); print(ch_mc)
print(round(ch$stdres[, "Тийм"], 2))                         # стандартчилсан үлдэгдэл
# Post-hoc: хос жанр бүрт Fisher-ийн яг тест (Documentary-д 0 давтамжтай тул), Holm засвар
g_lv <- rownames(tab); pf <- NULL
for (i in 1:9) for (j in (i + 1):10)
  pf <- rbind(pf, data.frame(a = g_lv[i], b = g_lv[j],
                             p = fisher.test(tab[c(i, j), ])$p.value))
pf$p_holm <- p.adjust(pf$p, "holm")
cat("Fisher (Holm): ялгаатай хос", sum(pf$p_holm < alpha), "/", nrow(pf), "\n")
print(pf[pf$p_holm >= alpha, ], digits = 3)                 # ялгаагүй хосууд

png("figures/fig6_bb_rate.png", 1500, 700, res = 160, type = "cairo")
par(mar = c(4.2, 7.5, 2.8, 1))
o <- order(rate); n_g <- rowSums(tab)[o]; p_g <- rate[o]
lo <- sapply(seq_along(o), function(i) binom.test(tab[o[i], "Тийм"], n_g[i])$conf.int[1])
hi <- sapply(seq_along(o), function(i) binom.test(tab[o[i], "Тийм"], n_g[i])$conf.int[2])
b <- barplot(p_g * 100, horiz = TRUE, las = 1, col = "gold", border = NA, xlim = c(0, 40),
             xlab = "Blockbuster болсон хувь, % (95% ИИ)", main = "Blockbuster хувь жанраар")
arrows(lo * 100, b, hi * 100, b, angle = 90, code = 3, length = .03)
abline(v = mean(movies$blockbuster == "Тийм") * 100, lty = 2, col = "red")
text(hi * 100 + 0.6, b, sprintf("%.1f%%", p_g * 100), cex = .8, adj = 0)
dev.off()

# ---------------------------------------------------------------------
# 4. МЭДРЭМЖИЙН ШАЛГАЛТ: хассан 39 мөрийг буцааж оруулахад үр дүн өөрчлөгдөх үү?
# ---------------------------------------------------------------------
full <- raw; full$genre <- factor(full$genre)
cat("Дундаж төсөв (сая $): бүтэн =", mean(full$budget_usd) / 1e6, " цэвэр =", mean(movies$budget_m), "\n")
cat("Дундаж орлого (сая $): бүтэн =", mean(full$revenue_usd) / 1e6, " цэвэр =", mean(movies$revenue_m), "\n")
cat("Blockbuster хувь: бүтэн =", mean(full$blockbuster), " цэвэр =", mean(movies$blockbuster == "Тийм"), "\n")
cat("Бүтэн өгөгдөл — MW p:",
    wilcox.test(rating ~ blockbuster, data = full)$p.value, "\n")
cat("Бүтэн өгөгдөл — Welch ANOVA p:",
    oneway.test(log10(revenue_usd) ~ genre, data = full)$p.value, "\n")
cat("Бүтэн өгөгдөл — KW p:",
    kruskal.test(log10(revenue_usd) ~ genre, data = full)$p.value, "\n")
cat("Бүтэн өгөгдөл — chi-square p:",
    chisq.test(table(full$genre, full$blockbuster))$p.value, "\n")
