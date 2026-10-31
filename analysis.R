# Data Literacy · complete analysis in one script (base R only)
#
#   Rscript analysis.R                                prints all results to the console
#   quarto render docs/report.qmd --output-dir ..     builds report.pdf
#
# Steps: 1 download → 2 prepare → 3 describe → 4 test → 5 check robustness → 6 plot

invisible(Sys.setlocale("LC_CTYPE", "en_US.UTF-8"))
ALPHA <- .05
set.seed(2026)   # jitter in figure 3

# --- 1 · Download ---------------------------------------------------------------
# Stack Overflow Developer Survey 2026, license: ODbL 1.0
# Pinned to a fixed commit and verified by checksum, so results cannot change silently.
raw_file <- "data/survey_results_public.csv"
raw_url  <- paste0("https://github.com/StackExchange/Survey/raw/",
                   "9dd5c163b7ec09ce5503f711c37a99dbd80ff3c7/",
                   "packages/archive/2026/survey_results_public.csv")
raw_md5  <- "4f12b68104bd2dadac6bd44f413f35d2"

if (!file.exists(raw_file)) {
  dir.create("data", showWarnings = FALSE)
  options(timeout = 600)
  download.file(raw_url, raw_file, mode = "wb", quiet = TRUE)
}
stopifnot("data file differs from the pinned version" = tools::md5sum(raw_file) == raw_md5)

raw <- read.csv(raw_file, encoding = "UTF-8", na.strings = c("NA", ""))
names(raw)[1] <- "ResponseId"   # strip BOM from first column name

# --- 2 · Prepare: select → group → clean → recode → build scale --------------------
items <- paste0("AIImpact_", 1:7)   # usefulness of AI for 7 tasks
dat <- raw[, c("Role", "WorkExp", "AISelect", "AISentiment", items)]

roles_platform <- c("DevOps engineer or professional", "Cloud infrastructure engineer",
                    "Site reliability engineer", "Platform engineer",
                    "System administrator", "Cybersecurity or InfoSec professional")
roles_dev <- c(grep("^Developer, ", unique(raw$Role), value = TRUE), "Software engineer")

dat$group <- NA
dat$group[dat$Role %in% roles_platform] <- "Platform & Security"
dat$group[dat$Role %in% roles_dev]      <- "Software Development"
dat$group <- factor(dat$group, levels = c("Software Development", "Platform & Security"))
dat <- dat[!is.na(dat$group), ]

dat$WorkExp <- as.numeric(dat$WorkExp)
n_exp_removed <- sum(dat$WorkExp > 50, na.rm = TRUE)
dat$WorkExp[dat$WorkExp > 50] <- NA   # implausible answers

# AI use at work: TRUE = uses AI tools, FALSE = "I don't use AI tools", NA = not answered
dat$ai_user <- ifelse(is.na(dat$AISelect), NA, !grepl("use AI tools", dat$AISelect))

# Attitude: text → 1–5 ("Not sure" → NA)
dat$attitude <- match(dat$AISentiment, c("Very unfavorable", "Somewhat unfavorable",
                                         "Neutral", "Somewhat favorable", "Very favorable"))

# Usefulness items: text → 1–4 ("Not applicable" → NA).
# "No strong opinion" is placed between "not useful" and "useful" (checked in step 5).
useful_levels <- c("More impractical than useful, not useful", "No strong opinion",
                   "Useful", "Very useful")
raw_items <- dat[, items]
for (item in items) dat[[item]] <- match(dat[[item]], useful_levels)

# Scale: mean of the 7 items, if at least 4 are answered
build_scale <- function(x) {
  s <- rowMeans(x, na.rm = TRUE)
  s[rowSums(!is.na(x)) < 4] <- NA
  s
}
dat$usefulness <- build_scale(dat[, items])

# Reliability: Cronbach's alpha (complete cases)
cc_items <- na.omit(dat[, items])
alpha   <- 7 / 6 * (1 - sum(apply(cc_items, 2, var)) / var(rowSums(cc_items)))
alpha_n <- nrow(cc_items)

# --- 3 · Describe ----------------------------------------------------------------
describe <- function(var) {
  agg <- aggregate(dat[[var]], list(group = dat$group), function(x) {
    x <- na.omit(x)
    c(n = length(x), M = mean(x), SD = sd(x), Mdn = median(x))
  })
  data.frame(variable = var, group = agg$group, agg$x)
}
desc <- do.call(rbind, lapply(c("attitude", "usefulness", "WorkExp"), describe))
platform_roles <- sort(table(dat$Role[dat$group == "Platform & Security"]), decreasing = TRUE)

# Who answered the AI questions? Almost only people who use AI.
nonusers <- dat[dat$ai_user %in% FALSE, ]
nonuser_answered <- c(n = nrow(nonusers),
                      attitude = sum(!is.na(nonusers$attitude)),
                      usefulness = sum(!is.na(nonusers$usefulness)))

# --- 4 · Test hypotheses (directional; first group = Software Development) --------
# Effect sizes: r_rb (rank-biserial) for Wilcoxon, Cohen's d for t-test
rank_biserial <- function(test, data, var) {
  n <- table(data$group[!is.na(data[[var]])])
  unname(2 * test$statistic / prod(n) - 1)
}
cohens_d <- function(data, var) {
  g <- lapply(split(data[[var]], data$group), na.omit)
  sp <- sqrt(((length(g[[1]]) - 1) * var(g[[1]]) + (length(g[[2]]) - 1) * var(g[[2]])) /
             (length(g[[1]]) + length(g[[2]]) - 2))
  (mean(g[[1]]) - mean(g[[2]])) / sp
}

# H1: Platform & Security hold a less favorable attitude      → Wilcoxon (ordinal)
h1 <- wilcox.test(attitude ~ group, dat, alternative = "greater", exact = FALSE)
h1$r_rb <- rank_biserial(h1, dat, "attitude")
h1$n    <- sum(!is.na(dat$attitude))

# H2: Platform & Security rate AI as less useful              → Welch t-test (metric)
h2 <- t.test(usefulness ~ group, dat, alternative = "greater")
h2$d <- cohens_d(dat, "usefulness")
h2$n <- sum(!is.na(dat$usefulness))

# H3: more work experience → lower usefulness                 → Spearman correlation
h3 <- cor.test(dat$WorkExp, dat$usefulness, method = "spearman", alternative = "less", exact = FALSE)
h3$n <- sum(complete.cases(dat$WorkExp, dat$usefulness))

# Three tests on one question → Holm correction
p_holm <- p.adjust(c(H1 = h1$p.value, H2 = h2$p.value, H3 = h3$p.value), method = "holm")
supported <- p_holm < ALPHA

# Exploratory (not planned, therefore not part of the Holm family)
h3x <- cor.test(dat$WorkExp, dat$usefulness, method = "spearman", exact = FALSE)   # two-sided
nonuse_tab <- table(dat$group, factor(!dat$ai_user, labels = c("uses AI", "does not use AI")))
nonuse <- chisq.test(nonuse_tab)
nonuse$phi   <- sqrt(unname(nonuse$statistic) / sum(nonuse_tab))
nonuse$share <- prop.table(nonuse_tab, 1)[, "does not use AI"]

# --- 5 · Robustness ------------------------------------------------------------------
# (a) without system administrators, the most debatable member of the platform group
no_sysadmin <- dat[dat$Role != "System administrator", ]
# (b) "No strong opinion" treated as missing instead of as a middle category
alt <- raw_items
for (item in items) alt[[item]] <- match(alt[[item]], useful_levels[-2])
dat$usefulness_alt <- build_scale(alt)

robust <- data.frame(
  check = c("H1 without system administrators", "H2 without system administrators",
            "H2 with \"No strong opinion\" as missing"),
  p = c(wilcox.test(attitude ~ group, no_sysadmin, alternative = "greater", exact = FALSE)$p.value,
        t.test(usefulness ~ group, no_sysadmin, alternative = "greater")$p.value,
        t.test(usefulness_alt ~ group, dat, alternative = "greater")$p.value),
  effect = c(rank_biserial(wilcox.test(attitude ~ group, no_sysadmin, exact = FALSE),
                           no_sysadmin, "attitude"),
             cohens_d(no_sysadmin, "usefulness"),
             cohens_d(dat, "usefulness_alt"))
)

# --- 6 · Plot (called by the report) ----------------------------------------------
COL <- c(blue = "#2a78d6", orange = "#eb6834", ink = "#0b0b0b", muted = "#898781")
style <- function(...) par(col.axis = COL["muted"], fg = COL["muted"], las = 1, font.main = 1, ...)

plot_attitude <- function() {
  style(mar = c(7, 11, 3, 1))
  tab <- prop.table(table(dat$group, factor(dat$attitude, levels = 1:5)), 1) * 100
  fills <- c("#d03b3b", "#f1a3a2", "#c9c8c1", "#86b6ef", "#2a78d6")
  barplot(t(tab[2:1, ]), horiz = TRUE, col = fills, border = "white", xlim = c(0, 100),
          xlab = "Share of respondents [%]", main = "Attitude toward AI tools at work (AI users)")
  legend(x = grconvertX(.5, "ndc", "user"), y = grconvertY(.02, "ndc", "user"),
         xjust = .5, yjust = 0, xpd = NA, horiz = TRUE, bty = "n", fill = fills, border = NA,
         cex = .75, legend = c("very unfavorable", "somewhat unfav.", "neutral",
                               "somewhat fav.", "very favorable"))
}

plot_usefulness <- function() {
  style()
  g  <- lapply(split(dat$usefulness, dat$group), na.omit)
  m  <- sapply(g, mean)
  se <- sapply(g, function(x) sd(x) / sqrt(length(x)))
  b <- barplot(m, col = COL[c("blue", "orange")], border = NA, ylim = c(1, 4), xpd = FALSE,
               ylab = "Perceived usefulness (1–4)", main = "Usefulness of AI across 7 tasks")
  arrows(b, m - se, b, m + se, code = 3, length = .08, angle = 90, col = COL["ink"])
  text(b, m + .15, sprintf("M = %.2f", m), col = COL["ink"])
}

plot_experience <- function() {
  style()
  cc <- na.omit(dat[, c("WorkExp", "usefulness")])
  plot(jitter(cc$WorkExp, .6), jitter(cc$usefulness, .6), pch = 16, cex = .4,
       col = adjustcolor(COL["blue"], .12), ylim = c(1, 4),
       xlab = "Work experience [years]", ylab = "Perceived usefulness (1–4)",
       main = "Work experience and usefulness of AI")
  abline(lm(usefulness ~ WorkExp, cc), lwd = 2)
}

# --- Console summary (only when run directly) ---------------------------------------
if (sys.nframe() == 0) {
  cat(sprintf("Sample: %d of %d responses · Cronbach's alpha = %.2f (n = %d)\n\n",
              nrow(dat), nrow(raw), alpha, alpha_n))
  print(desc, digits = 3, row.names = FALSE)
  cat(sprintf("\nNon-users of AI who answered: attitude %d, usefulness %d of %d\n",
              nonuser_answered["attitude"], nonuser_answered["usefulness"], nonuser_answered["n"]))
  cat(sprintf("\nH1 Wilcoxon  W = %.0f, p = %.3f, p_Holm = %.3f, r_rb = %.2f",
              h1$statistic, h1$p.value, p_holm["H1"], h1$r_rb))
  cat(sprintf("\nH2 Welch t   t(%.2f) = %.2f, p = %.3f, p_Holm = %.3f, d = %.2f",
              h2$parameter, h2$statistic, h2$p.value, p_holm["H2"], h2$d))
  cat(sprintf("\nH3 Spearman  rho = %.2f, p = %.3f, p_Holm = %.3f (two-sided: p = %.2g)",
              h3$estimate, h3$p.value, p_holm["H3"], h3x$p.value))
  cat(sprintf("\nNon-use      %.1f%% vs %.1f%%, chi2 = %.2f, p = %.4f, phi = %.2f\n\n",
              100 * nonuse$share[1], 100 * nonuse$share[2], nonuse$statistic,
              nonuse$p.value, nonuse$phi))
  print(robust, digits = 3, row.names = FALSE)
}
