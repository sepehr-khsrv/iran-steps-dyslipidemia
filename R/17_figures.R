# 17_figures.R : Figures 1-3 (standardised prevalence by sex, age group, area) and Figure S1 (provincial maps).
source("R/09_functions.R")  # path relative to the repository root
suppressMessages({library(ggplot2); library(sf); library(patchwork)})
sink(file.path(W, "output/logs/17_figures.log"), split = TRUE)
dir.create(FIG, showWarnings = FALSE)
prev <- readRDS(file.path(RDS, "prev_main.rds")); pv <- readRDS(file.path(RDS, "provinces.rds"))
OI <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
save_fig <- function(p, name, w = 7.5, h = 4.5) {
  # PLOS: TIFF, LZW, width <= 2250 px: 7.5 in x 300 dpi = 2250 px
  ragg::agg_tiff(file.path(FIG, paste0(name, ".tiff")), width = w, height = h, units = "in", res = 300, compression = "lzw", background = "white")
  print(p); invisible(dev.off())
  ragg::agg_png(file.path(FIG, paste0(name, ".png")), width = w, height = h, units = "in", res = 150, background = "white")
  print(p); invisible(dev.off())
}
trend_fig <- function(an, levels, labels, cols, legend_title) {
  x <- prev %>% filter(analysis == an, type == "standardised") %>% mutate(level = factor(level, levels, labels))
  stopifnot(all(x$lower >= 50), all(x$upper <= 100))
  pd <- position_dodge(width = 0.9)
  ggplot(x, aes(round, est, colour = level, group = level)) +
    geom_line(linewidth = 0.4, position = pd) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.6, linewidth = 0.5, position = pd) +
    geom_point(size = 2, position = pd) +
    scale_x_continuous("Survey year", breaks = YEARS, limits = c(2005.5, 2022.5)) +
    scale_y_continuous("Prevalence of dyslipidemia (%)", limits = c(50, 100), breaks = seq(50, 100, 10), expand = c(0, 0)) +
    scale_colour_manual(legend_title, values = cols) +
    theme_classic(base_size = 12) + theme(legend.position = "right")
}
f1 <- trend_fig("sex", c("women", "men"), c("Women", "Men"), OI[c(6, 5)], "Sex")
f2 <- trend_fig("agegrp", AGE_LEV, AGE_LEV, OI[c(1, 2, 3, 6)], "Age group (years)")
f3 <- trend_fig("area", c("urban", "rural"), c("Urban", "Rural"), OI[c(7, 3)], "Area of residence")
save_fig(f1, "Fig1_sex"); save_fig(f2, "Fig2_agegroup"); save_fig(f3, "Fig3_area")

# ---- Figure S1: provincial maps ----
shp <- file.path(W, "data_raw/maps/ne10m/ne_10m_admin_1_states_provinces.shp")
if (!file.exists(shp)) {
  zf <- file.path(W, "data_raw/maps/ne_10m_admin_1.zip"); dir.create(dirname(zf), showWarnings = FALSE, recursive = TRUE)
  download.file("https://naciscdn.org/naturalearth/10m/cultural/ne_10m_admin_1_states_provinces.zip", zf, mode = "wb")
  unzip(zf, exdir = file.path(W, "data_raw/maps/ne10m"))
}
ne <- st_read(shp, quiet = TRUE) %>% filter(adm0_a3 == "IRN") %>% select(name, name_en)
cat("Natural Earth version:", readLines(sub("\\.shp$", ".VERSION.txt", shp))[1], "; Iran admin-1 units:", nrow(ne), "\n")
cw <- data.frame(ne_name = ne$name, ne_name_en = ne$name_en, province31 = ne$name_en, stringsAsFactors = FALSE)
cw$province31[cw$ne_name_en == "West Azarbaijan"] <- "West Azerbaijan"
stopifnot(nrow(ne) == 31, setequal(cw$province31, levels(pv$full$prov)))
cw$match <- ifelse(cw$ne_name_en == cw$province31, "exact (name_en)", "recoded")
write_csv(cw, file.path(W, "docs/map_name_crosswalk.csv")); print(cw)
ne$province31 <- cw$province31
num <- data.frame(province31 = levels(pv$full$prov)) %>% arrange(province31) %>% mutate(id = row_number())
mp <- pv$full %>% mutate(province31 = as.character(prov)) %>% select(province31, round, est, type)
brks <- seq(55, 95, 5)
stopifnot(all(mp$est[!is.na(mp$est)] > min(brks) & mp$est[!is.na(mp$est)] < max(brks)))
lab <- paste0(formatC(head(brks, -1), format = "f", digits = 1), "-", formatC(tail(brks, -1) - 0.1, format = "f", digits = 1)); NA_LAB <- "No estimate"
geo <- ne %>% select(province31) %>% left_join(num, by = "province31")
md <- merge(geo, mp, by = "province31") %>% mutate(cat = cut(est, brks, labels = lab, right = FALSE),
  cat = factor(ifelse(is.na(cat), NA_LAB, as.character(cat)), c(lab, NA_LAB)), year = factor(round, YEARS))
stopifnot(all(md$cat[md$province31 == "Alborz" & md$round == 2007] == NA_LAB), all(md$cat[md$province31 == "Qom" & md$round == 2016] == NA_LAB))
pal <- c(setNames(viridisLite::viridis(length(lab), direction = -1), lab), setNames("grey85", NA_LAB))
pmap <- ggplot(md) + geom_sf(aes(fill = cat), colour = "white", linewidth = 0.15) +
  facet_wrap(~year, ncol = 2) +
  scale_fill_manual("Standardized prevalence (%)", values = pal, drop = FALSE) +
  theme_void(base_size = 10) + theme(legend.position = "right", strip.text = element_text(size = 11, face = "bold"),
                                     legend.title = element_text(size = 9), legend.text = element_text(size = 8)) +
  guides(fill = guide_legend(ncol = 1))
pts <- suppressWarnings(suppressMessages(st_point_on_surface(geo)))
pts <- cbind(pts, st_coordinates(pts))
kmap <- ggplot() + geom_sf(data = geo, fill = "grey96", colour = "grey55", linewidth = 0.2) +
  ggrepel::geom_text_repel(data = pts, aes(X, Y, label = id), size = 2.4, min.segment.length = 0, segment.size = 0.2, max.overlaps = Inf, seed = 1) +
  labs(title = "Province key") + theme_void(base_size = 10) + theme(plot.title = element_text(size = 10, face = "bold"))
key <- num %>% mutate(txt = paste0(id, "  ", province31), col = (id - 1) %/% 16, row = (id - 1) %% 16)
ktxt <- ggplot(key, aes(x = col, y = -row, label = txt)) + geom_text(hjust = 0, size = 2.5) +
  scale_x_continuous(limits = c(0, 2.05)) + scale_y_continuous(limits = c(-15.5, 0.5)) + theme_void()
pS1 <- pmap / (kmap + ktxt + plot_layout(widths = c(1, 1.3))) + plot_layout(heights = c(2.3, 1)) +
  plot_annotation(caption = "Direct standardization to the 2016 census (age group x sex). 2007: Tehran includes Alborz; Kurdistan has no laboratory data.\n2016: Qom not surveyed. Boundaries: Natural Earth 1:10m admin-1 (public domain).",
                  theme = theme(plot.caption = element_text(size = 7.5, hjust = 0)))
save_fig(pS1, "FigS1_provinces", w = 7.5, h = 8.7)
writeLines(c("Figure files: output/figures/Fig1_sex, Fig2_agegroup, Fig3_area (7.5 x 4.5 in), FigS1_provinces (7.5 x 8.7 in): .tiff 300 dpi LZW, 2250 px wide; .png previews 150 dpi.",
             "Map source: Natural Earth 1:10m Cultural Vectors, Admin 1 - States, Provinces, version in data_raw/maps/ne10m/*.VERSION.txt; public domain (naturalearthdata.com/about/terms-of-use).",
             paste0("Name crosswalk: docs/map_name_crosswalk.csv (", sum(cw$match == "exact (name_en)"), " of 31 exact matches on name_en; ", sum(cw$match == "recoded"), " recoded).")), file.path(FIG, "README_figures.txt"))
sink()
