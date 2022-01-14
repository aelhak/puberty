
library(tidyverse)
library(sitar)

##### AGE AT PUBERTY ESTIMATES IN FEMALES ####

# HEIGHT

htf_mods <- map(3:6, possibly(~ { eval(parse(text = paste0(
  "sitar(x = age_, y = ht_, id = id, data = ht_dat_f, 
  df = ", .x, ")")))}, otherwise = NA_real_))

(htf_mods <- Filter(Negate(anyNA), htf_mods))
(htf_mods_bic <- unlist(map(htf_mods, BICadj)))
(htf_mods_best <- htf_mods[[which.min(htf_mods_bic)]])
plot(htf_mods_best, apv = T)

htf_apv <- ht_dat_f %>% mutate(
  PHV = xyadj(htf_mods_best, getPeak(
    plot_v(htf_mods_best))[["x"]], tomean=FALSE)[["x"]]) %>% 
  select(id, aln, qlet, PHV) %>% distinct(id, .keep_all = TRUE) %>% 
  select(-id)

# PUBIC HAIR

phf_mods <- map(3:6, possibly(~ { eval(parse(text = paste0(
  "sitar(x = age_, y = phfc, id = id, random = 'b+c', data = phf_dat, 
  df = ", .x, ")")))}, otherwise = NA_real_))

(phf_mods <- Filter(Negate(anyNA), phf_mods))
(phf_mods_bic <- unlist(map(phf_mods, BIC)))
(phf_mods_best <- phf_mods[[which.min(phf_mods_bic)]])
plot(phf_mods_best, apv = T)
plot(phf_mods[[1]], apv = T) #######
plot(phf_mods[[2]], apv = T)
plot(phf_mods[[3]], apv = T)

phf_apv <- phf_dat %>% mutate(
  PH_apv = xyadj(phf_mods[[1]], getPeak(plot_v(phf_mods[[1]]))[["x"]], tomean=FALSE)[["x"]]) %>% 
  select(id, aln, qlet, PH_apv) %>% distinct(id, .keep_all = TRUE) %>% 
  select(-id)

# BREAST

br_mods <- map(3:6, possibly(~ { eval(parse(text = paste0(
  "sitar(x = age_, y = brc, id = id, random = 'b+c', data = br_dat, 
  df = ", .x, ")")))}, otherwise = NA_real_))

(br_mods <- Filter(Negate(anyNA), br_mods))
(br_mods_bic <- unlist(map(br_mods, BIC)))
(br_mods_best <- br_mods[[which.min(br_mods_bic)]])
plot(br_mods_best, apv = T)
plot(br_mods[[1]], apv = T)
plot(br_mods[[2]], apv = T) #####
plot(br_mods[[3]], apv = T)

br_apv <- br_dat %>% mutate(
  Br_apv = xyadj(br_mods[[2]], getPeak(plot_v(br_mods[[2]]))[["x"]], tomean=FALSE)[["x"]]) %>% 
  select(id, aln, qlet, Br_apv) %>% distinct(id, .keep_all = TRUE) %>% 
  select(-id)

# AXILLARY HAIR: b only

axhf_mods <- map(3:6, possibly(~ { eval(parse(text = paste0(
  "sitar(x = age_, y = axhfc, id = id, random = 'b', data = axh_dat_f, 
  df = ", .x, ")")))}, otherwise = NA_real_))

(axhf_mods <- Filter(Negate(anyNA), axhf_mods))
(axhf_mods_bic <- unlist(map(axhf_mods, BIC)))
(axhf_mods_best <- axhf_mods[[which.min(axhf_mods_bic)]])
plot(axhf_mods_best, apv = T)

axhf_apv <- axh_dat_f %>% mutate(
  AH_apv =  xyadj(axhf_mods_best, getPeak(plot_v(axhf_mods_best))[["x"]], tomean=FALSE)[["x"]]) %>% 
  select(id, aln, qlet, AH_apv) %>% distinct(id, .keep_all = TRUE) %>% 
  select(-id)

##### AGE AT PUBERTY ESTIMATES IN MALES ####

# HEIGHT

htm_mods <- map(3:6, possibly(~ { eval(parse(text = paste0(
  "sitar(x = age_, y = ht_, id = id, data = ht_dat_m, 
  df = ", .x, ")")))}, otherwise = NA_real_))

(htm_mods <- Filter(Negate(anyNA), htm_mods))
(htm_mods_bic <- unlist(map(htm_mods, BIC)))
(htm_mods_best <- htm_mods[[which.min(htm_mods_bic)]])
plot(htm_mods_best, apv = T)
plot(htm_mods[[1]], apv = T)
plot(htm_mods[[2]], apv = T)
plot(htm_mods[[3]], apv = T) ####

htm_apv <- ht_dat_m %>% mutate(
  PHV = xyadj(htm_mods[[3]], getPeak(plot_v(htm_mods[[3]]))[["x"]], tomean=FALSE)[["x"]]) %>% 
  select(id, aln, qlet, PHV) %>% distinct(id, .keep_all = TRUE) %>% 
  select(-id)

# PUBIC HAIR

phm_mods <- map(3:6, possibly(~ { eval(parse(text = paste0(
  "sitar(x = age_, y = phmc, id = id, random = 'b+c', data = phm_dat, 
  df = ", .x, ")")))}, otherwise = NA_real_))

(phm_mods <- Filter(Negate(anyNA), phm_mods))
(phm_mods_bic <- unlist(map(phm_mods, BIC)))
(phm_mods_best <- phm_mods[[which.min(phm_mods_bic)]])
plot(phm_mods_best, apv = T)

phm_apv <- phm_dat %>% mutate(
  PH_apv = xyadj(phm_mods_best, getPeak(plot_v(phm_mods_best))[["x"]], tomean=FALSE)[["x"]]) %>% 
  select(id, aln, qlet, PH_apv) %>% distinct(id, .keep_all = TRUE) %>% 
  select(-id)

# GENITALIA: b only

gn_mods <- map(3:6, possibly(~ { eval(parse(text = paste0(
  "sitar(x = age_, y = gnc, id = id, random = 'b', data = gn_dat, 
  df = ", .x, ")")))}, otherwise = NA_real_))

(gn_mods <- Filter(Negate(anyNA), gn_mods))
(gn_mods_bic <- unlist(map(gn_mods, BIC)))
(gn_mods_best <- gn_mods[[which.min(gn_mods_bic)]])
plot(gn_mods_best, apv = T)

gn_apv <- gn_dat %>% mutate(
  Gn_apv = xyadj(gn_mods_best, getPeak(plot_v(gn_mods_best))[["x"]], tomean=FALSE)[["x"]]) %>% 
  select(id, aln, qlet, Gn_apv) %>% distinct(id, .keep_all = TRUE) %>% 
  select(-id)

# VOICE BREAK: b only

vb_mods <- map(3:6, possibly(~ { eval(parse(text = paste0(
  "sitar(x = age_, y = vbc, id = id, random = 'b', data = vb_dat, 
  df = ", .x, ")")))}, otherwise = NA_real_))

(vb_mods <- Filter(Negate(anyNA), vb_mods))
(vb_mods_bic <- unlist(map(vb_mods, BIC)))
(vb_mods_best <- vb_mods[[which.min(vb_mods_bic)]])
plot(vb_mods_best, apv = T)

vb_apv <- vb_dat %>% mutate(
  VB_apv =  xyadj(vb_mods_best, getPeak(plot_v(vb_mods_best))[["x"]], tomean=FALSE)[["x"]]) %>% 
  select(id, aln, qlet, VB_apv) %>% distinct(id, .keep_all = TRUE) %>% 
  select(-id)

# AXILLARY HAIR

axhm_mods <- map(3:6, possibly(~ { eval(parse(text = paste0(
  "sitar(x = age_, y = axhmc, id = id, random = 'b', data = axh_dat_m, 
  df = ", .x, ")")))}, otherwise = NA_real_))

(axhm_mods <- Filter(Negate(anyNA), axhm_mods))
(axhm_mods_bic <- unlist(map(axhm_mods, BIC)))
(axhm_mods_best <- axhm_mods[[which.min(axhm_mods_bic)]])
plot(axhm_mods_best, apv = T)

axhm_apv <- axh_dat_m %>% mutate(
  AH_apv =  xyadj(axhm_mods_best, getPeak(plot_v(axhm_mods_best))[["x"]], tomean=FALSE)[["x"]]) %>% 
  select(id, aln, qlet, AH_apv) %>% distinct(id, .keep_all = TRUE) %>% 
  select(-id)

#### PLOT GROWTH & VELOCITY/DENSITY CURVES ####

pdf("res/eFig_pub_models.pdf",
    width     = 6.5,
    height    = 3.25,
    pointsize = 4
)
par(
  mfrow    = c(2,3),
  mar      = c(5, 5, 2, 2),
  xaxs     = "r",
  yaxs     = "r",
  cex.axis = 1.5,
  cex.lab  = 1.5,
  font.axis= 1.5,
  cex.main = 1.5,
  lwd      = 1
)

# a. HEIGHT

plot(htf_mods_best, opt = c('d', 'v'), las = 1, apv = F, 
     legend = NULL, xlim = c(8, 18), ylim = c(100, 200),
     vlab = "", ylab = "", xlab = "", vlim = c(0, 12),
     col='red', y2par = list(lty = 3, lwd = 2, col = 'red'), lwd = 2,
     main = "a. Height", font.main = 1, adj  = 0, cex.main = 2)

lines(htm_mods[[3]], opt=c('d', 'v'), 
      col = 'black', y2par = list(
        lty = 3, lwd = 2, col = 'black'), 
      lwd = 2, apv = F)

mtext("cm", side = 2, line = 3, cex = 1.5)
legend("topleft", c( "Females", "Males"), col = c(
  "red", "black"), lty = c(1, 1), cex = 1.5)

abline(v = getPeak(plot_v(htf_mods_best))[["x"]], lwd = 1, lty = 3, col = "red")
abline(v = getPeak(plot_v(htm_mods[[3]]))[["x"]], lwd = 1, lty = 3, col = "black")

# b. AXILLARY HAIR

plot(axhf_mods_best, opt = c('d', 'v'), las = 1, apv = F, 
     legend = NULL, xlim = c(8, 18), , ylim = c(-0.1, 1.2), yaxt = "n",
     vlab = "", ylab = "", xlab = "", vlim = c(-0.2, 1),
     col='red', y2par = list(lty = 3, lwd = 2, col='red'), lwd = 2,
     main = "b. Axillary Hair", font.main = 1, adj  = 0, cex.main = 2)

lines(axhm_mods_best, opt = c('d', 'v'), 
      col = 'black', y2par = list(
        lty = 3, lwd = 2, col = 'black'), 
      lwd = 2, apv = F)

axis(2, at = 0:1, labels = c("No", "Yes"))
legend("topleft", c( "Females", "Males"), col = c(
  "red", "black"), lty = c(1, 1), cex = 1.5)

abline(v = getPeak(plot_v(axhf_mods_best))[["x"]], lwd = 1, lty = 3, col = "red")
abline(v = getPeak(plot_v(axhm_mods_best))[["x"]], lwd = 1, lty = 3, col = "black")

# c. PUBIC HAIR

plot(phf_mods[[1]], opt = c('d', 'v'), las = 1, apv = F, 
     legend = NULL, xlim = c(8, 18), , ylim = c(1,5), yaxt = "n",
     vlab = "", ylab = "", xlab = "", vlim = c(0,1.5),
     col='red', y2par = list(lty = 3, lwd = 2, col='red'), lwd = 2,
     main = "c. Pubic Hair Stage", font.main = 1, adj  = 0, cex.main = 2)

lines(phm_mods_best, opt = c('d', 'v'), 
      col='black', y2par = list(
        lty = 3, lwd = 2, col = 'black'), 
      lwd = 2, apv = F)

axis(2, at = 1:5, labels = c(
  "PH1", "PH2", "PH3", "PH4", "PH5"))

legend("topleft", c( "Females", "Males"), col = c(
  "red", "black"), lty = c(1, 1), cex = 1.5)

abline(v = getPeak(plot_v(phf_mods[[1]]))[["x"]], lwd = 1, lty = 3, col = "red")
abline(v = getPeak(plot_v(phm_mods_best))[["x"]], lwd = 1, lty = 3, col = "black")

# d. BREAST STAGE

plot(br_mods[[2]], opt = c('d', 'v'), las = 1, apv = F, 
     legend = NULL, ylim = c(1,5), yaxt = "n", 
     vlab = "", ylab = "", xlab = "", xlim = c(8, 18),
     y2par = list(lty = 3, lwd = 2), vlim = c(0, 1), lwd = 2,
     main = "d. Breast Stage", font.main = 1, 
     adj  = 0, cex.main = 2)

mtext("age - y", side = 1, line = 2.6, cex = 1.5)
axis(2, at = 1:5, labels = c(
  "B1", "B2", "B3", "B4", "B5"))

abline(v = getPeak(plot_v(br_mods[[2]]))[["x"]], lwd = 1, lty = 3, col = "black")

# e. GENITALIA STAGE

plot(gn_mods_best, opt = c('d', 'v'), las = 1, apv = F, 
     legend = NULL, ylim = c(1,5), yaxt = "n", 
     vlab = "", ylab = "", xlab = "", xlim = c(8, 18),
     y2par = list(lty = 3, lwd = 2), vlim = c(0, 1), lwd = 2,
     main = "e. Genitalia Stage", font.main = 1, 
     adj  = 0, cex.main = 2)

mtext("age - y", side = 1, line = 2.6, cex = 1.5)
axis(2, at = 1:5, labels = c(
  "G1", "G2", "G3", "G4", "G5"))

abline(v = getPeak(plot_v(gn_mods_best))[["x"]], lwd = 1, lty = 3, col = "black")

# f. VOICE BREAK STATUS

plot(vb_mods_best, opt = c('d', 'v'), las = 1, apv = F, 
     legend = NULL, ylim = c(1,3.1), yaxt = "n", 
     vlab = "", ylab = "", xlab = "", xlim = c(8, 18),
     y2par = list(lty = 3, lwd = 2), vlim = c(0, 1), lwd = 2,
     main = "f. Voice Break", font.main = 1, 
     adj  = 0, cex.main = 2)

mtext("age - y", side = 1, line = 2.6, cex = 1.5)
axis(2, at = 1:3, labels = c(
  "Not yet\nbroken", 
  "Occasionally\na lot lower", 
  "Changed\ntotally"))

abline(v = getPeak(plot_v(vb_mods_best))[["x"]], lwd = 1, lty = 3, col = "black")

dev.off()