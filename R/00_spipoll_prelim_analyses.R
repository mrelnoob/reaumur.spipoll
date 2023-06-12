######## *------------------------------------------------------------* ########
##################### PRELIMINARY ANALYSES FOR Spipoll data ####################
######## *------------------------------------------------------------* ########

# Note that I manually added this project and its files into my Github account that can be found at the
# following URL: https://github.com/mrelnoob/reaumur.spipoll

########### *-----------------------------------------------------* ############
############################ Main Git commits ##################################
# ---------------------------------------------------------------------------- #
usethis::use_git(message = ":boom: First generated results!")
usethis::use_git(message = ":metal: Created a new function")
usethis::use_git(message = ":zap: Ignoring something")
usethis::use_git(message = ":pencil: Documented a function or wrote something")
usethis::use_git(message = ":hammer: Ongoing programming!")
usethis::use_git(message = ":white_check_mark: Updated the {target} pipeline")
usethis::use_git(message = ":x: Problem detected!")
#system("git push") # Or using a CLI!
# Don't forget to push your commits once you're sure you made no mistakes.
# ---------------------------------------------------------------------------- #
# ------------------------------- THE END ------------------------------------ #
########### *-----------------------------------------------------* ############





# ---------------------- #
##### 0. Data import #####
# ---------------------- #

##### * 0.1. Import and initial formatting -------------------------------------
# ---------------------------------------------------------------------------- #
### ** 0.1.1. Import ----
# _________________________

library(magrittr)

rgenera <- read.csv2(here::here("data/spipoll_obs13_genre2ordre_work.csv"), na.strings = "")
rsites <- read.csv2(here::here("data", "national_data500.csv"))
rcoord <- read.csv2(here::here("data/spipoll_obs13_genre2ordre_geom.csv"))

rgenera %>% dplyr::mutate(
  collection = as.factor(collection),
  espece_fleur = as.factor(espece_fleur),
  date_de_session = as.factor(date_de_session),
  espece_insecte = as.factor(espece_insecte),
  genre_insecte = as.factor(genre_insecte),
  famille_insecte = as.factor(famille_insecte),
  ordre_insecte = as.factor(ordre_insecte)
  ) %>%
  dplyr::rename(
    flower_sp = espece_fleur,
    date = date_de_session,
    observed_arthropoda = espece_insecte,
    obs_genus = genre_insecte,
    obs_family = famille_insecte,
    obs_order = ordre_insecte
  ) -> rgenera

rsites %>% dplyr::mutate(
  collection = as.factor(collection),
  espece_fleur = as.factor(espece_fleur),
  ville = as.factor(ville),
  code_ua = as.factor(UA_code_2018_simp)) %>%
  dplyr::relocate(code_ua, .after = UA_code_2018_simp) -> rsites

rcoord %>% dplyr::mutate(collection = as.factor(collection),
                         xcoord = as.numeric(xcoord),
                         ycoord = as.numeric(ycoord)) -> rcoord

rgenera$coord_x <- rcoord$xcoord
rgenera$coord_y <- rcoord$ycoord
rgenera %>% dplyr::relocate(coord_x, .after = date) %>%
  dplyr::relocate(coord_y, .after = coord_x) -> rgenera
summary(rgenera)



### ** 0.1.2. Data preparation and cleaning ----
# ______________________________________________

### *** 0.1.2.1. Computing richness for corrected genera data ----
## For all arthropods:
rgenera %>% dplyr::group_by(collection) %>%
  dplyr::summarise(flower_sp = dplyr::first(flower_sp),
                   date = dplyr::first(date),
                   coord_x = mean(coord_x),
                   coord_y = mean(coord_y),
                   genus_richness = dplyr::n_distinct(obs_genus, na.rm = TRUE),
                   family_richness = dplyr::n_distinct(obs_family),
                   order_richness = dplyr::n_distinct(obs_order)) -> richness_arthro

## For the main pollinator orders only:
rgenera %>% dplyr::filter(obs_order %in% c("Diptera", "Hymenoptera", "Coleoptera", "Lepidoptera")) %>%
  dplyr::group_by(collection) %>%
  dplyr::summarise(flower_sp = dplyr::first(flower_sp),
                   date = dplyr::first(date),
                   coord_x = mean(coord_x),
                   coord_y = mean(coord_y),
                   genus_richness = dplyr::n_distinct(obs_genus, na.rm = TRUE),
                   family_richness = dplyr::n_distinct(obs_family),
                   order_richness = dplyr::n_distinct(obs_order)) -> richness_polli


### *** 0.1.2.2. Data reduction ----
summary(rsites)
rsites %>% tidyr::drop_na() %>%
  dplyr::filter(bn > 5) %>% # I only keep sites with at least 5 buildings
  dplyr::select(-nb_insecte, -nb_espece_insecte, -nb_famille_insecte, -nb_ordre_insecte, -rayon,
                -log_nb_insecte, -log_nb_espece_insecte, -log_nb_famille_insecte, -log_nb_ordre_insecte,
                -UA_code_2018_simp) %>%
  dplyr::select(!dplyr::starts_with("log")) %>% # Deletes columns that start with...
  tibble::as_tibble() %>%
  janitor::clean_names() -> rsites # The "clean_names" function sets every name to lower cases!


# Deleting the original OSO variables:
rsites %>% dplyr::select(!dplyr::starts_with("oso") | dplyr::starts_with("oso_f")) -> site_metrics
colnames(site_metrics)


### *** 0.1.2.3. Joining datasets ----
srich_arthro <- dplyr::left_join(x = site_metrics, y = richness_arthro, by = "collection")
srich_arthro %>% tidyr::drop_na() %>%
  dplyr::select(-espece_fleur) %>%
  dplyr::relocate(date, .after = collection) %>%
  dplyr::relocate(flower_sp, .after = date) %>%
  dplyr::relocate(coord_x, .after = ville) %>%
  dplyr::relocate(coord_y, .after = coord_x) %>%
  dplyr::relocate(genus_richness, .after = flower_sp) %>%
  dplyr::relocate(family_richness, .after = genus_richness) %>%
  dplyr::relocate(order_richness, .after = family_richness) -> srich_arthro

srich_polli <- dplyr::left_join(x = site_metrics, y = richness_polli, by = "collection")
srich_polli %>% tidyr::drop_na() %>%
  dplyr::select(-espece_fleur) %>%
  dplyr::relocate(date, .after = collection) %>%
  dplyr::relocate(flower_sp, .after = date) %>%
  dplyr::relocate(coord_x, .after = ville) %>%
  dplyr::relocate(coord_y, .after = coord_x) %>%
  dplyr::relocate(genus_richness, .after = flower_sp) %>%
  dplyr::relocate(family_richness, .after = genus_richness) %>%
  dplyr::relocate(order_richness, .after = family_richness) -> srich_polli
summary(srich_arthro)
summary(srich_polli)





########################## ************************************************* ###############################
# ------------------------------------------ #
##### 1. Exploratory data analyses (EDA) #####
# ------------------------------------------ #

##### * 1.1. Outliers detection ------------------------------------------------
# ---------------------------------------------------------------------------- #
.pardefault <- par()

ppl.tits::uni.boxplots(srich_arthro)
ppl.tits::uni.dotplots(srich_arthro)
ppl.tits::uni.boxplots(srich_polli)
ppl.tits::uni.dotplots(srich_polli)
# It appears that:
#  - There are some extreme values for the B_metrics.
#  - There are values above 1 for the distance to city-centre, but it's normal.



##### * 1.2. Distribution, skewness and kurtosis -------------------------------
# ---------------------------------------------------------------------------- #
ppl.tits::uni.histograms(srich_arthro)
ppl.tits::uni.histograms(srich_polli)

arthro.num <- srich_arthro[, sapply(srich_arthro, is.numeric)]
tab <- data.frame(moments::skewness(x = arthro.num), moments::kurtosis(x = arthro.num)-3)
arthro_skewkurtable <- knitr::kable(x = tab, digits = 3, col.names = c("Skewness", "Excess kurtosis"))
polli.num <- srich_polli[, sapply(srich_polli, is.numeric)]
tab <- data.frame(moments::skewness(x = polli.num), moments::kurtosis(x = polli.num)-3)
polli_skewkurtable <- knitr::kable(x = tab, digits = 3, col.names = c("Skewness", "Excess kurtosis"))
# It appears that:
#  - There is excess skewness for BH, BAM, BASTD, and especially BHW.
#  - There is highly excessive kurtosis for BN, BD, and especially BH, BAM, BASTD, and BHW!
rm(tab)



##### * 1.3. Bivariate relationships -------------------------------------------
# ---------------------------------------------------------------------------- #
# To compute the correlation matrix:
res.cor.spipoll <- round(stats::cor(arthro.num, use = "complete.obs", method = "spearman"), 2)
# To compute a matrix of correlation p-values:
res.pcor.spipoll <- ggcorrplot::cor_pmat(x = arthro.num, method = "spearman")

spipoll.corplot <- ggcorrplot::ggcorrplot(res.cor.spipoll, type = "upper",
                                         outline.col = "white",
                                         ggtheme = ggplot2::theme_gray,
                                         colors = c("#6D9EC1", "white", "#E46726"), p.mat = res.pcor.spipoll,
                                         insig = "blank")
# We can see that:
# - Taxonomic COUNTS are quite strongly positively correlated among themselves but only weakly with the other
#   other variables.
# - The DISTANCE variable is negatively correlated with BD, BH, BAM, BASTD, BHW, OSO_F1 and UA_F1!
# - BN, BD and BH are strongly positively correlated with each other and with BHW, OSO_F1 and UA_F1! They are
#   also negatively correlated with BW.
# - BW is also negatively correlated with BF, BHW and OSO_F1!
# - UA_F1 is also positively correlated with BHW and OSO_F1!
## I suggest removing BD and BASTD and perhaps BN and BHW.

spipoll.pairplot <- GGally::ggpairs(arthro.num)
spipoll.pairplot <- GGally::ggpairs(polli.num)
# We find globally the same patterns:
# - BN and BF, BF and BW, BF and BC, or UA_F1 and OSO_F1 indeed seem highly collinear.
# - It is also true for BW but to a lesser extent and in a curvilinear way.
# - The BAM metric shows diverging patterns (e.g. with BN and BH), BHW also but to a lesser extent!
# - BF and BHW extreme values may be multivariate outliers! For BHW, they come from Paris so they're likely
#   not true outliers but are characteristic from a specific urban form.
# - Many relationships are likely highly collinear on the log scale: e.g. BN, BH and BHW with UA_F1 or OSO_F1!
## I suggest removing BF, BW and the F1 factors.



##### * 1.4. Final formatting --------------------------------------------------
# ---------------------------------------------------------------------------- #

## As suggested, I remove BD, BASTD, BF, BW and the F1 factors (or at least, I won't use them):
srich_arthro %>% dplyr::mutate(log_bn = log10(bn),
                        sqrt_bf = sqrt(bf),
                        sqrt_bh = sqrt(bh),
                        log_bam = log10(bam),
                        log_bhw = log10(bhw)) %>%
  dplyr::relocate(log_bn, .after = bn) %>%
  dplyr::relocate(sqrt_bf, .after = log_bn) %>%
  dplyr::relocate(sqrt_bh, .after = bh) %>%
  dplyr::relocate(log_bam, .after = bam) %>%
  dplyr::relocate(log_bhw, .after = bhw) %>%
  dplyr::rename(dist_centre = part_dist_centre,
                city = ville) %>%
  dplyr::mutate(coord_y = jitter(x = coord_y, factor = 1.2)) %>%
  dplyr::mutate(coord_x = jitter(x = coord_x, factor = 1.2)) -> srich_arthro2

srich_polli %>% dplyr::mutate(log_bn = log10(bn),
                     sqrt_bf = sqrt(bf),
                     sqrt_bh = sqrt(bh),
                     log_bam = log10(bam),
                     log_bhw = log10(bhw)) %>%
  dplyr::relocate(log_bn, .after = bn) %>%
  dplyr::relocate(sqrt_bf, .after = log_bn) %>%
  dplyr::relocate(sqrt_bh, .after = bh) %>%
  dplyr::relocate(log_bam, .after = bam) %>%
  dplyr::relocate(log_bhw, .after = bhw) %>%
  dplyr::rename(dist_centre = part_dist_centre,
                city = ville) %>%
  dplyr::mutate(coord_y = jitter(x = coord_y, factor = 1.2)) %>%
  dplyr::mutate(coord_x = jitter(x = coord_x, factor = 1.2)) -> srich_polli2

summary(srich_arthro2)
summary(srich_polli2)
colnames(srich_arthro2)
colnames(srich_polli2)





########################## ************************************************* ###############################
# --------------------------------------------------- #
##### 2. Modelling the arthropods family richness #####
# --------------------------------------------------- #

##### * 2.1. Genus richness models ---------------------------------------------
# ---------------------------------------------------------------------------- #
### ** 2.1.1. Poisson or negative binomial GL(M)M ----
# ____________________________________________________

srich_arthro2 %>% dplyr::filter(genus_richness > 0) %>%
  dplyr::mutate(longitude = scale(coord_x),
                latitude = scale(coord_y),
                c.log_bn = scale(log_bn, scale = FALSE),
                c.sqrt_bf = scale(sqrt_bf, scale = FALSE),
                c.bd = scale(bd, scale = FALSE),
                c.bh = scale(bh, scale = FALSE),
                c.sqrt_bh = scale(sqrt_bh, scale = FALSE),
                c.bc = scale(bc, scale = FALSE),
                c.bf = scale(bf, scale = FALSE),
                c.bam = scale(bam, scale = FALSE),
                c.log_bam = scale(log_bam, scale = FALSE),
                c.bw = scale(bw, scale = FALSE),
                c.bhw = scale(bhw, scale = FALSE),
                c.log_bhw = scale(log_bhw, scale = FALSE))-> srich_arthro3 # Rescaling to avoid convergence issues.
summary(srich_arthro3)
ppl.tits::uni.dotplots(srich_arthro3)


## Fitting a regular Poisson regression:
sppGNa_glm1 <- stats::glm(genus_richness ~ dist_centre + log_bn + sqrt_bh + bc + log_bam + log_bhw +
                               oso_f2 + oso_f3 + oso_f4 + flower_sp + longitude + latitude,
                             data = srich_arthro3, family = "poisson")

## Fitting a regular Poisson GLMM:
sppGNa_glmm1 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + log_bn + sqrt_bh + bc + log_bam + log_bhw +
                                      oso_f2 + oso_f3 + oso_f4 + longitude + latitude + (1|city) + (1|flower_sp),
                               data = srich_arthro3, family = "poisson")
summary(sppGNa_glm1) # AIC = 11791
summary(sppGNa_glmm1) # AIC = 11555.2 (or 11510.5 if "flower_sp" is used as a fixed effect).
# The use of CITY as a random effect (RE) seems warranted by the data!
## UPDATE: diagnostics turned out very problematic (overdispersion, multicollinearity, spatial
# autocorrelation etc.). I will thus try with a negative binomial distribution and altered model specifications.

## Fitting negative binomial GLMMs:
sppGNa_glmm2 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + log_bn + bc + log_bam + bw +
                                   ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
                                 data = srich_arthro3, family = glmmTMB::nbinom2(link = "log"))
summary(sppGNa_glmm2) # AIC = 10887.1
sppGNa_glmm3 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + c.log_bn * c.bc * c.log_bam + bw +
                                   ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
                                 data = srich_arthro3, family = glmmTMB::nbinom2(link = "log"))
summary(sppGNa_glmm3) # AIC = 10886.1
# So the use of a negative binomial (NB) model seems to help the model and so does the interaction.





### ** 2.1.2. Diagnostics and assumption checks ----
# __________________________________________________

### *** 2.1.2.1. Residuals extraction, autocorrelation and collinearity ----
par(.pardefault)

## Simulation-based scaled residuals computation ({DHARMa} method):
simu.resid <- DHARMa::simulateResiduals(fittedModel = sppGNa_glmm2, n = 1000, re.form = NULL) # The
# 're.form' argument is to base simulations on the model unconditional of the random effects (and only works
# for {lme4} formulations). It is useful for testing dispersion (see below) but can be omitted eventually.
plot(simu.resid) # Significant deviation and outliers detected!
# DHARMa::testOutliers(simu.resid, type = 'bootstrap', nBoot = 500) # Long.
DHARMa::outliers(simu.resid) # Six outliers detected!
srich_arthro3[c(510),] # Not sure why...

## Autocorrelation and collinearity:
DHARMa::testSpatialAutocorrelation(simulationOutput = simu.resid,
                                   x = srich_arthro3$coord_x, y = srich_arthro3$coord_y, plot = TRUE) # Nope!!!
performance::check_autocorrelation(sppGNa_glmm2) # Ok.
performance::check_collinearity(sppGNa_glmm2) # Ok-ish.
stats::vcov(sppGNa_glmm2) # Ok.

## Heteroscedasticity and possible model misspecifications:
par(.pardefault)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$flower_sp)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$city)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$dist_centre)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$log_bn)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$bf)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$sqrt_bh)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$bc)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$log_bam)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$bw)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$log_bhw)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$oso_f1)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$oso_f2)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$oso_f3)
DHARMa::plotResiduals(simu.resid, form = srich_arthro3$oso_f4)
# Nothing's fine!!!



### *** 2.1.2.2. Distribution (family, ZI, dispersion) ----
## Assessing over or under-dispersion:
AER::dispersiontest(object = sppGNa_glm1, alternative = c("greater")) # Significant overdispersion!
DHARMa::testDispersion(simu.resid, alternative = "greater") # Almost significant overdispersion.

# ## Theoretical count distribution:
# theo_count <- COMPoissonReg::rcmp(n = nrow(srich_arthro3), lambda = mean(srich_arthro3$clutch_size), nu = 1.05) # The 'nu'
# # parameter should be chosen by trial-and-errors.
# tc_df <- data.frame(theo_count)
#
# ggplot2::ggplot(srich_arthro3, ggplot2::aes(clutch_size)) +
#   ggplot2::geom_bar(fill = "#1E90FF") +
#   ggplot2::geom_bar(data = tc_df, ggplot2::aes(theo_count, fill="#1E90FF", alpha=0.5)) +
#   ggplot2::theme_classic() +
#   ggplot2::theme(legend.position = "none") # Blue = observed counts; red = simulated.
# # This plot suggests that clutch_size could be following a COM-Poisson distribution of parameter nu~1.1!
#
# ## Distribution of the predicted counts:
# pred_counts <- stats::predict(object = sppGNa_glmm2, type = "response") # Extract the predicted counts.
# par(mfrow= c(1,2))
# hist(pred_counts, main = "Predicted counts", xlab = "Number of laid eggs")
# hist(srich_arthro3$clutch_size, main = "Observed counts", xlab = "Number of laid eggs") # The models' predictions
# # are very similar and relatively acceptable (although too narrow).
#
#
#
# ### *** 2.1.2.3. Linearity ----
# # For the sake of further exploration, I also plot variants of our predictors:
# srich_arthro3 %>% dplyr::select(log_patch_area, log_patch_perim, log_woody_vw, log_woody_area,
#                          log_F_metric_d1b0, log_F_metric_d2b0, log_F_metric_d3b0, log_F_metric_d1b1,
#                          log_F_metric_d2b1,
#                          Rr_metric_d1c1, Rr_metric_d2c1, Rr_metric_d3c1, Dr_metric_c1, Dr_metric_c2,
#                          urban_intensity, log_herb_area, built_area, sqrt_built_vol, traffic,
#                          light_pollution, noise_m,
#                          cumdd_30, laying_day, min_t_before) -> mydata
# predictors <- colnames(mydata)
# # Bind log(Y) and tidying the data for plot (ggplot2, so long format):
# mydata <- mydata %>%
#   dplyr::mutate(log_y = log(srich_arthro3$clutch_size)) %>%
#   tidyr::gather(key = "predictors", value = "predictor.value", -log_y)
# # Create scatterplot
# ggplot2::ggplot(mydata, ggplot2::aes(y = log_y, x = predictor.value))+
#   ggplot2::geom_point(size = 0.5, alpha = 0.5) +
#   ggplot2::geom_smooth(method = "loess") +
#   ggplot2::theme_bw() +
#   ggplot2::facet_wrap(~predictors, scales = "free_x") # Linearity seems respected.
#
#
#
# ### *** 2.1.2.4. Model goodness-of-fit (GOF) and performances ----
# # GOF test of Pearson's Chi2 residuals:
# dat.resid <- sum(stats::resid(sppGNa_glmm2, type = "pearson")^2)
# 1 - stats::pchisq(dat.resid, stats::df.residual(sppGNa_glmm2)) # p = 0.83, indicating that there is no
# # significant lack of fit. Keep in mind though that GOF measures for mixed models is an extremely complicated
# # topic and interpretations are not straightforward.
#
# # Computing a pseudo-R2:
# performance::r2_nakagawa(sppGNa_glmm2) # [Additive model]: Marg_R2_glmm = 0.1; Cond_R2_glmm = 0.11.
# # Does not work if we tune the dispersion parameter of the COM-Poisson model.
#
# ## Likelihood-based evaluation of effects inclusion:
# # For the "site" random-effects (RE):
# sppGNa_glmm2ac <- glmmTMB::glmmTMB(clutch_size ~ log_patch_area + log_F_metric_d2b1 + species +
#                                       urban_intensity + manag_intensity +
#                                       light_pollution + noise_m + traffic +
#                                       cumdd_30 + laying_day + year + (1|id_nestbox) + (1|site),
#                                     data = srich_arthro3, family = glmmTMB::compois(link = "log"),
#                                     dispformula = ~1) # Rather long to fit (~3-4 min)!
# summary(sppGNa_glmm2ac) # AIC = 1390.6.
# # The non-mixed model gives AIC = 1391.7, so similar to the mixed-model (AIC = 1391.3) with only
# # "id_nestbox" as RE. The one with both RE gives AIC = 1390.6., so it seems like the use of a mixed model
# # is not truly supported by the data.
#
# ## For the whole model:
# ttCy_comglmm0 <- glmmTMB::glmmTMB(clutch_size ~ 1 + (1|id_nestbox),
#                                   data = srich_arthro3, family = glmmTMB::compois(link = "log"),
#                                   dispformula = ~1)
# res.LRT_null <- stats::anova(object = ttCy_comglmm0, sppGNa_glmm2, test = "LRT")
# # The test is significant, confirming that the model is useful to explain the data.
#
#
#
# ### *** 2.1.2.5. Posterior predictive simulations ----
# # Predicted counts:
# par(.pardefault)
# obsprop <- prop.table(table(srich_arthro3$clutch_size))
# sims <- stats::simulate(sppGNa_glmm2, nsim = 1000)
# nsim4 <- colSums(sims == 4) # Number of fours (min obs value)
# par(las=4,bty="l")
# plot(pt <- prop.table(table(nsim4)),
#      ylab="Probability", xlab="Number of fours (true == 1)")
# (obs4 <- sum(srich_arthro3$clutch_size == 4))
# points(obs4, 0.002, col="red", pch=16, cex=2) # See y values in obsprop!
#
# nsim9 <- colSums(sims == 9) # Number of nines (modal obs value).
# par(las=1,bty="l")
# plot(pt <- prop.table(table(nsim9)),
#      ylab="Probability", xlab="Number of nines (true == 86)")
# (obs9 <- sum(srich_arthro3$clutch_size == 9))
# points(obs9, 0.22, col="red", pch=16, cex=2)
#
# nsim14 <- colSums(sims == 14) # Number of fourteens (max obs value).
# par(las=1,bty="l")
# plot(pt <- prop.table(table(nsim14)),
#      ylab="Probability", xlab="Number of fourteens (true == 5)")
# (obs14 <- sum(srich_arthro3$clutch_size == 14))
# points(obs14, 0.013, col="red", pch=16, cex=2)
# # These three examples confirm that the model tends to overpredict a bit.





### ** 2.1.3. Conclusions ----
# ____________________________

# There are strong GENUS richness variations across regions.
# Methodologically challenging to account for many urban metrics at the same time as they are highly correlated
# (which causes multicollinearity issues).
# This model works relatively fine:
sppGNa_glmm2 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + log_bn + bc + log_bam + bw +
                                   ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
                                 data = srich_arthro3, family = glmmTMB::nbinom2(link = "log"))
summary(sppGNa_glmm2) # AIC = 10887.1
# It shows:
#   - A fairly strong negative effect of building patches mean area!
#   - A strong positive effect of building gaps proportion!
#   - A weak negative effect of UA_F3!
sppGNa_glmm3 <- glmmTMB::glmmTMB(genus_richness ~ dist_centre + c.log_bn * c.bc * c.log_bam + bw +
                                   ua_f2 + ua_f3 + longitude + latitude + (1|city) + (1|flower_sp),
                                 data = srich_arthro3, family = glmmTMB::nbinom2(link = "log"))
summary(sppGNa_glmm3) # AIC = 10886.1

sjPlot::plot_model(model = sppGNa_glmm3, type = "int", terms = c("c.log_bn", "c.bc", "c.log_bam"),
                   mdrt.values = "quart", # Can also be "minmax" or otherwise.
                   title = "Marginal effects of the three-way interaction: BN * BC * BAM
                   Note that metrics are log-transformed and mean-centred",
                   legend.title = "Building contiguity (BC)",
                   axis.title = c("Building numbers (BN)", "Predicted number of arthropod genera"),
                   show.p = TRUE, show.values = TRUE)
# Interesting, but very high variability, suggesting that some variable combinations are likely rare.


