# Analysis McNew, Duggan & Barley


library(lme4) # for standard modeling
library(lmerTest) # for standard modeling
library(ggplot2) # for plots
library(brms) # for mcmc models including relatedness matrix
library(tidybayes) # for plotting posterior estimates
library(tidyr)
library(dplyr) # for data wrangling


# Data --------------------------------------------------------------------


# Screening results data
liz <- read.csv("data_analysis/DugganLizardFullDat2.csv") %>%
  rename_with(tolower) %>%
  filter(!is.na(checkedhepfrorhaem)) %>% # filter samples that weren't found
  mutate(species = recode(species, "G" = "speciesG"))

# Read in the csv listing the sexual system for each species and some other covars
sexual_system <- read.csv("data_analysis/sexual_system_info.csv") %>%
  select(-X) %>%
  rename_with(tolower)

# Read in the relatedness matrix. Note that each species is exactly related to itself (1.00)
related <- read.table("data_analysis/whiptails_IBS.tsv") %>%
  rename_with(~ sub("_.*", "", .x)) %>%
  tibble::rownames_to_column("rowname") %>%
  mutate(rowname = sub("_.*", "", rowname)) %>%
  tibble::column_to_rownames("rowname")

# Read in locality data
locdata <- read.csv("data_analysis/local_dat.csv")  %>%
  rename_with(tolower) %>%
  select(specimen.number, species, latitude, longitude, date, county, state, country) %>%
  mutate(date = as.Date(date, "%m/%d/%y"))

# Add the sexual system to the master datasheet. Now each sample should be associated
# with its species, unisexual/sexual etc.
liz <- left_join(liz, sexual_system)


# For now just keep species we have relatedness info for
liz <- liz %>%
  filter(species %in% rownames(related))

table(liz$reproduction) # sample size for each reproduction type.

# Join with locality data
liz <- left_join(liz, locdata) %>% distinct(specimen.number, .keep_all = TRUE)


# Write out cleaned data for map making
write.csv(liz, "data_analysis/combined_lizard_data.csv", row.names = FALSE)

# Summary of screening methods
liz %>% select(contains("checked")) %>%
  summarise(across(everything(), ~sum(.x, na.rm = TRUE)))

# summary of samples checked by nanodrop
fivenum(liz$dna.concentration..ng.ul., na.rm =T)
nrow(liz) - sum(is.na(liz$dna.concentration..ng.ul.))

# Summary of different screening methods
table(liz$positive_37604292)
table(liz$pos_hepfr_gel)
table(liz$sequenced_hepfr)
table(liz$actualparasite)
16/418


# investigation of non-target sequences produced
liz %>% filter(sequenced_hepfr == 1) %>%
  filter(actualparasite != 1) %>%
  count(best_sequence_result)



15 + 1 + 45 + 91
# 15 pos with hepfr, 1 pos bedell, 45 NA, 90 other
# How many different parasite families detected
liz %>%
  filter(actualparasite == 1) %>%
  count(parasitefamily)

liz %>% filter(sequenced_hepfr == 1) %>%
  filter(actualparasite == 1) %>%
  group_by(parasitefamily, species) %>% tally


# summary of prevalence in sexuals/asexuals

liz %>%
  group_by(reproduction) %>%
  summarize(prevalence = sum(actualparasite)/n())

# Test of prevalence sex vs asexual ---------------------------------------


eigen(related)$values # check to make sure all > 0, important for running the phylogenetic model

# Order the relationship matrix following the species levels in our sample sheet
# If this fails, it's because species needs to be a factor.
related <- related[unique(liz$species), unique(liz$species)]

# Make sure the species in our sample sheet are formatted as a factor whose levels
# match the relatedness matrix
liz <- liz %>% mutate(species = factor(species, levels = rownames(related)))
levels(liz$species) == colnames(related) # Check that species levels matches the relatedness matrix; should be all TRUE

head(liz)
# Create basic model
fit <- brm(
  actualparasite ~ reproduction + (1 | gr(species, cov = related)),
  data = liz,
  family = bernoulli(),
  data2 = list(related = related),
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.99)
)
summary(fit) # Coefs estimates: -2.73, estimate of effect is -0.41, similar-ish.

# interpretation: Remember that Bayesian approaches don't give p values. They
# give estimates of effects, and errors or CI's on that effect. The rule of thumb
# is that if the 95% CI does not overlap 0, we say the effect is "significant,"
# but if the 95% CI does overlap 0, we don't consider it significant.


plot(fit) # Diagnostic plots see https://michael-franke.github.io/Bayesian-Regression/practice-sheets/05a-MCMC-diagnostics.html
pairs(fit) # shouldn't be any visible patterns here. Not perfect, but basically fine.

# detect a relationship with latitude?
fit2 <- brm(
  actualparasite ~ reproduction + latitude + longitude +
    (1 | gr(species, cov = related)),
  data = liz,
  family = bernoulli(),
  data2 = list(related = related),
  chains = 4,
  cores = 4,
  iter = 4000,
  control = list(adapt_delta = 0.99)
)
head(liz)
summary(fit2)


# Plot results

newdat <- data.frame(
  reproduction = c("sexual","unisexual")
)

draws <- add_epred_draws(
  newdat,
  fit,
  re_formula = NA
)

pdf("output_plots/model_results.pdf")
ggplot(draws, aes(y = .epred, x = reproduction)) +
  stat_eye(
    point_interval = median_qi,
    .width = c(0.95)) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 14)) +
  labs(
    x = "",
    y = "Estimated prevalence"
  )
dev.off()

# Turn coefficients into Odds Ratio

summary(fit)
exp(-0.41)
exp(-1.72)
exp(0.95)
summary(fit)

# unisexuals????
boot::inv.logit(0.57)
boot::inv.logit(1.55)

boot::inv.logit(-1.92 + 0.57)
boot::inv.logit(-0.19 + 1.55)

