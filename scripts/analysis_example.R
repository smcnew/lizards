# Test out analysis using fake data
install.packages("brms")

library(lme4) # for standard modeling
library(lmerTest) # for standard modeling
library(ggplot2) # for plots
library(brms) # for mcmc models including relatedness matrix
library(dplyr) # for data wrangling

# Read in data ------------------------------------------------------------
# This is the "master" spreadsheet, each row is a sample, includes Lat Long info.
# Reformate date and sample so that they work as intended
samples <- read.csv("data_analysis/local_dat.csv") %>%
  select(-X) %>%
  rename_with(tolower) %>%
  mutate(species = as.factor(species)) %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) # make sure date is read in correctly.


# This is the csv from Anthony listing the sexual system for each species and some other covars
sexual_system <- read.csv("data_analysis/sexual_system_info.csv") %>%
  select(-X) %>%
  rename_with(tolower)

# For now just keep species we have info for (including in the relatedness matrix)
samples <- samples %>% filter(species %in% sexual_system$species)

# Add the sexual system to the master datasheet. Now each sample should be associated
# with its species, unisexual/sexual etc.
samples <- left_join(samples, sexual_system)

# Read in the relatedness matrix. Note that each species is exactly related to itself (1.00)
related <- read.table("data_analysis/whiptails_IBS.tsv")


# Simulate some data while we're waiting on results ------------------------------

# Simplify the data a little bit.
samples <- samples %>%
  select(specimen.number, species, latitude, longitude, date, ploidy,
         reproduction, parents, group, lineage1, lineage2)


# Simulate a difference between unisexuals (40% prevalence), and sexuals (20% prev).
# Then add a small effect of "group number" i.e. family.
sexual_system <- sexual_system %>%
  mutate(prevalence = case_when(reproduction == "unisexual" ~ 40,
                                reproduction == "sexual" ~ 20)) %>% # create an effect of reproductive mode
  mutate(prevalence = prevalence * (1+ group * 0.1)*0.01)


# add intercept for each species onto sample column
samples <- left_join(samples, sexual_system)

# Simulate positive or negative from each sample based on a binomial prob where prob = the baseline prev for that spp.
# Remember that rbinom will flip a coin as many times as we want, with probability p.
# We vary P according to our pre-determined difference between sexual and asexual types.



samples$pos <- rbinom(nrow(samples), 1, samples$prevalence)

aggregate (pos ~ reproduction, samples, mean) # Across samples we get a difference in groups

# Visualize this difference
ggplot(samples %>% group_by(reproduction) %>% summarise(pos_rate = mean(pos)),
       aes(reproduction, pos_rate)) +
  geom_col()

# Run some classic GLMs --------------------------------------------------

# This works, we get a singularity error; it's having trouble with the random effects.
# I think this is mostly a problem with my lazy data simulations

mod1 <- glmer(pos ~ reproduction + (1|species) + (1|group), family = "binomial", data = samples)
summary(mod1)
# interpretation: Unisexuals have a significantly (P < 0.001) higher probability
# of being infected than sexuals.

summary(mod1)$coefficients # coefficient table
sexual_coef <- summary(mod1)$coefficients[1,1]
unisexual_coef <- summary(mod1)$coefficients[1,1] + summary(mod1)$coefficients[2,1]

# Let's back transform (i.e. use the inverse logit) to recover our Ps.
# intercept is "sexual"

boot::inv.logit(sexual_coef) # take the intercept coeff
boot::inv.logit(unisexual_coef) # take the intercept coeff

# Looks pretty similar to raw data!
aggregate (pos ~ reproduction, samples, mean)

# Try the bayesian approach ------------------------------------------------------
# First do a couple of formatting checks:

eigen(related)$values # check to make sure all > 0, important for running the phylogenetic model

# Order the relationship matrix following the species levels in our sample sheet
# If this fails, it's because species needs to be a factor.
related <- related[unique(samples$species), unique(samples$species)]

# Make sure the species in our sample sheet are formatted as a factor whose levels
# match the relatedness matrix
samples <- samples %>% mutate(species = factor(species, levels = rownames(related)))
levels(samples$species) == colnames(related) # Check that species levels matches the relatedness matrix; should be all TRUE


# Create basic model
fit <- brm(
  pos ~ reproduction + (1 | gr(species, cov = related)),
  data = samples,
  family = bernoulli(),
  data2 = list(related = related),
  chains = 4,
  cores = 4,
  iter = 4000
)
summary(fit) # Coefs estimates: -1.20, estimate of effect is 1.08, similar-ish.

# interpretation: Remember that Bayesian approaches don't give p values. They
# give estimates of effects, and errors or CI's on that effect. The rule of thumb
# is that if the 95% CI does not overlap 0, we say the effect is "significant,"
# but if the 95% CI does overlap 0, we don't consider it significant.


plot(fit) # Diagnostic plots see https://michael-franke.github.io/Bayesian-Regression/practice-sheets/05a-MCMC-diagnostics.html
pairs(fit) # shouldn't be any visible patterns here. Not perfect, but basically fine.

# If we want to add species as another random effect, create a separate column with same identity as species
# We can also add some more covars if we want (latitude? ploidy? etc.)

samples$species2 <- samples$species

fit2 <- brm(
  pos ~ reproduction + ploidy + date +
    (1 | gr(species, cov = related)) + (1|species2),
  data = samples,
  family = bernoulli(),
  data2 = list(related = related),
  chains = 4,
  cores = 4,
  iter = 4000
)
summary(fit2)

boot::inv.logit(-1.92)
boot::inv.logit(-0.19)
summary(fit)

# unisexuals????
boot::inv.logit(0.57)
boot::inv.logit(1.55)

boot::inv.logit(-1.92 + 0.57)
boot::inv.logit(-0.19 + 1.55)


# Suggests that reproduction has (basically) a "significant effect" i.e estimate
# of reproduction mode is positive and CI does not overlap 0.
#
# Both ploidy and date do not have significant effects (in this pretend data)


# example for in-text scenario:
#
# "Unisexual species had a significantly higher probability of infection (Table 1;
#  model estimate prevalence of sexuals = 0.27; unisexuals = 0.48)

# Species richness  -------------------------------------------------------
# Let's also simulate some data so we could analyze species richness if we wanted
# Lazy so will not make an effect.
#
# One problem jumps out: We have pretty significant variation in sample size
# among lizard species. The more samples we get, the more parasites we'll detect (naturally).
#
#
# Potential covars to include? probably midpoint of latitude or something like that.
#
head(samples)


haplotypes <- c("a","b","c","d","e","f","g","h","i")
samples <- samples %>%
  mutate(haplotype = sample(haplotypes, size = nrow(samples),  replace = T)) %>%
  mutate(haplotype = ifelse(pos == 0, NA, haplotype))


# inspect haplotype x species combos

table(samples$species, samples$haplotype)


# Calculate species richness (lots of different ways to code this, consider dplyr solutions)

aggregate(haplotype ~ species, data = samples, unique) # this shows us all haplotypes x species

richness <- aggregate(haplotype ~ species, data = samples, FUN = function (x) length(unique(x)))

# add this info back onto sexual system table
sexual_system <- left_join(sexual_system, richness)


# Quick and dirty: neg binomial glm with extractions as a covar
MASS::glm.nb(haplotype ~ reproduction + extractions, data = sexual_system) %>% summary

# quasipois should do something similar
glm(haplotype ~ reproduction + extractions, family = "quasipoisson", data = sexual_system) %>% summary # v. similar.




# example thing -----------------------------------------------------------

phylo <- ape::read.nexus("https://paul-buerkner.github.io/data/phylo.nex")
data_simple <- read.table(
  "https://paul-buerkner.github.io/data/data_simple.txt",
  header = TRUE
)

plot(phylo)
head(data_simple)

A <- ape::vcv.phylo(phylo)

