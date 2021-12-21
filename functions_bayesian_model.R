library(sjstats)
library(sjmisc)
library(mediation)
library(brms)
options(mc.cores = parallel::detectCores())


# load sample data
data(jobs)
data(efc)
efc <- to_factor(efc, e42dep, c172code, c161sex, e15relat)
zinb <- read.csv("http://stats.idre.ucla.edu/stat/data/fish.csv")


# linear models, for mediation analysis
b1 <- lm(job_seek ~ treat + econ_hard + sex + age, data = jobs)
b2 <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data = jobs)


# mediation analysis, for comparison with brms
m1 <- mediate(b1, b2, sims = 1000, treat = "treat", mediator = "job_seek")


# fit Bayesian models
f1 <- bf(job_seek ~ treat + econ_hard + sex + age)
f2 <- bf(depress2 ~ treat + job_seek + econ_hard + sex + age)

m2 <- brm(f1 + f2 + set_rescor(FALSE), data = jobs, cores = 4)

m3 <- brm(
  bf(count ~ child + camper + (1 | persons), 
     zi ~ child + camper + (1 | persons)),
  data = zinb,
  family = zero_inflated_poisson(),
  cores = 4
)

m4 <- brm(
  mpg ~ wt + hp + (1 | cyl) + (1 + wt | gear), 
  data = mtcars, 
  cores = 4
)


