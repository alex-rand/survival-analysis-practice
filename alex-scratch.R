library(tidyverse)
library(survival)
library(survminer)
library(flexsurv)
library(rstanarm)

dat <- read_delim("data/ADDICTS.txt") %>% 
  
  janitor::clean_names() 

surv_object <- Surv(dat$days_survival, dat$status)

# rstanarm
options(mc.cores = parallel::detectCores())

bm.1 <- rstanarm::stan_surv(
  surv_object ~ 1 + prison + dose,
  data = dat,
  chains = 4,
  seed = NULL,
  iter = 200,
  basehaz = "bs",
  basehaz_ops = list(degree = 3, knots = c(130, 375)),
  prior_PD = FALSE
)

bm.1 <- rstanarm::stan_surv(
  surv_object ~ 1 + prison + dose,
  data = dat,
  basehaz = "exp",
  #prior = normal(0, 1),
  prior_intercept = normal(100, 1)
#  basehaz_ops = list(degree = 3, knots = c(130, 375)),
#  prior_PD = FALSE
)

rstanarm::ps_check(bm.1)

summary(bm.1)
# posterior_survfit() to generate all the curves we need
# also just plot() though I think.
# fixef() and ranef() for ease of access to parameter estimates
# log_lik gives us the LPPD
# waic and loo both work :)
# ps_check() to plot the survival function against th KM curve :)

### With splines we need to specify:
# spline degree δ;
# knot locations k OR degrees of freedom DF. Degrees of freedom is just 'number of knots' I think. 

# There are always automatically two boundary knots that I can't change, 
# IE one at the first event time and one at the last time observed, be it
# an event or a censor. So all I can control are the _internal_ knots. 
# You can either specify the exact locations of all the internal knots,
# OR you can just say 'I want 5 internal knots' and then they'll be spaced
# evenly in terms of percentile across the curve. 



# You specify these by making them a list and passing them to the basehaz_ops.

# for example:

# basehaz_ops = list(degree = 2, knots = c(10,20)) 

# would request a baseline hazard modelled using quadratic
# M-splines with two internal knots located at t = 10 and t = 20.


summary(bm.1, digits = 2) %>% 
  
  as.data.frame() %>% 
  
  rownames_to_column("parameter") %>%
  
  select(1:4) %>% 
  
  map_if(is.numeric, exp) %>% 
  
  view()

loo::loo(bm.1)

plot(bm.1)

#options(brms.backend = "cmdstanr")
options(mc.cores = parallel::detectCores())
rstanarm::
stan_surv(surv_object ~ prison, dat)
