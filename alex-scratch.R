library(tidyverse)
library(survival)


dat <- read_delim("data/ADDICTS.txt")

## Describe the sample

addicts_dat %>% view()
  
  skimr::skim() %>% 
  
  as_tibble() 



addicts_dat %>% 
  
  janitor::clean_names() %>% 

  ggplot() +
  geom_bar(aes(x=dose), stat = "count")
\
# Create survival object
services_surv <- Surv(dat_surv$services$time, dat_surv$services$status)

# Get the Kaplan-Meier survival estimates
services.km <- survfit(services_surv ~ 1, data = dat_surv$services)

# Plot the Kaplan-Meier curve
survminer::ggsurvplot(
  fit = services.km,
  conf.int = TRUE,
  surv.median.line = "v",
  linetype = 1,
  legend = "none",
  palette = c("#007FFF"),
  xlab = "Days After Completing Amplify", 
  ylab = "Proportion Still with No Service Outcome",
  ggtheme = theme_bw()
) 
