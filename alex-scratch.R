library(tidyverse)
library(survival)
library(survminer)
dat %>% select(days_survival, dose)



bind_rows(
  surv_adjustedcurves(cox.dose.only, data = tibble(dose = 0)) %>% mutate(dosage = "Dose = 0"),
  surv_adjustedcurves(cox.dose.only, data = tibble(dose = 20)) %>% mutate(dosage = "Dose = 20"),
  surv_adjustedcurves(cox.dose.only, data = tibble(dose = 40)) %>% mutate(dosage = "Dose = 40"),
  surv_adjustedcurves(cox.dose.only, data = tibble(dose = 60)) %>% mutate(dosage = "Dose = 60"),
  surv_adjustedcurves(cox.dose.only, data = tibble(dose = 80)) %>% mutate(dosage = "Dose = 80")
) %>% 

  ggplot(aes(x = time, y = surv, group = dosage, colour = dosage)) +
  geom_point(shape = 3, size = 2) + 
  geom_line(size = 1) +
  scale_colour_manual(values=c("burlywood4", "deepskyblue4", "aquamarine4", "cadetblue4", "antiquewhite4")) +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "bottom",
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank(),
    legend.title = element_blank()
  )
  
  
  
scale_colour_manual()
predict(cox.dose.only, newdata = fake_data2, type = "risk")

ggadjustedcurves(cox.dose.only, data = fake_data2) +
  

?ggadjustedcurves


dat <- read_delim("data/ADDICTS.txt") %>% 
  
  janitor::clean_names() 


# Create survival object
surv_object <- Surv(dat$days_survival, dat$status)

# Fit the Kaplan-Meier curve
km.dose.strata <- survfit(surv_object ~ 1 + dose, data = dat)


# Fit a Cox regression
cox.dose.only <- coxph(surv_object ~ 1 + dose, data = dat)


ggsurvplot(surv_object)

# Get a zph object, which takes a Cox model object and returnslots of nice diagnostic information specifically about the proportional hazards assumption of that Cox model
zph_object_cox_dose <- cox.zph(cox.dose.only)

# Plot the Schoenfeld residuals with Survminer
survminer::ggcoxzph(zph_object_cox_dose, point.col = "cadetblue4")
