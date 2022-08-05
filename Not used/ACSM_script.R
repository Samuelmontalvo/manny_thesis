library(readxl)
ACSM <- read_excel("ACSM.xlsx")
View(ACSM)
attached(ACSM)

## Convert from character to factor
ACSM$Gender <- as.factor(ACSM$Gender)
ACSM$Condition <- as.factor(ACSM$Condition)
ACSM$Type <- as.factor(ACSM$Type)

ACSM$Condition <- ordered(ACSM$Condition,
                        levels = c("Baseline","Low","Moderate","High"))

## Shapiro wilk by group
library(dplyr)
library(rstatix)

##ESS normality
ACSM %>% group_by(Condition) %>%
  shapiro_test(ESS)

##Re normality
ACSM %>% group_by(Condition) %>%
  shapiro_test(Re)


## Filter to antegrade
Antegrade <- ACSM %>%
  filter(Type == "Antegrade")
attach(Antegrade)


# Repeated Measures Anova (within 2 x 3 subjects)
res.aov <- anova_test(data = Antegrade, dv = ESS, wid = ID, within = Condition)
get_anova_table(res.aov)
# Greenhouse-Geisser sphericity correction is automatically applied-
#-through the Mauchly's Test for Sphericity
get_anova_table(res.aov)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Antegrade %>%
  group_by(Type) %>%
  pairwise_t_test(ESS ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc

# Effect size Cohen's D with Hedge's g correction for small sample size
Antegrade %>% cohens_d(ESS ~ Condition,
            paired = TRUE, hedges.correction = TRUE)

#Plots
library(ggplot2)
library(ggpubr)
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Condition")
# Boxplot of Vertical Jump Height
Antegrade_ESS_plot <- ggboxplot(Antegrade, x = "Condition", y = "ESS",
                      color = "Condition", palette = get_palette("Set1", 4),
                      ylab = "Antegrade ESS (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 2.8,hide.ns = TRUE)
#Save Plot
ggsave("Antegrade_ESS_acsm.png")


## Antegrade Reynols B analysis
# Repeated Measures Anova (within 2 x 3 subjects)
res.aov2 <- anova_test(data = Antegrade, dv = Re,
                       wid = ID, within = Condition,effect.size = "pes")
# Greenhouse-Geisser sphericity correction is automatically applied-
#-through the Mauchly's Test for Sphericity
get_anova_table(res.aov2)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc2 <- Antegrade %>%
  pairwise_t_test(Re ~ Condition, paired = TRUE,
                  p.adjust.method	= "none")
pwc2

# Effect size Cohen's D with Hedge's g correction for small sample size
Antegrade %>%
  cohens_d(Re ~ Condition,
           paired = TRUE, hedges.correction = TRUE)


# Boxplot of Diameter
# Add position for p values in boxplot
pwc2 <- pwc2 %>% add_xy_position(x = "Condition")
Antegrade_RE_plot <- ggboxplot(Antegrade, x = "Condition", y = "Re",
                       color = "Condition", palette = get_palette("Set1", 4),
                       ylab = "Antegrade Reynolds number B") +
  stat_pvalue_manual(pwc2,size = 2.8,hide.ns = TRUE)
#Save Plot
ggsave("Antegrade_re_acsm.png")





## Filter to RETROGRADE
Retrograde <- ACSM %>%
  filter(Type == "Retrograde")
attach(Retrograde)


# Repeated Measures Anova (within 2 x 3 subjects)
res.aov3 <- anova_test(data = Retrograde, dv = ESS, wid = ID, within = Condition)
get_anova_table(res.aov3)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc3 <- Retrograde %>%
  pairwise_t_test(ESS ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc3

# Effect size Cohen's D with Hedge's g correction for small sample size
Retrograde %>% cohens_d(ESS ~ Condition,
                       paired = TRUE, hedges.correction = TRUE)

#Plots
library(ggplot2)
library(ggpubr)
# Add position for p values in boxplot
pwc3 <- pwc3 %>% add_xy_position(x = "Condition")
# Boxplot of Vertical Jump Height
Retrograde_ESS_plot <- ggboxplot(Retrograde, x = "Condition", y = "ESS",
                                color = "Condition", palette = get_palette("Set1", 4),
                                ylab = "Retrograde ESS (dynes/cm2)") +
  stat_pvalue_manual(pwc3,size = 2.8,hide.ns = TRUE)
#Save Plot
ggsave("Retrograde_ESS_acsm.png")


## Retrograde Reynols B analysis
# Repeated Measures Anova (within 2 x 3 subjects)
res.aov4 <- anova_test(data = Retrograde, dv = Re,
                       wid = ID, within = Condition,effect.size = "pes")
# Greenhouse-Geisser sphericity correction is automatically applied-
#-through the Mauchly's Test for Sphericity
get_anova_table(res.aov4)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc4 <- Retrograde %>%
  pairwise_t_test(Re ~ Condition, paired = TRUE,
                  p.adjust.method	= "none")
pwc4

# Effect size Cohen's D with Hedge's g correction for small sample size
Retrograde %>%
  cohens_d(Re ~ Condition,
           paired = TRUE, hedges.correction = TRUE)


# Boxplot of Diameter
# Add position for p values in boxplot
pwc4 <- pwc4 %>% add_xy_position(x = "Condition")
Retrograde_re_plot <- ggboxplot(Retrograde, x = "Condition", y = "Re",
                       color = "Condition", palette = get_palette("Set1", 4),
                       ylab = "Retrograde Reynolds number B") +
  stat_pvalue_manual(pwc4,size = 2.8,hide.ns = TRUE)
#Save Plot
ggsave("Retrograde_re_acsm.png")

## Arrange 2 figures into 1
library(ggpubr)
ggarrange(Antegrade_ESS_plot, Antegrade_RE_plot,
          ncol = 2, nrow=1, labels = c("A)", "B)"))
ggsave("ESS_RE_B_gridplot.png")
