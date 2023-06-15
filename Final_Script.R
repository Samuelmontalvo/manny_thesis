### Required packages
library(readxl) ## To load excel sheet
library(dplyr) # Data grammar and manipulation
library(rstatix) # Shapiro Wilk and effect size
library(psych) #descriptives
library(kableExtra) #tables
library(lme4) #linear mixed effects models (LMM)
library(lmerTest) #anova like output for LMM
library(ggplot2) #data visualization
library(ggpubr)#data visualization
library(ggprism)##makes plots look like graphad
library(table1) #for descriptives

############################ DESCRIPTIVES######################
Descriptives <- read_excel("Eccentric_Thesis.xlsx",
                        sheet = "Descriptives")
View(Descriptives)
attach(Descriptives)

Descriptives <- Descriptives %>%
  rename(HRmax = HR, Lactatemax=Lactate,RPEmax=RPE)

#ALL
Descriptives <- Descriptives %>%
  rename(HRmax = HR, Lactatemax=Lactate,RPEmax=RPE)

Descriptives <- Descriptives %>%  select("Age","Height","Weight",
                              "VO2max","HRmax","Lactatemax","RPEmax")

Descriptives %>%
  describe(na.rm=T, skew=FALSE, ranges=F)%>% mutate_if(is.numeric, round, 2) %>%
  kbl(caption = "Descriptives All participants") %>%
  kable_classic(full_width = F, html_font = "Cambria")


# By Sex
describeBy(Descriptives ~ Sex, na.rm=T, skew=FALSE, ranges=F)

Descriptives_max <- Descriptives %>% select(ID,VO2max,HRmax,Lactatemax,RPEmax)

############################## ESS AND BF ###########################
Df <- read_excel("Eccentric_Thesis.xlsx",
                               sheet = "ESS_BFP")
View(Df)
attach(Df)

Df <- left_join(Df,Descriptives_max, by ="ID")

Df <- Df %>% rename(VO2 = VO )

Df <- Df %>% mutate(VO2_perc = VO2 / VO2max * 100,
                    Lactate_perc = Lactate / Lactatemax * 100,
                    HR_perc = HR / HRmax * 100,
                    RPE_perc = RPE / RPEmax * 100)

## Convert from character to factor data
Df$Sex <- as.factor(Df$Sex)
Df$Condition <- as.factor(Df$Condition)

## Order conditions
Df$Condition <- ordered(Df$Condition,
                          levels = c("Baseline", "Low",
                                     "Moderate","High"))


table1(~ Age + Height + Weight + VO2max + HR + Lactate + RPE | Sex*Condition,
       total=F,render.categorical="FREQ (PCTnoNA%)", na.rm = TRUE,data=Df,
       render.missing=NULL,topclass="Rtable1-grid Rtable1-shade Rtable1-times",
       overall=FALSE)

################# DATA NORMALITY TEST ###############

##ESS normality
Df %>% group_by(Condition) %>%
  shapiro_test(ESS_Antegrade)

Df %>% group_by(Condition) %>%
  shapiro_test(ESS_Retrograde)

##Re normality
Df %>% group_by(Condition) %>%
  shapiro_test(Re_Antegrade)

Df %>% group_by(Condition) %>%
  shapiro_test(Re_Retrograde)


###### Linear Mixed models ESS Antegrade
lmModel = lmer(ESS_Antegrade ~ Condition + Sex + (1|ID),
               data=Df, REML=FALSE)
summary(lmModel)

# mixed model
anova(lmModel)
#test of the random effects in the model
rand(lmModel)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc <- Df %>%
  pairwise_t_test(ESS_Antegrade ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc %>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(ESS_Antegrade ~ Condition,
                       paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc <- pwc %>% add_xy_position(x = "Condition")
# Boxplot of ESS
Antegrade_ESS_plot <- ggboxplot(Df, x = "Condition", y = "ESS_Antegrade",
                                color = "Condition", palette = get_palette("Set1", 4),
                                ylab = "ESS Antegrade (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()
#Save Plot
ggsave("ESS_Antegrade.png")


# Boxplot of ESS by sex
ESS_BySex_Antegrade <- ggboxplot(Df, x = "Condition", y = "ESS_Antegrade",
                                color = "Sex", palette = get_palette("Set1", 4),
                                ylab = "ESS Antegrade (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()
#Save Plot
ggsave("ESS_BySex_Antegrade.png")

###### Linear Mixed models ESS retrograde
lmModel2 = lmer(ESS_Retrograde ~ Condition + Sex + (1|ID),
               data=Df, REML=FALSE)
summary(lmModel2)
# mixed model
anova(lmModel2)
#test of the random effects in the model
rand(lmModel2)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc2 <- Df %>%
  pairwise_t_test(ESS_Retrograde ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc2%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(ESS_Retrograde ~ Condition,
                     paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc2 <- pwc2 %>% add_xy_position(x = "Condition")
# Boxplot of ESS retrograde
Retrograde_ESS_plot <- ggboxplot(Df, x = "Condition", y = "ESS_Retrograde",
                                color = "Condition", palette = get_palette("Set1", 4),
                                ylab = "ESS Retrograde (dynes/cm2)") +
  stat_pvalue_manual(pwc2,size = 4.5,hide.ns = TRUE)+
  theme_prism()## Change PWC
Retrograde_ESS_plot
#Save Plot
ggsave("ESS_Retrograde.png")


Retrograde_By_sex_ESS_plot <- ggboxplot(Df, x = "Condition", y = "ESS_Retrograde",
                                 color = "Sex", palette = get_palette("Set1", 4),
                                 ylab = "ESS Retrograde (dynes/cm2)") +
  stat_pvalue_manual(pwc2,size = 4.5,hide.ns = TRUE)+
  theme_prism()## Change PWC
Retrograde_By_sex_ESS_plot
#Save Plot
ggsave("Retrograde_By_sex_ESS_plot.png")

###### Linear Mixed models RE antegrade
lmModel3 = lmer(Re_Antegrade ~ Condition + Sex + (1|ID),
                data=Df, REML=FALSE)
summary(lmModel3)
# mixed model
anova(lmModel3)
#test of the random effects in the model
rand(lmModel3)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc3 <- Df %>%
  pairwise_t_test(Re_Antegrade ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc3%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(Re_Antegrade ~ Condition,
                     paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")


#Plots
# Add position for p values in boxplot
pwc3 <- pwc3 %>% add_xy_position(x = "Condition") ## Change pwc number
# Boxplot of RE antegrade
Re_Antegrade_plot <- ggboxplot(Df, x = "Condition", y = "Re_Antegrade",
                                color = "Condition", palette = get_palette("Set1", 4),
                                ylab = "Reynolds number Antegrade") +
  stat_pvalue_manual(pwc3,size = 4.5,hide.ns = TRUE)+
  theme_prism()## Change pwc number
Re_Antegrade_plot
#Save Plot
ggsave("Re_Antegrade.png")

Re_Antegrade_Bysex_plot <- ggboxplot(Df, x = "Condition", y = "Re_Antegrade",
                               color = "Sex", palette = get_palette("Set1", 4),
                               ylab = "Reynolds number Antegrade") +
  stat_pvalue_manual(pwc3,size = 4.5,hide.ns = TRUE)+
  theme_prism()## Change pwc number
Re_Antegrade_Bysex_plot
#Save Plot
ggsave("Re_Antegrade_Bysex_plot.png")
###### Linear Mixed models RE retrograde
lmModel4 = lmer(Re_Retrograde ~ Condition + Sex + (1|ID),
                data=Df, REML=FALSE)
summary(lmModel4)
# mixed model
anova(lmModel4)
#test of the random effects in the model
rand(lmModel4)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc4 <- Df %>%
  pairwise_t_test(Re_Retrograde ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc4%>%
  kbl(caption = "Pairwise") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(Re_Retrograde ~ Condition,
                     paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

#Plots
# Add position for p values in boxplot
pwc4 <- pwc4 %>% add_xy_position(x = "Condition") ## Change pwc number
# Boxplot of Re retrograde
Re_Retrograde_plot <- ggboxplot(Df, x = "Condition", y = "Re_Retrograde",
                               color = "Condition", palette = get_palette("Set1", 4),
                               ylab = "Reynolds number Retrograde") +
  stat_pvalue_manual(pwc4,size = 2.8,hide.ns = TRUE) +
  theme_prism()## Change pwc number
Re_Retrograde_plot
#Save Plot
ggsave("Re_Retrograde.png")

# Boxplot of Re retrograde
Re_Retrograde_Bysex_plot <- ggboxplot(Df, x = "Condition", y = "Re_Retrograde",
                                color = "Sex", palette = get_palette("Set1", 4),
                                ylab = "Reynolds number Retrograde") +
  stat_pvalue_manual(pwc4,size = 2.8,hide.ns = TRUE) +
  theme_prism()## Change pwc number
Re_Retrograde_Bysex_plot
#Save Plot
ggsave("Re_Retrograde_Bysex_plot.png")

################################ FMD and VOP ##############################

library(readxl) ## to load excel file
Df2 <- read_excel("Eccentric_Thesis.xlsx",
                               sheet = "FMD_VOP")
View(Df2)
attach(Df2)

Df2$Condition <- as.factor(Df2$Condition)

## Shapiro Wilk
Df2 %>% group_by(Condition) %>% shapiro_test(FMD)
Df2 %>% group_by(Condition) %>% shapiro_test(Upper_Basal) #Normal
Df2 %>% group_by(Condition) %>% shapiro_test(Upper_ischemia)
Df2 %>% group_by(Condition) %>% shapiro_test(Upper_postischemia)
Df2 %>% group_by(Condition) %>% shapiro_test(Lower_Basal)
Df2 %>% group_by(Condition) %>% shapiro_test(Lower_ischemia) #Normal
Df2 %>% group_by(Condition) %>% shapiro_test(Lower_postischemia)
Df2 %>% group_by(Condition) %>% shapiro_test(FMD_Basal) #Normal
Df2 %>% group_by(Condition) %>% shapiro_test(FMD_Peak) #Normal
Df2 %>% group_by(Condition) %>% shapiro_test(FMD_Difference)

## Wilcoxon for Non-parametric data
wilcox.test(FMD ~ Condition, data = Df2)

## Effect size
Df2  %>% wilcox_effsize(FMD ~ Condition)

## Figure FMD
FMD <- ggboxplot(Df2, x = "Condition", y = "FMD",
                color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                order = c("Pre", "Post"),
                ylab = "FMD", xlab = "Condition")  +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 28)
FMD
ggsave("FMD.png")

########## Upper_Basal

## Descriptives
## T-test for parametric data
t_test(Upper_Basal ~ Condition, data = Df2)

## Effect size
Df2  %>% cohens_d(Upper_Basal ~ Condition,
                  paired = TRUE, hedges.correction = TRUE)

## Figure FMD
Upper_Basal <- ggboxplot(Df2, x = "Condition", y = "Upper_Basal",
                 color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                 order = c("Pre", "Post"),
                 ylab = "Upper Basal", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "t.test", paired = F,
                     label.x = 1.4,
                     label.y = 20)
Upper_Basal
ggsave("Upper_Basal.png")

####### Upper Ischemia

## Kruskall Wallis for Non-parametric data
wilcox.test(Upper_ischemia ~ Condition, data = Df2)

## Effect size
Df2  %>% wilcox_effsize(Upper_ischemia ~ Condition)

## Figure Upper_ischemia
Upper_ischemia <- ggboxplot(Df2, x = "Condition", y = "Upper_ischemia",
                 color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                 order = c("Pre", "Post"),
                 ylab = "Upper ischemia", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 28)
Upper_ischemia
ggsave("Upper_ischemia.png")


####### Upper  Post Ischemia

## Descriptives
## Kruskall Wallis for Non-parametric data
wilcox.test(Upper_postischemia ~ Condition, data = Df2)

## Effect size
Df2  %>% wilcox_effsize(Upper_postischemia ~ Condition)

## Figure Upper_postischemia
Upper_postischemia <- ggboxplot(Df2, x = "Condition", y = "Upper_postischemia",
                            color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                            order = c("Pre", "Post"),
                            ylab = "Upper postischemia", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = TRUE,
                     label.x = 1.4,
                     label.y = 17)
Upper_postischemia
ggsave("Upper_postischemia.png")

####### lower basal

## Descriptives
## Kruskall Wallis for Non-parametric data
wilcox.test(Lower_Basal ~ Condition, data = Df2)

## Effect size
Df2  %>% wilcox_effsize(Lower_Basal ~ Condition)

## Figure Lower_Basal
Lower_Basal <- ggboxplot(Df2, x = "Condition", y = "Lower_Basal",
                                color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                                order = c("Pre", "Post"),
                                ylab = "Lower Basal", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 16)
Lower_Basal
ggsave("Lower_Basal.png")


########## Lower Ischemia

## T-test for parametric data
t_test(Lower_ischemia ~ Condition, data = Df2)

## Effect size
Df2  %>% cohens_d(Lower_ischemia ~ Condition,
                  paired = TRUE, hedges.correction = TRUE)

## Figure Lower_ischemia
Lower_ischemia <- ggboxplot(Df2, x = "Condition", y = "Lower_ischemia",
                         color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                         order = c("Pre", "Post"),
                         ylab = "Lower ischemia", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "t.test", paired = F,
                     label.x = 1.4,
                     label.y = 35)
Lower_ischemia
ggsave("Lower_ischemia.png")

########## Lower Post Ischemia

## wilcoxon for parametric data
wilcox.test(Lower_postischemia ~ Condition, data = Df2)

## Effect size
Df2  %>% wilcox_effsize(Lower_postischemia ~ Condition)

## Figure Lower_postischemia
Lower_postischemia <- ggboxplot(Df2, x = "Condition", y = "Lower_postischemia",
                            color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                            order = c("Pre", "Post"),
                            ylab = "Lower postischemia", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 18)
Lower_postischemia
ggsave("Lower_postischemia.png")

## FMD BASAL t-test

## T-test for parametric data
t_test(FMD_Basal ~ Condition, data = Df2)

## Effect size
Df2  %>% cohens_d(FMD_Basal ~ Condition,
                  paired = TRUE, hedges.correction = TRUE)

## Figure FMD
FMD_Basal <- ggboxplot(Df2, x = "Condition", y = "FMD_Basal",
                         color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                         order = c("Pre", "Post"),
                         ylab = "FMD Basal", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "t.test", paired = F,
                     label.x = 1.4,
                     label.y = 4.7)
FMD_Basal
ggsave("FMD_Basal.png")


## FMD PEAK t-test

## T-test for parametric data
t_test(FMD_Peak ~ Condition, data = Df2)

## Effect size
Df2  %>% cohens_d(FMD_Peak ~ Condition,
                  paired = TRUE, hedges.correction = TRUE)

## Figure FMD
FMD_Peak <- ggboxplot(Df2, x = "Condition", y = "FMD_Peak",
                         color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                         order = c("Pre", "Post"),
                         ylab = "FMD Peak", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "t.test", paired = F,
                     label.x = 1.4,
                     label.y = 5)
FMD_Peak
ggsave("FMD_Peak.png")

########## FMD difference

## wilcoxon for parametric data
wilcox.test(FMD_Difference ~ Condition, data = Df2)

## Effect size
Df2  %>% wilcox_effsize(FMD_Difference ~ Condition)

## Figure Lower_postischemia
FMD_Difference <- ggboxplot(Df2, x = "Condition", y = "FMD_Difference",
                                color = "Condition", palette = c("#00AFBB", "#FC4E07"),
                                order = c("Pre", "Post"),
                                ylab = "FMD Difference", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 27)
FMD_Difference
ggsave("FMD_Difference.png")

Descriptive_FMD <- Df2 %>% select("Condition","FMD_Basal","FMD_Peak","FMD_Difference")

p1 <- Df %>% ggplot(aes(Condition,HR, fill=Sex, color=Sex))+ ylab("Heart Rate (bpm)")+
  geom_jitter() + theme_prism()

p2 <- Df %>% ggplot(aes(Condition,RPE, fill=Sex, color=Sex))+ ylab("RPE") +
  geom_jitter() + theme_prism()
p2

p3 <- Df %>% ggplot(aes(Condition,Lactate, fill=Sex, color=Sex))+ ylab("Lactate mmol/L") +
  geom_jitter() + theme_prism()
p3

ggarrange(p1,p2,p3)

############################ CLINICAL ECCENTRIC OUTCOMES #############


table1(~ workload + VO2 + VO2_perc + Lactate + Lactate_perc + HR + HR_perc + RPE  | Sex*Condition,
       total=F,render.categorical="FREQ (PCTnoNA%)", na.rm = TRUE,data=Df,
       render.missing=NULL,topclass="Rtable1-grid Rtable1-shade Rtable1-times",
       overall=FALSE)

###### Linear Mixed models VO2
lmModel5 = lmer(VO2 ~ Condition + Sex + (1|ID),
                data=Df, REML=FALSE)
summary(lmModel5)
# mixed model
anova(lmModel5)
#test of the random effects in the model
rand(lmModel5)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc5 <- Df %>%
  pairwise_t_test(VO2 ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc5%>%
  kbl(caption = "Pairwise comparison") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(VO2 ~ Condition,
                paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

###### Linear Mixed models RE antegrade
lmModel6 = lmer(HR ~ Condition + Sex + (1|ID),
                data=Df, REML=FALSE)
summary(lmModel6)
# mixed model
anova(lmModel6)
#test of the random effects in the model
rand(lmModel6)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc6 <- Df %>%
  pairwise_t_test(HR  ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc6%>%
  kbl(caption = "Pairwise comparison") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(HR  ~ Condition,
                paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")

###### Linear Mixed models RE antegrade
lmModel7 = lmer(Lactate ~ Condition + Sex + (1|ID),
                data=Df, REML=FALSE)
summary(lmModel7)
# mixed model
anova(lmModel7)
#test of the random effects in the model
rand(lmModel7)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc6 <- Df %>%
  pairwise_t_test(Lactate ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc6%>%
  kbl(caption = "Pairwise comparison") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(Re_Antegrade ~ Condition,
                paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")


###### Linear Mixed models RE antegrade
lmModel8 = lmer(RPE ~ Condition + Sex + (1|ID),
                data=Df, REML=FALSE)
summary(lmModel8)
# mixed model
anova(lmModel8)
#test of the random effects in the model
rand(lmModel8)

# Post-hoc pairwise comparisons Holms-Bonferroni correction
pwc8 <- Df %>%
  pairwise_t_test(RPE ~ Condition, paired = TRUE,
                  p.adjust.method	= "holm")
pwc8%>%
  kbl(caption = "Pairwise comparison") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Effect size Cohen's D with Hedge's g correction for small sample size
Df %>% cohens_d(RPE ~ Condition,
                paired = TRUE, hedges.correction = TRUE)%>%
  kbl(caption = "Effect Size") %>%
  kable_classic(full_width = F, html_font = "Cambria")
