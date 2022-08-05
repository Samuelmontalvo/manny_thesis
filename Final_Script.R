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
##### DESCRIPTIVES
Descriptives <- read_excel("Eccentric_Thesis.xlsx",
                        sheet = "Descriptives")
View(Descriptives)
attach(Descriptives)

#ALL
Descritives_all <- Descriptives %>% select("Age","Height","Weight",
                                           "VO2max","HR","Lactate","RPE")

Descritives_all %>%  
  describe(na.rm=T, skew=FALSE, ranges=F)%>%
  kbl(caption = "Descriptives All participants") %>% 
  kable_classic(full_width = F, html_font = "Cambria") 

# By Sex
describeBy(Descriptives ~ Sex, na.rm=T, skew=FALSE, ranges=F)


############################## ESS AND BF ###########################
Df <- read_excel("Eccentric_Thesis.xlsx",
                               sheet = "ESS_BFP")
View(Df)
attach(Df)

## Convert from character to factor data
Df$Sex <- as.factor(Df$Sex)
Df$Condition <- as.factor(Df$Condition)

## Order conditions
Df$Condition <- ordered(Df$Condition,
                          levels = c("Baseline", "Low",
                                     "Moderate","High"))

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
# Boxplot of Vertical Jump Height
Antegrade_ESS_plot <- ggboxplot(Df, x = "Condition", y = "ESS_Antegrade",
                                color = "Condition", palette = get_palette("Set1", 4),
                                ylab = "ESS Antegrade (dynes/cm2)") +
  stat_pvalue_manual(pwc,size = 4.5,hide.ns = TRUE) +
  theme_prism()
#Save Plot
ggsave("ESS_Antegrade.png")


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
# Boxplot of Vertical Jump Height
Antegrade_ESS_plot <- ggboxplot(Df, x = "Condition", y = "ESS_Retrograde",
                                color = "Condition", palette = get_palette("Set1", 4),
                                ylab = "ESS Retrograde (dynes/cm2)") +
  stat_pvalue_manual(pwc2,size = 4.5,hide.ns = TRUE)+
  theme_prism()## Change PWC
Antegrade_ESS_plot
#Save Plot
ggsave("ESS_Retrograde.png")

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
# Boxplot of Vertical Jump Height
Re_Antegrade_plot <- ggboxplot(Df, x = "Condition", y = "Re_Antegrade",
                                color = "Condition", palette = get_palette("Set1", 4),
                                ylab = "Reynolds number Antegrade") +
  stat_pvalue_manual(pwc3,size = 4.5,hide.ns = TRUE)+
  theme_prism()## Change pwc number
Re_Antegrade_plot
#Save Plot
ggsave("Re_Antegrade.png")

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
# Boxplot of Vertical Jump Height
Re_Retrograde_plot <- ggboxplot(Df, x = "Condition", y = "Re_Retrograde",
                               color = "Condition", palette = get_palette("Set1", 4),
                               ylab = "Reynolds number Retrograde") +
  stat_pvalue_manual(pwc4,size = 2.8,hide.ns = TRUE) ## Change pwc number
Re_Retrograde_plot
#Save Plot
ggsave("Re_Retrograde.png")


## Arrange 2 figures into 1 (if needed)
library(ggpubr)
ggarrange(Antegrade_ESS_plot, Antegrade_RE_plot,
          ncol = 2, nrow=1, labels = c("A)", "B)"))
ggsave("ESS_RE_B_gridplot.png")


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


## Wilcoxon for Non-parametric data
wilcox.test(FMD ~ Condition, data = Df2)

## Effect size
Df2  %>% wilcox_effsize(FMD ~ Condition)

## Figure FMD
FMD <- ggboxplot(Df2, x = "Condition", y = "FMD",
                color = "Condition", palette = c("#00AFBB", "#E7B800"),
                order = c("Pre", "Post"),
                ylab = "FMD", xlab = "Condition") + theme_prism() +
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
                 color = "Condition", palette = c("#00AFBB", "#E7B800"),
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
                 color = "Condition", palette = c("#00AFBB", "#E7B800"),
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
                            color = "Condition", palette = c("#00AFBB", "#E7B800"),
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
                                color = "Condition", palette = c("#00AFBB", "#E7B800"),
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
                         color = "Condition", palette = c("#00AFBB", "#E7B800"),
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
                            color = "Condition", palette = c("#00AFBB", "#E7B800"),
                            order = c("Pre", "Post"),
                            ylab = "Lower postischemia", xlab = "Condition") +
  theme_prism() +
  stat_compare_means(method = "wilcox.test", paired = F,
                     label.x = 1.4,
                     label.y = 18)
Lower_postischemia
ggsave("Lower_postischemia.png")


