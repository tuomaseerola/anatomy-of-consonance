# exp2_alternative_analysis.R
df <- read.csv('data/experiment2_data.csv',header = TRUE)

library(lme4)
library(lmerTest)
library(MuMIn)
library(ppcor)
library(caret)

#### 1. Null model -------------------------
# VARIANT where factors are NOT REMOVED (1|chord_size) + (1|dataset)
lmer.0 <- lmer(Rating ~   (1|RandomID),data=df,control = lmerControl(optimizer="Nelder_Mead"))
summary(lmer.0, correlation = FALSE) #
rand(lmer.0)           
r.squaredGLMM(lmer.0) #  0 0.127
round(AIC(lmer.0))    # 34709

#### 2. Harr20R model components --------
lmer.D23 <- update(lmer.0, . ~ . + har_18_harmonicity + har_19_corpus + chord_size)
lmer.D <- update(lmer.0, . ~ . + hutch_78_roughness + har_18_harmonicity + har_19_corpus + chord_size)
anova(lmer.D23,lmer.D,REML=FALSE,refit=FALSE) # chi^2=426.82, p<.001

lmer.D13 <- update(lmer.0, . ~ . + hutch_78_roughness + har_19_corpus +chord_size)
anova(lmer.D13,lmer.D,REML=FALSE,refit=FALSE) # chi^2=10.213, p=0.01

lmer.D12 <- update(lmer.0, . ~ . + hutch_78_roughness + har_18_harmonicity + chord_size)
anova(lmer.D12,lmer.D,REML=FALSE,refit=FALSE) # chi^2=590.4, p=.001

#### 2. Harr20R model -------------------------
lmer.RHF <- update(lmer.0, . ~ . + hutch_78_roughness + har_18_harmonicity + har_19_corpus + chord_size)
print(summary(lmer.RHF, correlation = FALSE)) #
s<-summary(lmer.RHF, correlation = FALSE) #
round(s$coefficients[,1],2)
#round(confint(lmer.RHF),2)
rand(lmer.RHF)
r.squaredGLMM(lmer.RHF) # 0.2096238 0.3331691
round(AIC(lmer.RHF))  # 31932

#### Are the models different for chord size?
model_randomF <- update(lmer.0, . ~ . + hutch_78_roughness + har_18_harmonicity + har_19_corpus + (1|chord_size))
model_fixedF <- update(lmer.0, . ~ . + hutch_78_roughness + har_18_harmonicity + har_19_corpus + chord_size)
print(anova(model_randomF,model_fixedF))
anova(model_fixedF,model_randomF)
r.squaredGLMM(model_randomF)
r.squaredGLMM(model_fixedF)
round(AIC(model_randomF))
round(AIC(model_fixedF))

#### 3. Optimise model -------------------------

#### 3 A Replace ROUGHNESS hutch_78_roughness ---------------- 
lmer.R1 <- update(lmer.0, . ~ . + seth_93_roughness + har_18_harmonicity + har_19_corpus + chord_size)
lmer.R2 <- update(lmer.0, . ~ . + vass_01_roughness + har_18_harmonicity + har_19_corpus + chord_size)
lmer.R3 <- update(lmer.0, . ~ . + wang_13_roughness + har_18_harmonicity + har_19_corpus + chord_size)
lmer.R4 <- update(lmer.0, . ~ . + spflux + har_18_harmonicity + har_19_corpus + chord_size)

anova(lmer.RHF,lmer.R1,REML=FALSE,refit=FALSE) # chi^2=0
anova(lmer.RHF,lmer.R2,REML=FALSE,refit=FALSE) # chi^2=0
anova(lmer.RHF,lmer.R3,REML=FALSE,refit=FALSE) # chi^2=0
anova(lmer.RHF,lmer.R4,REML=FALSE,refit=FALSE) # chi^2=0

#### 3 B. Replace HARMONICITY har_18_harmonicity ------------------
lmer.H1 <- update(lmer.0, . ~ . + hutch_78_roughness + milne_13_harmonicity + har_19_corpus + chord_size)
lmer.H2 <- update(lmer.0, . ~ . + hutch_78_roughness + parn_88_root_ambig + har_19_corpus + chord_size)
lmer.H3 <- update(lmer.0, . ~ . + hutch_78_roughness + parn_94_complex + har_19_corpus + chord_size)
lmer.H4 <- update(lmer.0, . ~ . + hutch_78_roughness + bowl_18_min_freq_dist + har_19_corpus + chord_size)
lmer.H5 <- update(lmer.0, . ~ . + hutch_78_roughness + TDL + har_19_corpus + chord_size)
lmer.H6 <- update(lmer.0, . ~ . + hutch_78_roughness + stolz_15_periodicity + har_19_corpus + chord_size)
lmer.H7 <- update(lmer.0, . ~ . + hutch_78_roughness + gill_09_harmonicity + har_19_corpus + chord_size)

anova(lmer.RHF,lmer.H1,REML=FALSE,refit=FALSE) # 0
anova(lmer.RHF,lmer.H2,REML=FALSE,refit=FALSE) # 15.612 ***
anova(lmer.RHF,lmer.H3,REML=FALSE,refit=FALSE) # 0
anova(lmer.RHF,lmer.H4,REML=FALSE,refit=FALSE) # 10.293  ***
anova(lmer.RHF,lmer.H5,REML=FALSE,refit=FALSE) # 111.17  ***
anova(lmer.RHF,lmer.H6,REML=FALSE,refit=FALSE) # 150.11  ***
anova(lmer.RHF,lmer.H7,REML=FALSE,refit=FALSE) # 9.3996  ***

#### 3 C Replace FAMILIARY har_19_corpus -----------------
lmer.F1 <- update(lmer.0, . ~ . + hutch_78_roughness + stolz_15_periodicity + keyclar + chord_size)
lmer.F2 <- update(lmer.0, . ~ . + hutch_78_roughness + stolz_15_periodicity + CorpPop + chord_size)
lmer.F3 <- update(lmer.0, . ~ . + hutch_78_roughness + stolz_15_periodicity + CorpJazz + chord_size)
lmer.F4 <- update(lmer.0, . ~ . + hutch_78_roughness + stolz_15_periodicity + CorpClas + chord_size)

anova(lmer.H2,lmer.F1)   # 0
anova(lmer.H2,lmer.F2)   # 431.65
anova(lmer.H2,lmer.F3)   # 0
anova(lmer.H2,lmer.F4)   # 0          

#### 3 D. Replace SPECTRAL ENVELOPE PREDICTORS -------------------------
lmer.S1 <- update(lmer.0, . ~ . + hutch_78_roughness + stolz_15_periodicity + CorpPop + spcentr + chord_size)
lmer.S2 <- update(lmer.0, . ~ . + hutch_78_roughness + stolz_15_periodicity + CorpPop + sharpness + chord_size)
lmer.S3 <- update(lmer.0, . ~ . + hutch_78_roughness + stolz_15_periodicity + CorpPop + spirreg + chord_size)
lmer.S4 <- update(lmer.0, . ~ . + hutch_78_roughness + stolz_15_periodicity + CorpPop + sproll + chord_size)

anova(lmer.F2,lmer.S1,REML=FALSE,refit=FALSE)  # 0
anova(lmer.F2,lmer.S2,REML=FALSE,refit=FALSE)  # 0
anova(lmer.F2,lmer.S3,REML=FALSE,refit=FALSE)  # 12.139
anova(lmer.F2,lmer.S4,REML=FALSE,refit=FALSE)  # 0

#### 4. Composite -------------------
lmer.HP <- update(lmer.0, . ~ . + har_19_composite)
summary(lmer.HP,correlation=FALSE)
r.squaredGLMM(lmer.HP) # 0.2019198 0.3096447


#### TABLE 2 ---------------------
# null model
summary(lmer.0,  correlation = FALSE)
#round(confint(lmer.0),2)
rand(lmer.0)
round(r.squaredGLMM(lmer.0),3) # 0 0.128
round(AIC(lmer.0)) # 34709

## original model
summary(lmer.RHF,  correlation = FALSE)
#round(confint(lmer.RHF),2)
s<-summary(lmer.RHF, correlation = FALSE) #
round(s$coefficients[,1],2)

rand(lmer.RHF)
round(r.squaredGLMM(lmer.RHF),3) 
round(AIC(lmer.RHF))

# Harr20R model
lmer.bestHFS <- update(lmer.0, . ~ . + hutch_78_roughness + stolz_15_periodicity + CorpPop + spirreg + chord_size)
summary(lmer.bestHFS,  correlation = FALSE)
s<-summary(lmer.bestHFS, correlation = FALSE) #
round(s$coefficients[,1],2)

#round(confint(lmer.bestHFS),2)
rand(lmer.bestHFS)
round(r.squaredGLMM(lmer.bestHFS),3) # 0.245 0.365
round(AIC(lmer.bestHFS)) # 31478

#### 6. Statisticas to text --------------------
# Composite vs optimised Harrison Pearce
anova(lmer.HP,lmer.RHF,REML=FALSE,refit=FALSE) # chi^2 = 309.36, ***

# original vs optimised
anova(lmer.RHF,lmer.bestHFS,REML=FALSE,refit=FALSE) # chi^2 = 456.09  ***

