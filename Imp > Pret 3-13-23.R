#Order of instruction of preterit-imperfect project code
#3-13-23
#Sophia Minnillo

#project about whether order of instruction
#impacts L2 Spanish preterit-imperfect accuracy and suppliance

#load packages
library(tidyverse)
library(stringr)
library(broom)
library(lme4)
library(psych)
library(afex)
library(sjPlot)
library(effects)
library(sjstats)
library(ngramr)
library(Rmisc) #for SummarySE
library(plyr); library(dplyr)
library(rstatix)


## Load CSV ####
#load csv with data
csv <- read_csv("SPA_3_Control_Experimental_03_07_23 - Sheet1.csv")%>%
  dplyr::mutate(Form = tolower(Form),
                `Corrected Form` = tolower(`Corrected Form`))
#View(csv)

## Clean CSV ####
#turn NAs to 0s
csv$Appropriate[is.na(csv$Appropriate)] <- 0
csv$Ambiguous[is.na(csv$Ambiguous)] <- 0
csv$Inappropriate[is.na(csv$Inappropriate)] <- 0

#case when for form: turning different forms into numbers
csv1 <- csv %>%
  dplyr::mutate(Form_num = case_when(
    Form == 'preterit' | 
      Form == 'preterite' |
      Form == 'PRETERIT' |
      Form == 'pretérito' ~ 1,
    Form == 'imperfect' |
      Form == 'IMPERFECT' |
      Form == 'imperfecto' ~ 2,
    Form == 'present' |
      Form == 'PRESENT' |
      Form == 'presente' ~ 3,
    Form == 'infinitive' |
      Form == 'INFINITIVE' |
      Form == 'infinitivo' |
      Form == 'infinative'~ 4,
    Form == 'gerund' |
      Form == 'GERUND' |
      Form == 'Gerund' |
      Form == 'gerundio' ~ 5,
    Form == 'pluscuamperfecto' |
      Form == 'PLUSCUAMPERFECTO' |
      Form == 'PLUPERFECT' |
      Form == 'past perfect' |
      Form == 'PAST PERFECT' |
      Form == 'pluperfect' ~ 6, #added 8-15
    Form == 'imperfect subjunctive' |
      Form == 'IMPERFECT SUBJUNCTIVE' |
      Form == 'PAST SUBJUNCTIVE' |
      Form == 'past subjunctive' ~ 7,
    Form == 'present subjunctive' |
      Form == 'PRESENT SUBJUNCTIVE' |
      `Form` == 'subjunctive' ~ 8, #added 7-29
    Form == 'command' |
      Form == 'COMMAND' |
      Form == 'IMPERATIVE' |
      Form == 'imperative' |
      Form == 'imperativo' ~ 9, #added 7-29
    Form == 'conditional' |
      Form == 'CONDITIONAL' |
      Form == 'condicional' ~ 10, #added 7-29
    Form == 'future' |
      Form == 'FUTURE' |
      Form == 'FUTURO' |
      `Form` == 'Future' |
      Form == 'futuro' ~ 11, #added 7-29
    Form == 'AMBIGUOUS' |
      Form == 'ambiguous' |
      Form == 'ambiguo' ~ 12, #added 7-30
    Form == 'past participle' |
      Form == 'PAST PARTICIPLE' |
      Form == 'participio pasado' ~ 13, #added 8-15
    Form == 'present perfect' |
      Form == 'PRESENT PERFECT'~ 14, #added 8-15
    Form == 'pluperfect subjunctive' |
      Form == 'PLUPERFECT SUBJUNCTIVE' ~ 15,
    Form == 'English' ~ 16,
    TRUE ~ 1000))%>% #anything else gets a large # to draw our attention
  dplyr::mutate(Corrected_num = case_when(
    `Corrected Form` == 'preterit' |
      `Corrected Form` == 'preterite' |
      `Corrected Form` == 'PRETERIT' |
      `Corrected Form` == 'preterito' ~ 1,
    `Corrected Form` == 'imperfect' |
      `Corrected Form` == 'IMPERFECT' |
      `Corrected Form` == 'imperfecto' ~ 2,
    `Corrected Form` == 'present' |
      `Corrected Form` == 'PRESENT' |
      `Corrected Form` == 'presente' ~ 3,
    `Corrected Form` == 'infinitive' |
      `Corrected Form` == 'INFINITIVE' |
      `Corrected Form` == 'infinitivo' |
      `Corrected Form` == 'infinative'~ 4,
    `Corrected Form` == 'gerund' |
      `Corrected Form` == 'GERUND' |
      `Corrected Form` == 'Gerund' |
      `Corrected Form` == 'gerundio' ~ 5,
    `Corrected Form` == 'pluscuamperfecto' |
      `Corrected Form` == 'PLUSCUAMPERFECTO' |
      `Corrected Form` == 'PLUPERFECT' |
      `Corrected Form` == 'past perfect' |
      `Corrected Form` == 'PAST PERFECT' |
      `Corrected Form` == 'pluscuamperfect'|
      `Corrected Form` == 'pluperfect'~ 6,
    `Corrected Form` == 'imperfect subjunctive' |
      `Corrected Form` == 'IMPERFECT SUBJUNCTIVE' |
      `Corrected Form` == 'PAST SUBJUNCTIVE' |
      `Corrected Form` == 'past subjunctive' ~ 7,
    `Corrected Form` == 'present subjunctive' |
      `Corrected Form` == 'PRESENT SUBJUNCTIVE' |
      `Corrected Form` == 'subjunctive' |
      `Corrected Form` == 'subjuntive' ~ 8, #added 7-29
    `Corrected Form` == 'command' |
      `Corrected Form` == 'COMMAND' |
      `Corrected Form` == 'IMPERATIVE' |
      `Corrected Form` == 'imperative' |
      `Corrected Form` == 'imperativo' ~ 9, #added 7-29
    `Corrected Form` == 'conditional' |
      `Corrected Form` == 'CONDITIONAL' |
      `Corrected Form` == 'condicional' ~ 10, #added 7-29
    `Corrected Form` == 'future' |
      `Corrected Form` == 'FUTURE' |
      `Corrected Form` == 'FUTURO' |
      `Corrected Form` == 'Future' |
      `Corrected Form` == 'futuro' ~ 11, #added 7-29
    `Corrected Form` == 'AMBIGUOUS' |
      `Corrected Form` == 'ambiguous' |
      `Corrected Form` == 'Ambiguous' |
      `Corrected Form` == 'Ambigous' |
      `Corrected Form` == 'ambiguo' ~ 12,
    `Corrected Form` == 'past participle' |
      `Corrected Form` == 'PAST PARTICIPLE' |
      `Corrected Form` == 'participio pasado' ~ 13, #added 8-15
    `Corrected Form` == 'present perfect' |
      `Corrected Form` == 'PRESENT PERFECT' ~ 14, #added 8-15
    `Corrected Form` == 'pluperfect subjunctive' |
      `Corrected Form` == 'PLUPERFECT SUBJUNCTIVE' ~ 15, #Added 9-14
    `Corrected Form` == 'English' ~ 16,
    TRUE ~ 1000))

#View(csv1)

#just making sure that orthography of tenses
#is standardized
csv18 <- csv1 %>%
  dplyr::mutate(Corrected_Form1 = case_when(
    Corrected_num == 1 ~ 'preterit',
    Corrected_num == 2 ~ 'imperfect',
    Corrected_num == 3 ~ 'present',
    TRUE ~ 'other'))%>%
  dplyr::mutate(Form_num1 = case_when(
    Form_num == 1 ~ 'preterit',
    Form_num == 2 ~ 'imperfect',
    Form_num == 3 ~ 'present',
    TRUE ~ 'other'))

#View(csv18)


#remove ambiguous and use of English cases
csv18_token_cde_clean <- csv18 %>%
  filter(Ambiguous != 1 & Form != 'english')%>%
  #601 tokens
  #only cases where preterit or imperfect are accurate
  filter(Corrected_Form1 == 'preterit'| Corrected_Form1 == 'imperfect')
  # reduces down to 583 tokens
#view(csv18_token_cde_clean)

#write_csv(csv18_token_cde_clean, "Control_experimental_3_7_23_cleaned.csv")

## ACCURACY ####
# The accuracy models are based on whether or not the preterit-imperfect
# are used in cases where they would be appropriate in the context of narration.

#MODEL STRUCTURE

#logistic mixed-effects model

#response variable:
# accuracy (0-1)

#independent variables:
# group: control vs. experimental
# appropriate form: preterit vs. imperfect

#random intercepts:
# participant

### GLMM- preterit & imperfect accuracy of use ####
model1 <- glmer(Appropriate ~ Group * Corrected_Form1 +
                  (1|ID),
                csv18_token_cde_clean,
                family = 'binomial', 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))
summary(model1)
#significant main effect of appropriate tense-aspect form
#more accurate in preterit-appropriate contexts
#no effect of group (intervention) on accuracy

#get rid of interaction term
model1_1 <- glmer(Appropriate ~ Group + Corrected_Form1 +
                  (1|ID),
                csv18_token_cde_clean,
                family = 'binomial', 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))
summary(model1_1)
#still no significant effect of experimental group

#just preterit
csv18_token_cde_clean_pret <- csv18_token_cde_clean %>%
  filter(Corrected_Form1 == 'preterit')

model2 <- glmer(Appropriate ~ Group +
                  (1|ID),
                csv18_token_cde_clean_pret,
                family = 'binomial', 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))
summary(model2)
#still no main effect of group

#just imperfect
csv18_token_cde_clean_imp <- csv18_token_cde_clean %>%
  filter(Corrected_Form1 == 'imperfect')
#view(csv18_token_cde_clean_imp)
#161 observations

model3 <- glmer(Appropriate ~ Group +
                  (1|ID),
                csv18_token_cde_clean_imp,
                family = 'binomial', 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=50000)))
summary(model3)
#still no main effect of group

### Descriptive stats ####
#just cases where the preterit and imperfect are appropriate
pret_imp_appr <- read_csv("Control_experimental_3_7_23_cleaned.csv")%>%
  mutate(Appropriate = Appropriate * 100)
#view(pret_imp_appr)

#use summary se
pret_imp_appr_se <- summarySE(data = pret_imp_appr, 
                               measurevar = 'Appropriate', 
                               groupvars = c('Group', 'Corrected_Form1'),
                               na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)
#view(pret_imp_appr_se)

#visualize in graph
ggplot(pret_imp_appr_se, aes(x=Group, y=Appropriate, fill = Group)) + 
  facet_wrap(~Corrected_Form1)+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=Appropriate-ci, ymax=Appropriate+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  ylim(0,100)+
  ylab('Percent accuracy')+ 
  scale_fill_brewer(palette="Accent")+ 
  theme(text = element_text(size = 15))

### Alternate tenses in cases where pret-imp accurate ####
#preterit appropriate
csv18_token_cde_clean_pret1 <-csv18_token_cde_clean_pret %>%
  group_by(Group, Form_num1)%>%
  dplyr::summarize(count = n())%>%
  pivot_wider(names_from = Form_num1, values_from = count)%>%
  replace(is.na(.), 0)%>%
  dplyr::rowwise()%>%
  dplyr::mutate(
    total = sum(other, present, preterit, imperfect),
    Preterit = preterit / total * 100,
    Imperfect = imperfect / total * 100,
    Present = present / total * 100,
    Other = other / total * 100
  )%>%
  select(Group, Preterit, Imperfect, Present, Other)%>%
  pivot_longer(
    cols = c(Preterit, Imperfect, Present, Other),
    names_to = 'Tense-aspect form',
    values_to = 'Percent use'
  )

#view(csv18_token_cde_clean_pret1)

#create a bar graph with this information
#now graph this with stacked graph
ggplot(csv18_token_cde_clean_pret1, aes(x = Group, y = `Percent use`, fill = `Tense-aspect form`))+
  geom_bar(stat = "identity", position = "stack")+
  #facet_wrap(~n_person1)+
  theme_minimal()+
  #labs(x = "Course level", y = "Percent use of forms in preterit-appropriate contexts")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.80, vjust = 0.9),
        text = element_text(size = 15))+
  scale_fill_brewer(palette="Set2")


#imperfect appropriate
csv18_token_cde_clean_imp1 <-csv18_token_cde_clean_imp %>%
  group_by(Group, Form_num1)%>%
  dplyr::summarize(count = n())%>%
  pivot_wider(names_from = Form_num1, values_from = count)%>%
  replace(is.na(.), 0)%>%
  dplyr::rowwise()%>%
  dplyr::mutate(
    total = sum(other, present, preterit, imperfect),
    Preterit = preterit / total * 100,
    Imperfect = imperfect / total * 100,
    Present = present / total * 100,
    Other = other / total * 100
  )%>%
  select(Group, Preterit, Imperfect, Present, Other)%>%
  pivot_longer(
    cols = c(Preterit, Imperfect, Present, Other),
    names_to = 'Tense-aspect form',
    values_to = 'Percent use'
  )%>%
  dplyr::mutate(`Tense-aspect form` = 
                  factor(`Tense-aspect form`, 
                         levels = (c('Other', 'Present', 'Preterit', 'Imperfect'))))

#view(csv18_token_cde_clean_imp1)

#create a bar graph with this information
#now graph this with stacked graph
ggplot(csv18_token_cde_clean_imp1, aes(x = Group, y = `Percent use`, fill = `Tense-aspect form`))+
  geom_bar(stat = "identity", position = "stack")+
  #facet_wrap(~n_person1)+
  theme_minimal()+
  #labs(x = "Course level", y = "Percent use of forms in preterit-appropriate contexts")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.80, vjust = 0.9),
        text = element_text(size = 15))+
  scale_fill_brewer(palette="Set2")

## SUPPLIANCE ####

#cleaned dataset only looking at suppliance
#remove ambiguous and use of English cases
csv18_suppliance <- csv18 %>%
  filter(Ambiguous != 1 & Form != 'english')
#view(csv18_suppliance)

#summarize by # marked per essay
csv18_suppliance_sum <- csv18_suppliance %>%
  dplyr::group_by(ID, Group, Form_num1)%>%
  dplyr::summarise(count = n())
#view(csv18_suppliance_sum)

#pivot out
csv18_suppliance_sum1 <- csv18_suppliance_sum %>%
  pivot_wider(names_from = Form_num1, values_from = count)

#replace NAs with 0s
csv18_suppliance_sum1$imperfect[is.na(csv18_suppliance_sum1$imperfect)] <- 0
csv18_suppliance_sum1$other[is.na(csv18_suppliance_sum1$other)] <- 0
csv18_suppliance_sum1$present[is.na(csv18_suppliance_sum1$present)] <- 0
csv18_suppliance_sum1$preterit[is.na(csv18_suppliance_sum1$preterit)] <- 0

#now pivot longer
csv18_suppliance_sum1 <-csv18_suppliance_sum1 %>%
  pivot_longer(cols= (3:6), names_to = 'tense', values_to = 'count')
  
#view(csv18_suppliance_sum1)

#now add metadata about text length
meta_txt <- read_csv('SPA_3_Control_Experimental_03_07_23 - text length.csv')
#view(meta_txt)

#calculating mean use per 100 words to account for text length
csv18_suppliance_sum1 <-csv18_suppliance_sum1 %>%
  left_join(meta_txt)%>%
  dplyr::mutate(count_by_length = count / length *100)

#view(csv18_suppliance_sum1)

#summary SE for use
pret_imp_supl_se <- summarySE(data = csv18_suppliance_sum1, 
                    measurevar = 'count_by_length', 
                    groupvars = c('Group', 'tense'),
                    na.rm = FALSE, conf.interval = 0.95, .drop = TRUE)%>%
  filter(tense == 'preterit'|tense == 'imperfect')
#view(pret_imp_supl_se)


#graph
ggplot(pret_imp_supl_se, aes(x=Group, y=count_by_length, fill=Group)) + 
  facet_wrap(~tense)+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=count_by_length-ci, ymax=count_by_length+ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  labs(x = "Group", y = "Suppliance per 100 words")+
  ylim(0,10)+
  scale_fill_brewer(palette="Accent")+ 
  theme(text = element_text(size = 15))

### anova- does suppliance differ by group and appropriate tense-aspect? ####
csv18_suppliance_sum1_anova <- csv18_suppliance_sum1 %>%
  filter(tense == 'imperfect'| tense == 'preterit')
#view(csv18_suppliance_sum1_anova)

aov_supl <- aov(count_by_length ~ Group * tense, csv18_suppliance_sum1_anova)
aov_supl
summary(aov_supl)

#yes, suppliance differs by group and tense with no interaction

#view(csv18_token_cde_clean_imp)



#Thanks for reading!