load(url("http://www.openintro.org/stat/data/evals.RData"))

nrow(evals) #463 rows in dataframe

# score - average professor evaluation score: (1) very unsatisfactory - (5) excellent.
# rank - rank of professor: teaching, tenure track, tenured.
# ethnicity - ethnicity of professor: not minority, minority.
# gender - gender of professor: female, male.
# language - language of school where professor received education: english or non-english.
# age - age of professor.
# cls_perc_eval - percent of students in class who completed evaluation.
# cls_did_eval - number of students in class who completed evaluation.
# cls_students - total number of students in class.
# cls_level - class level: lower, upper.
# cls_profs - number of professors teaching sections in course in sample: single, multiple.
# cls_credits - number of credits of class: one credit (lab, PE, etc.), multi credit.
# bty_f1lower - beauty rating of professor from lower level female: (1) lowest - (10) highest.
# bty_f1upper - beauty rating of professor from upper level female: (1) lowest - (10) highest.
# bty_f2upper - beauty rating of professor from second upper level female: (1) lowest - (10) highest.
# bty_m1lower - beauty rating of professor from lower level male: (1) lowest - (10) highest.
# bty_m1upper - beauty rating of professor from upper level male: (1) lowest - (10) highest.
# bty_m2upper - beauty rating of professor from second upper level male: (1) lowest - (10) highest.
# bty_avg - average beauty rating of professor.
# pic_outfit - outfit of professor in picture: not formal, formal. (not used in this analysis)
# pic_color - color of professors picture: color, black and white. (not used in this analysis)

str(evals)
summary(evals)

#analyze evals$score
hist(evals$score)
prop.table(table(evals$score > 4.6))
summary(evals$score)
median(evals$score)
table(evals$score < 3)

plot(evals$score ~ evals$bty_avg)

# use jitter() to add a small amount of noise to x, and then y
plot(evals$score ~ jitter(evals$bty_avg,factor = 1))
plot(jitter(evals$score,factor=1) ~ evals$bty_avg)

# Single Variable Linear regression

# fit a linear model called m_bty to predict average professor score 
# by average beauty rating and add the line to your plot using 
# abline(m_bty). 

m_bty = lm(score ~ bty_avg, data = evals)
summary(m_bty) 
#Average beauty score seems to be a statistically significant predictor

abline(m_bty)

# write out the equation for the linear model and interpret the slope.
# score  = 3.8804 + (0.6664*bty_avg)

plot(m_bty$residuals ~ evals$bty_avg)

test = lm(score ~ gender, data=evals)

# Multiple linear regression

plot(evals$bty_avg ~ evals$bty_f1lower)
cor(evals$bty_avg, evals$bty_f1lower) #0.8439112

# look at the relationships between all beauty variables:

plot(evals[,13:19])

# These variables are collinear (correlated)
# adding more than one of these variables to the model 
# would not add much value to the model

# It is reasonable to use the average beauty score as 
# the single representative of these variables.

# In order to see if beauty is still a significant 
# predictor of professor score after weve accounted 
# for the gender of the professor, we can add 
# the gender term into the model.

m_bty_gen = lm(score ~ bty_avg + gender, data = evals)
summary(m_bty_gen)

#Q6 ?

multiLines(m_bty_gen)

# You can change the reference level of a 
# categorical variable, which is the level 
# that is coded as a 0, using the relevel 
# function. Use ?relevel to learn more.

m_bty_rank = lm(score ~ bty_avg + rank , data = evals)
summary(m_bty_rank)

m_full = lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m_full)

step(m_full, direction = "backward", trace=T ) 












