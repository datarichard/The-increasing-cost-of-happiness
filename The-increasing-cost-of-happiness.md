---
title: "The increasing cost of happiness"
author: "Dr Richard Morris"
date: "Updated: 2020-09-05"
bibliography: references.bib
csl: elsevier-harvard.csl
output: 
  html_document:
    keep_md: true
---



## Abstract
A fundamental question for governments and people is how much happiness does a dollar buy? The accepted view among economists and psychologists is that money and happiness increase together up to a point, after which there is little further gain from increasing wealth. While the location of this _change point_ has been determined, and the cost of happiness reportedly ranges between USD\$60-95K, there has been no investigation as to whether the cost of happiness has increased or decreased over time.   

<br>

### Introduction
***
A fundamental question for the psychology and economics of wellbeing is just how much happiness does a dollar buy? Increasing wealth is commonly associated with increasing happiness, however a point at which wealth no longer increases happiness has also been [widely observed](https://www-sciencedirect-com.ezproxy2.library.usyd.edu.au/science/article/pii/S0167487007000694){target="_blank"} [@dolan2008we; @easterlin1974growth]. Given that a central goal of nations and governments is to improve wealth under the assumption that wealth always increases wellbeing, challenges to this notion have far reaching consequences [@frijters2020happy].   

For instance, one [survey of 1,000 Americans](https://www.pnas.org/content/107/38/16489.full){target="_blank"}, conducted in 2010, concluded that money does make us happier – but only up to a certain point. The findings showed that self-reported levels of happiness increased with household wealth up to \$75,000 a year. But after that, increasing amounts of money had no further effect on happiness [@kahneman2010high]. Results such as these have reinforced the view among psychologists and economists that the relationship between wealth and measures of subjective wellbeing, such as happiness, increases linearly up to a point after which there is little gained by further increments in wealth [@clark2018four; @dolan2008we].  

However a study reported in 2013 challenged the idea that the positive effect of money plateaus [@stevenson2013subjective]. After comparing life satisfaction levels in both rich and poor countries, and rich and poor people within a country – with “rich” being defined as an income greater than $15,000 per person – Stevenson & Wolfers concluded: “The relationship between wellbeing and income … does not diminish as income rises. If there is a satiation point, we are yet to reach it.”  

Subjective wellbeing is not a unitary entity [@diener2017findings]; studies typically distinguish between _life satisfaction_, the cognitive appraisal of one's own accomplishments, and _happiness_, one's prevailing affective state or emotional mood. Money can have different effects on each. For instance, we have recently reported that positive life events, such as a major financial windfall, can have a greater impact on a person's satisfaction than their happiness [@kettlewell2020differential]. And while Kahneman observed the association between household wealth and happiness plateaud after $75,000; he also reported the association between wealth and life satisfaction was consistentily positive. This raises the possibility that contradictory associations between wealth and wellbeing previously reported may be due to differences in the construct measured. It is easy to imagine a person who is satisfied (for example because they have a good job and healthy family) but is still unhappy on a day-to-day basis. And researchers studying the relationship between wealth and wellbeing tend to focus on surveys of _life satisfaction_, which reflects only one domain of wellbeing, and ignore other aspects of wellbeing such as happiness [e.g., @stevenson2013subjective; @frijters2020happy].  

Since 2013, a distinct effect of wealth and happiness has also been challenged. [A survey of 1.7 million people representing 164 countries over 12 years (2005-2016) in the Gallup World Poll](https://www-nature-com.ezproxy2.library.usyd.edu.au/articles/s41562-017-0277-0){target="_blank"} reported money was associated with no further improvement in happiness after \$60-75,000, _nor any improvement in satisfaction_ after $95,000 [@jebb2018happiness]. Thus while the location differed, money had inflected effects with both satisfaction and happiness. On the other hand, in a nationally representative sample of 44,000 adult Americans over 44 years in the General Social Survey (GSS) happiness continued to increase with money [@twenge2020expanding], implying no inflection point existed between money and happiness.  

Both these reports provide contradictory results regarding the nature of the relationship between wealth and wellbeing, and both are based on data spanning long periods of time (12-44 years). And while such research has revealed variations due to world region, gender, and education, there has been no investigation at to whether the relationship between wealth and wellbeing has changed over time. The value of money of course changes over time with inflation, however the distribution of wealth has also changed over the last 30 years in most OECD countries (as the rich got richer faster than the poor). Such changes in the distribution of wealth are also likely to change the relationship with wellbeing when it is governed by relative wealth rather than absolute wealth.  

We used household economic panel data from Australia (HILDA) to provide the first investigation of whether changes in wealth and wellbeing have changed over the last 16 years (2002-2018). We distinguished between satisfaction and happiness as different components of subjective wellbeing, and evaluated how each varies with household wealth. Economic security is better represented by household wealth, since members of the same household share expenses as well as risks; i.e., they can provide a direct and immediate support network when financial shocks occur. The other major studies also used household after-tax income as the indicator of wealth and economic security, and so we follow the same standard here as well.  

After adjusting for age, relationship status, and education level, we find that satisfaction increases linearly with household wealth, but the same purchasing power has diminishing returns on happiness. We also discover that the location of the inflection point has increased in real dollar value since 2002.   

 
<br>

#### Wealth
We have a variety of income & wealth information in HILDA, most of it collected annually. However the household net worth is collected every four years. In the present report we use:  

1. [hwnwip](https://www.online.fbe.unimelb.edu.au/HILDAodd/KWCrossWaveCategoryDetails.aspx?varnt=hwnwip){target="_blank"} Household Net Worth ($) positive values [imputed] [weighted topcode]. Calculated as assets minus debts for the household.
2. [hifdip](https://www.online.fbe.unimelb.edu.au/HILDAodd/KWCrossWaveCategoryDetails.aspx?varnt=hifdip){target="_blank"} Household financial year disposable regular income ($) positive values [imputed] [weighted topcode]. The sum across all household members of financial year gross regular income minus taxes on financial year gross regular income.
3. [tifdip](https://www.online.fbe.unimelb.edu.au/HILDAodd/KWCrossWaveCategoryDetails.aspx?varnt=tifdip){target="_blank"} Personal disposable income ($) positive values [weighted topcode]. Financial year gross regular income minus taxes on financial year gross regular income.  

_Note_. We also examined household and personal gross wages, and household and personal total disposable income.  

Income and wealth variables were provided by the University of Melbourne as imputed, and with weighted topcodes to preserve anonymity. We removed individuals with values above the threshold indicated for top-coding.  

Household wealth variables were adjusted for the number of people in each household in the same manner as Headey, Muffels & Wooden, 2004; i.e., income divided by the square root of household size (Buhmann et al, 1988; Coulter et al, 1992).  

<br>

#### Subjective Wellbeing
There are a variety of variables related to subjective well-being collected annually, but the two we used here matched the variables we used in our previous paper:  

1. `losat` Life satisfaction - How satisfied are you with your life (0 to 10)
2. `gh9a` to `gh9i` Affective wellbeing - Sum of SF-36 item 9 (mental health and vitality)   

_Note_. We also examined `ghmh` (the 5-item subset of mental health questions, sometimes called the **MHi-5**, from the SF-36), and the patterns were almost identical to our sum of 9a to 9i.  

<br>

#### Modelling and model-selection

_Modelling_  
To determine whether wellbeing was a decreasing function of wealth or a linear function of wealth, we modelled the relationship between individual wealth and wellbeing using a simple B-spline basis function with 2 degrees of freedom (i.e., 1 y-intercept and 1 inflection point or _knot_). This was equivalent to a "piecewise" or "broken-stick" linear regression, and allowed us to determine the direction and location of the inflection point between wealth and wellbeing in each year.  

_Model selection_  
We compared the fit of the "broken-stick" model with a simpler linear model ("unbroken"), adjusted for degrees of freedom by the Bayesian Information Criterion (BIC). BIC was chosen over AIC or LRT because it will penalize our more complex model (the broken stick) more heavily. Following Raftery, differences in BIC greater than 10 were taken as "very strong evidence"; greater than 5 were "strong evidence"; greater than 2 were "positive evidence"; and less than 2 (but greater than zero) were "weak evidence" for the model with the smallest BIC ( [Raftery, 1995](https://www.sciencedirect.com/topics/pharmacology-toxicology-and-pharmaceutical-science/bayesian-information-criterion){target="_blank"}).  

For visualization purposes only, due to the large number of individual data points in each year, we grouped individuals into 10 equal-sized subgroups according to their wealth decile ($). Thus in each plot, the mean wealth and mean wellbeing score for each decile is presented, rather than each individual data point. Note that the line-of-best-fit and 95% confidence intervals from the broken stick regression model _of all individuals_ is shown in overlay.  

<br> 

#### Covariates

Covariates included in subsequent sensitivity analyses were: age, age^2^, years of education, and dummy variables for partnered, single (never married), and unemployed.  

Years of education were determined from the [Australian Qualifications Framework (AQF)](https://en.wikipedia.org/wiki/Australian_Qualifications_Framework#Certificates_I-IV){target="_blank"} from the Department of Industry.  

Separate analyses for women are included in the Supplementary material.  

<br>

### Results
***

#### Demographics and sample characteristics

After stratification by decile (personal disposable income) we see that age, proportion of males, couples, years of education and education levels all increase with decile. Conversely, proportion of females, single people (never married), unemployed, and fulltime students decreases with decile. Of particular interest is that wellbeing, household wealth and SEIFA index follow a U-shaped function with the minimum value falling between the third and sixth decile.  

<br>

#### Household disposable income (adjusted for household size)

**Key points**  

- Cognitive wellbeing (satisfaction) had a relatively linear, positive relationship wtih household disposable income. The inflection point was equally positive and negative across years, without a uniform pattern. 
- By contrast, the relationship between affective wellbeing (happiness) and disposable income tended to be a positive but decreasing ("*broken*") function with a uniform inflection point.   

<br>

### Conclusions
***

The main conclusions of this report so far are:  

- The relationship between subjective wellbeing and wealth is positive but happiness and satisfaction have different (positive) relationships. Satisfaction increases linearly with wealth while happiness increases rapidly up to a point, after which further increments in wealth produce less change - this is the **cost point** of happiness. So money buys happiness when economic security is scarce, but not so much once the cost point is reached.
- The cost point after which money provides decreasing returns in happiness is increasing over time. In 2002, the cost point of happiness represented a 13% increase over median wealth, while in 2018 it represented a 67% increase over median wealth.
- According to @frijters2020happy, coming up with a consensus estimate for income to translate into welbeing features high on the wellbeing research agenda. However evidence of a linear relationship between wellbeing and wealth calls into question the need to supplant traditional measures of economic success such as GDP with measures of wellbeing, such as "life satisfaction" surveys.  

### References
