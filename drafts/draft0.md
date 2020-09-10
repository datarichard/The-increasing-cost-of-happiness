The increasing cost of happiness
================
Dr Richard Morris
Updated: 2020-09-10

-----

## Abstract

A fundamental question for governments and people is how much happiness
does a dollar buy? The accepted view among economists and psychologists
is that money and happiness increase together up to a point, after which
there is little further gain from increasing wealth. While the location
of this *change point* has been determined, and the cost of happiness
reportedly ranges between USD$60-95K, there has been no investigation as
to whether the cost of happiness has increased or decreased over time.
We tested the relationship between money and both happiness and life
satisfaction using household economic data from Australia between
2002-2016. We discovered no change point with wealth existed for
satisfaction, but the cost of happiness has increased over those 16
years faster than inflation (i.e., cost of living). Results such as
these suggest we need to understand why the cost of happiness has
increased, and to consider whether policy-goals to improve wellbeing by
increasing wealth are feasible.

<br>

### Introduction

-----

A fundamental question for the psychology and economics of wellbeing is
just how much happiness does a dollar buy? Increasing wealth is commonly
associated with increasing happiness, however a point at which wealth no
longer increases happiness has also been [widely
observed](https://www-sciencedirect-com.ezproxy2.library.usyd.edu.au/science/article/pii/S0167487007000694)
(Clark et al., 2008; Dolan et al., 2008; Easterlin, 1974). Given that a
central goal of nations and governments is to improve wealth under the
assumption that wealth always increases wellbeing, challenges to this
notion have far reaching consequences (Frijters et al., 2020).

For instance, one [survey of 1,000
Americans](https://www.pnas.org/content/107/38/16489.full), conducted in
2010, concluded that money does make us happier – but only up to a
certain point. The findings showed that self-reported levels of
happiness increased with household wealth up to $75,000 a year. But
after that, increasing amounts of money had no further effect on
happiness (Kahneman and Deaton, 2010). Results such as these have
reinforced the view among psychologists and economists that the
relationship between wealth and measures of subjective wellbeing, such
as happiness, increases linearly up to a point after which there is
little gained by further increments in wealth (Clark, 2018; Dolan et
al., 2008).

Subjective wellbeing is not a unitary entity (Diener et al., 2017);
studies typically distinguish between *life satisfaction*, the cognitive
appraisal of one’s own accomplishments, and *happiness*, one’s
prevailing affective state or emotional mood. Money can have different
effects on each. For instance, we have recently reported that positive
life events, such as a major financial windfall, can have a greater
impact on an individual’s satisfaction than their happiness (Kettlewell
et al., 2020). However these effects were observed as a change within
individuals, and the change was transitory as both satisfaction and
happiness returned to baseline after two years (see also Frijters et
al., 2011). Across individuals, wealth also has a distinct relationship
with happiness and satisfaction as well. While Kahneman et al observed
the association between household wealth and happiness plateaud after
$75,000, they also reported the association between wealth and life
satisfaction was consistently positive. Indeed, several researchers have
observed a difference between two questions that are often used in
surveys of subjective well-being: “How satisfied are you with your
life?” and “How happy are you these days?” (Howell and Howell, 2008;
Veenhoven and Hagerty, 2006). The common conclusion is that wealth is
more strongly related to satisfaction than to happiness.

The relationship between wealth and happiness and satisfaction are
relevant for the growing popularity of using subjective well-being as a
policy guide for government and agencies. If there are distinct effects
of wealth on happiness and satisfaction then the question of which is
better suited to assess human welfare and to guide policy seems
critical. The observation that happiness plateaus with increasing wealth
while satisfaction keeps rising acquires some importance in this
context. Not everyone will agree that enhancing the happiness
experienced by those who are already quite happy is a legitimate policy
objective.

And while research since 2010 has revealed variations in the plateau or
*change point* due to [world region, gender, and
education](https://www-nature-com.ezproxy2.library.usyd.edu.au/articles/s41562-017-0277-0)
(Jebb et al., 2018), there has been no investigation as to whether the
relationship between wealth and happiness is changing over time
\[although see Fig. 4 in Clark et al. (2008); for a speculative
mechanism\]. The value of money of course changes over time with
inflation, however the distribution of wealth has also changed over the
last 30 years in many OECD countries including Australia (as the rich
got richer faster than the poor, ref needed). Such changes in the
distribution of wealth are also likely to change the relationship with
wellbeing when it is governed by relative wealth rather than absolute
wealth (Clark et al., 2008).

We used household economic panel data from Australia (HILDA) to provide
the first investigation of whether changes in wealth and wellbeing have
changed over the last 16 years (2002-2018). We distinguished between
satisfaction and happiness as different components of subjective
wellbeing, and evaluated how each varies with household wealth. Economic
security is better represented by household wealth, since members of the
same household share expenses as well as risks; i.e., they can provide a
direct and immediate support network when financial shocks occur. The
other major studies also used household after-tax income as the
indicator of wealth and economic security, and so we follow the same
standard here as well.

After adjusting for age, gender, and education level, we find that
satisfaction increases linearly with household wealth, but the same
purchasing power has diminishing returns on happiness. We also discover
that the location of the inflection point has increased in real dollar
value since 2002.

<br>

#### Wealth

We have a variety of income & wealth information in HILDA, most of it
collected annually. However the household net worth is collected every
four years. In the present report we use
[hifdip](https://www.online.fbe.unimelb.edu.au/HILDAodd/KWCrossWaveCategoryDetails.aspx?varnt=hifdip)
Household financial year disposable regular income ($). The sum across
all household members of financial year gross regular income minus taxes
on financial year gross regular income. Wealth variables were provided
by the University of Melbourne as imputed, and with weighted topcodes to
preserve anonymity. *Nb*. We also examined household and personal gross
wages and disposable income (not shown).

We removed individuals with values above the threshold indicated for
top-coding. Household wealth was adjusted for the number of people in
each household in the same manner as Headey, Muffels & Wooden, 2004;
i.e., income divided by the square root of household size (Buhmann et
al, 1988; Coulter et al, 1992).

<br>

#### Subjective Wellbeing

There are a variety of variables related to subjective well-being
collected annually, but the two we used here matched the variables we
used in our previous paper:

1.  `losat` Life satisfaction - How satisfied are you with your life (0
    to 10)
2.  `gh9a` to `gh9i` Affective wellbeing - Sum of SF-36 item 9 (mental
    health and vitality)

*Note*. We also examined `ghmh` (the 5-item subset of mental health
questions, sometimes called the **MHi-5**, from the SF-36), and the
patterns were almost identical to our sum of 9a to 9i.

<br>

#### Modelling and model-selection

*Modelling*  
To initially describe the relationship between wealth and wellbeing, we
modelled the data from each year in three different ways: 1) a simple
linear model of the form y<sub>*i*</sub> = *β*<sub>0</sub> +
*β*<sub>1</sub>*x*<sub>*i*</sub> + *ε*<sub>*i*</sub>; 2) a cubic
b-spline model of the form *B*(*x*) =
*Σβ*<sub>*i*</sub>*B*<sub>*i,n*</sub>(*x*); and 3) a piecewise
(broken-stick) model of the form y<sub>*i*</sub> = *β*<sub>0</sub> +
*β*<sub>1</sub>(*x*<sub>*i*</sub> - ω)(*x*<sub>*i*</sub> ≤ ω) +
*β*<sub>2</sub>(*x*<sub>*i*</sub> - ω)(*x*<sub>*i*</sub> ≤ ω).

These three models were chosen to represent varying degrees of
complexity. The linear model was obviously the simplest relationship
that can exist between wealth and wellbeing. The cubic B-spline was a
complex model (more degrees of freedom) and chosen because it has been
recently used to describe the maximum point at which wealth no longer
increases wellbeing (Jebb et al., 2018). Following Jebb et al, we
restricted the model to 4, 5 and 6 knots and show each instantiation
here. The piecewise model was chosen as the simplest extension of a
linear model to identify the change point at which wellbeing no longer
increases linearly with wealth.

*Change point estimation*  
To determine whether the relationship between wellbeing and wealth
contained an inflection point (“change point”), we modelled the
relationship between wealth and wellbeing across individuals using the
piecewise model described above, and estimated the posterior probability
of the changepoint location using a Bayesian approach:

Let y<sub>*i*</sub> ~ *N*(*μ*<sub>*i*</sub>,
*σ*<sup>2</sup><sub>*y*</sub>)

*μ*<sub>i</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>(*x*<sub>*i*</sub> -
ω<sub>*t*</sub>)(*x*<sub>*i*</sub> ≤ ω<sub>*t*</sub>) +
*β*<sub>2</sub>(*x*<sub>*i*</sub> - ω<sub>*t*</sub>)(*x*<sub>*i*</sub>
\> ω<sub>*t*</sub>)                              (1)

Where *x*<sub>i</sub> and *y*<sub>i</sub> was an individuals’ household
wealth ($) and wellbeing, respectively; and ω<sub>*t*</sub> was the
changepoint in $ for each year (*t*).

The location of the changepoint was sampled randomly from a uniform
prior over the resticted range of wealth values, and the posterior
probability of the location for each year is presented.

<br>

*Model selection*  
To determine whether wellbeing was a linear or non-linear (e.g.,
piecewise) function of wealth, we compared the linear and piecewise
model fits using the [Widely Applicable Information Criterion
(WAIC)](https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/overfitting-regularization-and-information-criteria.html#the-problem-with-parameters).
The WAIC is the log-posterior predictive density plus a penalty
proportional to the variance in the posterior distribution. Thus it
provides an approximation of the out-of-sample deviance that converges
to the cross-validation approximation in a large sample, with a penalty
for the effective number of parameters (degrees of freedom). For this
reason is it useful to compare two models of varying complexity.

WAIC was defined as:

WAIC = -2(lppd - *p*<sub>WAIC</sub>)

Where lppd (log pointwise predictive density) is the total across
observations of the log of the average likelihood of each observation,
and *p*<sub>WAIC</sub> is the effective number of free parameters
determined by the sum of the variance in log-likelihood for each
observation (*i*).

For visualization purposes only, due to the large number of individual
data points in each year, we grouped individuals into 10 equal-sized
subgroups according to their wealth decile ($). Thus in each plot, the
mean wealth and mean wellbeing score for each decile is presented,
rather than each individual data point. Note that the line-of-best-fit
and 95% confidence intervals from the broken stick regression model *of
all individuals* is shown in overlay.

<br>

#### Covariates

Age (and age<sup>2</sup>), gender, and education were included as
covariates, and students were removed.

<br>

### Results

-----

#### Household wealth on satisfaction (cognitive wellbeing)

![](../figures/losat_hwnwip-1.png)<!-- -->

<br>

**Key points**

  - Generally the relationship between satisfaction and household wealth
    appeared linear
  - The linear relationship between satisfaction and wealth became
    weaker (less steep) over years
  - There is no satiety point (horizontal inflection) in satisfaction
    before the highest decile in any year
  - Any inflection point in satisfaction was as likely to be convex as
    concave

<br>

#### Household wealth on happiness (affective wellbeing)

![](../figures/gh9_hwnwip-1.png)<!-- -->

<br>

**Key points**

  - The linear relationship between happiness and wealth became weaker
    (less steep) over years
  - There was no happiness satiety point (horizontal inflection) before
    the final decile in any year
  - A concave inflection point with happiness occurred in each year, and
    appeared to shift right with time.

<br>

##### Bayesian change point results

Shaded areas to the right of the vertical dotted line are credibly (95%)
larger than the base year (2002).

![](../figures/happy_change_time-1.png)<!-- -->

<br>

**Key points**

  - The change point for happiness (affective wellbeing) shifts to the
    right over time and occurs at greater household wealth levels.

<br>

#### Change points over time

The mean change point location has moved further right (higher, faster)
than the median household wealth over time:

<table>

<caption>

Household wealth median, change point and percentile

</caption>

<thead>

<tr>

<th style="text-align:left;">

year

</th>

<th style="text-align:right;">

median ($000s)

</th>

<th style="text-align:right;">

change point ($000s)

</th>

<th style="text-align:right;">

%

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

fy2002

</td>

<td style="text-align:right;">

26.86

</td>

<td style="text-align:right;">

32.37

</td>

<td style="text-align:right;">

0.37

</td>

</tr>

<tr>

<td style="text-align:left;">

fy2006

</td>

<td style="text-align:right;">

38.07

</td>

<td style="text-align:right;">

46.08

</td>

<td style="text-align:right;">

0.36

</td>

</tr>

<tr>

<td style="text-align:left;">

fy2010

</td>

<td style="text-align:right;">

53.72

</td>

<td style="text-align:right;">

52.50

</td>

<td style="text-align:right;">

0.52

</td>

</tr>

<tr>

<td style="text-align:left;">

fy2014

</td>

<td style="text-align:right;">

66.62

</td>

<td style="text-align:right;">

85.70

</td>

<td style="text-align:right;">

0.32

</td>

</tr>

<tr>

<td style="text-align:left;">

fy2018

</td>

<td style="text-align:right;">

74.54

</td>

<td style="text-align:right;">

124.48

</td>

<td style="text-align:right;">

0.17

</td>

</tr>

</tbody>

</table>

<br><br>

### Conclusions

-----

The main conclusions of this report so far are:

  - The relationship between subjective wellbeing and wealth is positive
    but happiness and satisfaction have different (positive)
    relationships. Satisfaction increases linearly with wealth while
    happiness increases rapidly up to a point, after which further
    increments in wealth produce less change - this is the **cost
    point** of happiness. So money buys happiness when economic security
    is scarce, but not so much once the cost point is reached.
  - The cost point after which money provides decreasing returns in
    happiness is increasing over time. In 2002, the cost point of
    happiness represented a 13% increase over median wealth, while in
    2018 it represented a 67% increase over median wealth.
  - According to Frijters et al. (2020), coming up with a consensus
    estimate for income to translate into welbeing features high on the
    wellbeing research agenda. However evidence of a linear relationship
    between wellbeing and wealth calls into question the need to
    supplant traditional measures of economic success such as GDP with
    measures of wellbeing, such as “life satisfaction” surveys.

<br>

A study reported in 2013 challenged the idea that the positive effect of
money plateaus (Stevenson and Wolfers, 2013). After comparing life
satisfaction levels in both rich and poor countries, and rich and poor
people within a country – with “rich” being defined as an income greater
than $15,000 per person – Stevenson & Wolfers concluded: “The
relationship between wellbeing and income … does not diminish as income
rises. If there is a satiation point, we are yet to reach it.”

Since 2013, the distinct effect of wealth and happiness has also been
challenged. [A survey of 1.7 million people representing 164 countries
over 12 years (2005-2016) in the Gallup World
Poll](https://www-nature-com.ezproxy2.library.usyd.edu.au/articles/s41562-017-0277-0)
reported money was associated with no further improvement in happiness
after $60-75,000, *nor any improvement in satisfaction* after $95,000
(Jebb et al., 2018). Thus while the location differed, money had
inflected effects with both satisfaction and happiness. On the other
hand, in a nationally representative sample of 44,000 adult Americans
over 44 years in the General Social Survey (GSS) happiness continued to
increase with money (Twenge and Cooper, 2020), implying no inflection
point existed between money and happiness.

Both these reports provide contradictory results regarding the nature of
the relationship between wealth and wellbeing, and both are based on
data spanning long periods of time (12-44 years). Perhaps the different
results are due to differences in the constructs being measured, or
perhaps it is due to different effects of time between studies.

### References

<div id="refs" class="references">

<div id="ref-clark2018four">

Clark, A.E., 2018. Four decades of the economics of happiness: Where
next? Review of Income and Wealth 64, 245–269.

</div>

<div id="ref-clark2008relative">

Clark, A.E., Frijters, P., Shields, M.A., 2008. Relative income,
happiness, and utility: An explanation for the easterlin paradox and
other puzzles. Journal of Economic literature 46, 95–144.

</div>

<div id="ref-diener2017findings">

Diener, E., Heintzelman, S.J., Kushlev, K., Tay, L., Wirtz, D., Lutes,
L.D., Oishi, S., 2017. Findings all psychologists should know from the
new science on subjective well-being. Canadian Psychology/psychologie
canadienne 58, 87.

</div>

<div id="ref-dolan2008we">

Dolan, P., Peasgood, T., White, M., 2008. Do we really know what makes
us happy? A review of the economic literature on the factors associated
with subjective well-being. Journal of economic psychology 29, 94–122.

</div>

<div id="ref-easterlin1974growth">

Easterlin, R.A., 1974. Does economic growth improve the human lot? Some
empirical evidence, in: Nations and Households in Economic Growth.
Elsevier, pp. 89–125.

</div>

<div id="ref-frijters2020happy">

Frijters, P., Clark, A.E., Krekel, C., Layard, R., 2020. A happy choice:
Wellbeing as the goal of government. Behavioural Public Policy 4,
126–165.

</div>

<div id="ref-frijters2011life">

Frijters, P., Johnston, D.W., Shields, M.A., 2011. Life satisfaction
dynamics with quarterly life event data. Scandinavian Journal of
Economics 113, 190–211.

</div>

<div id="ref-howell2008relation">

Howell, R.T., Howell, C.J., 2008. The relation of economic status to
subjective well-being in developing countries: A meta-analysis.
Psychological bulletin 134, 536.

</div>

<div id="ref-jebb2018happiness">

Jebb, A.T., Tay, L., Diener, E., Oishi, S., 2018. Happiness, income
satiation and turning points around the world. Nature Human Behaviour 2,
33–38.

</div>

<div id="ref-kahneman2010high">

Kahneman, D., Deaton, A., 2010. High income improves evaluation of life
but not emotional well-being. Proceedings of the national academy of
sciences 107, 16489–16493.

</div>

<div id="ref-kettlewell2020differential">

Kettlewell, N., Morris, R.W., Ho, N., Cobb-Clark, D.A., Cripps, S.,
Glozier, N., 2020. The differential impact of major life events on
cognitive and affective wellbeing. SSM-population health 10, 100533.

</div>

<div id="ref-stevenson2013subjective">

Stevenson, B., Wolfers, J., 2013. Subjective well-being and income: Is
there any evidence of satiation? American Economic Review 103, 598–604.

</div>

<div id="ref-twenge2020expanding">

Twenge, J.M., Cooper, A.B., 2020. The expanding class divide in
happiness in the united states, 1972–2016. Emotion.

</div>

<div id="ref-veenhoven2006rising">

Veenhoven, R., Hagerty, M., 2006. Rising happiness in nations 1946–2004:
A reply to easterlin. Social indicators research 79, 421–436.

</div>

</div>
