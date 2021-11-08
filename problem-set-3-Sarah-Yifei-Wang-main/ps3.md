Experiments and Causality: Problem Set 3
================
Alex, Micah and Scott
12/7/2020

``` r
library(data.table)

library(sandwich)
library(lmtest)

library(ggplot2)
library(patchwork)

library(foreign)
```

# 1. Peruvian Recycling

Look at [this article](./readings/recycling_peru.pdf) about encouraging
recycling in Peru. The paper contains two experiments, a “participation
study” and a “participation intensity study.” In this problem, we will
focus on the latter study, whose results are contained in Table 4 in
this problem. You will need to read the relevant section of the paper
(starting on page 20 of the manuscript) in order to understand the
experimental design and variables. (*Note that “indicator variable” is a
synonym for “dummy variable,” in case you haven’t seen this language
before.*)

1.  In Column 3 of Table 4A, what is the estimated ATE of providing a
    recycling bin on the average weight of recyclables turned in per
    household per week, during the six-week treatment period? Provide a
    95% confidence interval.

2.  In Column 3 of Table 4A, what is the estimated ATE of sending a text
    message reminder on the average weight of recyclables turned in per
    household per week? Provide a 95% confidence interval.

3.  Which outcome measures in Table 4A show statistically significant
    effects (at the 5% level) of providing a recycling bin?

4.  Which outcome measures in Table 4A show statistically significant
    effects (at the 5% level) of sending text messages?

5.  Suppose that, during the two weeks before treatment, household A
    turns in 2kg per week more recyclables than household B does, and
    suppose that both households are otherwise identical (including
    being in the same treatment group). From the model, how much more
    recycling do we predict household A to have than household B, per
    week, during the six weeks of treatment? Provide only a point
    estimate, as the confidence interval would be a bit complicated.
    This question is designed to test your understanding of slope
    coefficients in regression.

6.  Suppose that the variable “percentage of visits turned in bag,
    baseline” had been left out of the regression reported in Column 1.
    What would you expect to happen to the results on providing a
    recycling bin? Would you expect an increase or decrease in the
    estimated ATE? Would you expect an increase or decrease in the
    standard error? Explain our reasoning.

7.  In column 1 of Table 4A, would you say the variable “has cell phone”
    is a bad control? Explain your reasoning.

8.  If we were to remove the “has cell phone” variable from the
    regression, what would you expect to happen to the coefficient on
    “Any SMS message”? Would it go up or down? Explain your reasoning.

# 2. Multifactor Experiments

Staying with the same experiment, now think about multifactor
experiments.

1.  What is the full experimental design for this experiment? Tell us
    the dimensions, such as 2x2x3. The full results appear in Panel 4B.
    We’ll note that the dimensions of an experiment are defined in terms
    of the *treatments that the experiment assigns*, not in terms of
    other features about the data.

1.  In the results of Table 4B, describe the baseline category. That is,
    in English, how would you describe the attributes of the group of
    people for whom all dummy variables are equal to zero?

1.  In column (1) of Table 4B, interpret the magnitude of the
    coefficient on “bin without sticker.” What does it mean?

1.  In column (1) of Table 4B, which seems to have a stronger treatment
    effect, the recycling bin with message sticker, or the recycling bin
    without sticker? How large is the magnitude of the estimated
    difference?

1.  Is this difference you just described statistically significant?
    Explain which piece of information in the table allows you to answer
    this question.

1.  Notice that Table 4C is described as results from “fully saturated”
    models. What does this mean? Looking at the list of variables in the
    table, explain in what sense the model is “saturated.”

# 3. Now! Do it with data

Download the data set for the recycling study in the previous problem,
obtained from the authors. We’ll be focusing on the outcome variable
Y=“number of bins turned in per week” (avg\_bins\_treat).

``` r
d <- foreign::read.dta("./data/karlan_data_subset_for_class.dta")
d <- data.table(d)
head(d)
```

    ##    street havecell avg_bins_treat base_avg_bins_treat bin sms bin_s bin_g sms_p
    ## 1:      7        1      1.0416666               0.750   1   1     1     0     0
    ## 2:      7        1      0.0000000               0.000   0   1     0     0     1
    ## 3:      7        1      0.7500000               0.500   0   0     0     0     0
    ## 4:      7        1      0.5416667               0.500   0   0     0     0     0
    ## 5:      6        1      0.9583333               0.375   1   0     0     1     0
    ## 6:      8        0      0.2083333               0.000   1   0     0     1     0
    ##    sms_g
    ## 1:     1
    ## 2:     0
    ## 3:     0
    ## 4:     0
    ## 5:     0
    ## 6:     0

``` r
## Do some quick exploratory data analysis with this data. 
## There are some values in this data that seem a bit strange. 

## Determine what these are. 
## Don't make an assessment about keeping, changing, or 
## dropping just yet, but at any point that your analysis touches 
## these variables, you'll have to determine what is appropriate 
## given the analysis you are conducting. 
```

1.  For simplicity, let’s start by measuring the effect of providing a
    recycling bin, ignoring the SMS message treatment (and ignoring
    whether there was a sticker on the bin or not). Run a regression of
    Y on only the bin treatment dummy, so you estimate a simple
    difference in means. Provide a 95% confidence interval for the
    treatment effect, using **of course** robust standard errors (use
    these throughout).

``` r
mod_1 <- 'fill this in'
```

1.  Now add the pre-treatment value of Y as a covariate. Provide a 95%
    confidence interval for the treatment effect. Explain how and why
    this confidence interval differs from the previous one.

``` r
mod_2 <- 'fill this in'
```

1.  Now add the street fixed effects. (You’ll need to use the R command
    factor().) Provide a 95% confidence interval for the treatment
    effect.

``` r
mod_3 <- 'fill this in'
```

1.  Recall that the authors described their experiment as “stratified at
    the street level,” which is a synonym for blocking by street. Does
    including these block fixed effects change the standard errors of
    the estimates *very much*? Conduct the appropriate test for the
    inclusion of these block fixed effects, and interpret them in the
    context of the other variables in the regression.

``` r
mod_4 <- 'fill this in'
```

``` r
test_fixed_effects <- 'fill this in'
```

1.  Perhaps having a cell phone helps explain the level of recycling
    behavior. Instead of “has cell phone,” we find it easier to
    interpret the coefficient if we define the variable " no cell
    phone." Give the R command to define this new variable, which equals
    one minus the “has cell phone” variable in the authors’ data set.
    Use “no cell phone” instead of “has cell phone” in subsequent
    regressions with this dataset.

2.  Now add “no cell phone” as a covariate to the previous regression.
    Provide a 95% confidence interval for the treatment effect. Explain
    why this confidence interval does not differ much from the previous
    one.

``` r
mod_5 <- 'fill this in'
```

1.  Now let’s add in the SMS treatment. Re-run the previous regression
    with “any SMS” included. You should get the same results as in Table
    4A. Provide a 95% confidence interval for the treatment effect of
    the recycling bin. Explain why this confidence interval does not
    differ much from the previous one.

``` r
mod_6 <- 'fill this in'
```

1.  Now reproduce the results of column 2 in Table 4B, estimating
    separate treatment effects for the two types of SMS treatments and
    the two types of recycling-bin treatments. Provide a 95% confidence
    interval for the effect of the unadorned recycling bin. Explain how
    your answer differs from that in part (g), and explain why you think
    it differs.

``` r
mod_7 <- 'fill this in'
```

# 4. A Final Practice Problem

Now for a fictional scenario. An emergency two-week randomized
controlled trial of the experimental drug ZMapp is conducted to treat
Ebola. (The control represents the usual standard of care for patients
identified with Ebola, while the treatment is the usual standard of care
plus the drug.)

Here are the (fake) data.

``` r
d <- fread("./data/Ebola_rct2.csv")
head(d)
```

    ##    temperature_day0 dehydrated_day0 treat_zmapp temperature_day14
    ## 1:         99.53168               1           0          98.62634
    ## 2:         97.37372               0           0          98.03251
    ## 3:         97.00747               0           1          97.93340
    ## 4:         99.74761               1           0          98.40457
    ## 5:         99.57559               1           1          99.31678
    ## 6:         98.28889               1           1          99.82623
    ##    dehydrated_day14 male
    ## 1:                1    0
    ## 2:                1    0
    ## 3:                0    1
    ## 4:                1    0
    ## 5:                1    0
    ## 6:                1    1

You are asked to analyze it. Patients’ temperature and whether they are
dehydrated is recorded on day 0 of the experiment, then ZMapp is
administered to patients in the treatment group on day 1. Dehydration
and temperature is again recorded on day 14.

1.  Without using any covariates, answer this question with regression:
    What is the estimated effect of ZMapp (with standard error in
    parentheses) on whether someone was dehydrated on day 14? What is
    the p-value associated with this estimate?

``` r
zmapp_1 <- 'fill this in'
```

1.  Add covariates for dehydration on day 0 and patient temperature on
    day 0 to the regression from part (a) and report the ATE (with
    standard error). Also report the p-value.

``` r
zmapp_2 <- 'fill this in'
```

1.  Do you prefer the estimate of the ATE reported in part (a) or part
    (b)? Why? Report the results of the F-test that you used to form
    this opinion.

``` r
zmapp_test_object <- 'fill this in'
```

1.  The regression from part (2) suggests that temperature is highly
    predictive of dehydration. Add, temperature on day 14 as a covariate
    and report the ATE, the standard error, and the p-value.

``` r
zmapp_3 <- 'fill this in'
```

1.  Do you prefer the estimate of the ATE reported in part (b) or part
    (d)? What is this preference based on?

1.  Now let’s switch from the outcome of dehydration to the outcome of
    temperature, and use the same regression covariates as in the chunk
    titled `add pre-treatment measures`. Test the hypothesis that ZMapp
    is especially likely to reduce mens’ temperatures, as compared to
    womens’, and describe how you did so. What do the results suggest?

``` r
zmapp_4 <- 'fill this in'
```

1.  Which group – those that are coded as `male == 0` or `male == 1`
    have better health outcomes in control? What about in treatment? How
    does this help to contextualize whatever heterogeneous treatment
    effect you might have estimated?

1.  Suppose that you had not run the regression in part (7). Instead,
    you speak with a colleague to learn about heterogeneous treatment
    effects. This colleague has access to a non-anonymized version of
    the same dataset and reports that they looked at heterogeneous
    effects of the ZMapp treatment by each of 80 different covariates to
    examine whether each predicted the effectiveness of ZMapp on each of
    20 different indicators of health. Across these regressions your
    colleague ran, the treatment’s interaction with gender on the
    outcome of temperature is the only heterogeneous treatment effect
    that he found to be statistically significant. They reason that this
    shows the importance of gender for understanding the effectiveness
    of the drug, because nothing else seemed to indicate why it worked.
    Bolstering your colleague’s confidence, after looking at the data,
    they also returned to his medical textbooks and built a theory about
    why ZMapp interacts with processes only present in men to cure.
    Another doctor, unfamiliar with the data, hears your colleague’s
    theory and finds it plausible. How likely do you think it is ZMapp
    works especially well for curing Ebola in men, and why? (This
    question is conceptual can be answered without performing any
    computation.)

1.  Now, imagine that your colleague’s fishing expedition did not
    happen, but that you had tested this heterogeneous treatment effect,
    and only this heterogeneous treatment effect, of your own accord.
    Would you be more or less inclined to believe that the heterogeneous
    treatment effect really exists? Why?

1.  Now, imagine that your colleague’s fishing expedition **did**
    happen, but that you on your own tested this and only this HTE,
    discover a positive result and conclude there is an effect. How does
    your colleague’s behavior change the interpretation of your test?
    Does this seem fair or reasonable?
