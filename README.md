# easyPR

**Prognostic meta-analysis in one call, argued the way the `meta` package
argues. The book that teaches it is free.**

Sponsored by [Meta-Analysis Academy](https://metaanalysisacademy.com).

The book in this repository teaches prognostic meta-analysis end to end, and
it is free to read: **[open the book](https://vanioljantunes.github.io/easyPR/)**.
Its source is in [`book/`](book) as Quarto files. The R package that the book
teaches is in [`R/`](R), in the same repository, so the text and the code
cannot drift apart.

## Contents

| Section | What it covers |
|:---|:---|
| **[1 . Install it, and what it does](#1-install-it-and-what-it-does)** | The one entry point, the four functions that take what it returns, and the two measures |
| **[2 . Prepare the data, then fit it](#2-prepare-the-data-then-fit-it)** | One row per validation, whatever each paper reported, then `easypr()` |
| **[3 . The forest, and the interval that matters](#3-the-forest-and-the-interval-that-matters)** | `easypr_forest()`, and why the prediction interval is the one to quote |
| **[4 . Leave one out](#4-leave-one-out)** | Whether one validation is carrying the pooled result |
| **[5 . Meta-regression](#5-meta-regression)** | `easypr_metareg()`, and what a study-level covariate can and cannot tell you |
| **[6 . Small-study effects](#6-small-study-effects)** | `easypr_funnel()`, and why asymmetry is read differently here |

<sub>[References](#references) . [Licence](#licence)</sub>

---

## 1. Install it, and what it does

A **prognostic meta-analysis** does not pool a treatment effect. It pools how
well a model performs when somebody else runs it on their own patients, and
performance has two halves that can disagree.

| Measure | Question | `measure =` |
|:---|:---|:---|
| c-statistic | Does the model rank patients correctly? | `"cstat"` |
| O:E ratio | Are the risks it quotes the risks that happen? | `"OE"` |
| hazard, odds or risk ratio | How much does one factor shift risk? | `"ratio"` |

A model can rank well and still be badly calibrated. A review that reports
only the first has quietly left out whether the numbers mean anything, so
`easypr()` makes you name which one you are pooling.

The third measure is the other half of the field. Not every prognostic
review validates a whole model; many pool what a single factor does to risk.
That is a ratio, pooled on the log scale and read on the ratio scale, and
easyPR does the transforming so the printed table and the plot axis both
come back in the units you reported.

```r
easypr(cohorts, measure = "ratio")   # reads an HR, OR or RR column
#> Pooled hazard ratio across 9 studies (REML)
#>   estimate            1.637
#>   95% confidence     1.428 to 1.876
#>   95% prediction     1.280 to 2.094
#>   tau-squared         0.011  (log scale)
```

Estimation is delegated to **{metamisc}** (Debray and de Jong), which easyPR
calls and never second-guesses.

```r
# install.packages("remotes")
remotes::install_github("vanioljantunes/easyPR")

library(easyPR)
```

## 2. Prepare the data, then fit it

One row per validation. What each row needs is whatever that paper happened
to report: the c-statistic with a confidence interval, or with a standard
error, or neither, leaving the sample size and the events to work back from.

You do not have to rename your columns first. easyPR recognises the common
spellings and says what it decided they meant.

```r
data(EuroSCORE, package = "metamisc")

fit <- easypr(EuroSCORE, measure = "cstat")
#> easypr: read c.index as cstat, se.c.index as cstat.se,
#>         c.index.95CIl as cstat.cilb, n as N, n.events as O.
fit
```

```
Pooled c-statistic across 23 validations (REML)

  estimate            0.789
  95% confidence     0.765 to 0.811
  95% prediction     0.679 to 0.868
  tau-squared         0.070
  I-squared           87.3%
```

`summary(fit)` adds the table of validations, with each one's weight.

## 3. The forest, and the interval that matters

```r
easypr_forest(fit)
```

The confidence interval is about the **average** performance across the
validations that happened. The prediction interval is what the **next**
hospital should expect, and it is the one a reader of a prognostic review
actually needs.

On EuroSCORE those two are 0.765 to 0.811 and 0.679 to 0.868. The second is
roughly three times the width of the first, which is the honest summary: the
model's discrimination is not settled, and where it is validated next matters
about as much as the model does.

## 4. Leave one out

```r
easypr_loo(fit)
```

The pool refitted with each validation removed in turn. A pooled performance
that moves when one study leaves is being carried by that study, and the
reader is entitled to know which one.

## 5. Meta-regression

```r
easypr_metareg(fit, "mean.age")
```

Heterogeneity here is usually the interesting part rather than a nuisance.
The model is being validated in different places on different patients, and
the question is whether that explains the spread.

The bubble plot is the honest way to show it, because you can see how few
studies are carrying each end of the line. The covariate is a property of the
study, so a slope says that studies differ. It does not say that moving a
patient along that covariate would move their risk.

## 6. Small-study effects

```r
easypr_funnel(fit)
```

Asymmetry is read with more care here than in a treatment review. Small
validations are not only more variable, they are often done in a different
kind of centre, so a gap on one side can be case mix rather than suppression.
easyPR reports the test and prints the caveat with it, and says so plainly
when there are too few studies for the test to carry weight.

## References

Debray TPA, de Jong VMT. *metamisc: Meta-Analysis of Diagnosis and Prognosis
Research Studies*. R package.

Debray TPA, Damen JAAG, Snell KIE, et al. A guide to systematic review and
meta-analysis of prediction model performance. *BMJ* 2017;356:i6460.

Riley RD, Ensor J, Snell KIE, et al. External validation of clinical
prediction models using big datasets. *BMJ* 2016;353:i3140.

## Licence

GPL-2 for the package code. CC BY-NC-SA 4.0 for the book text, see
`LICENSE-text.md`.

## Related

Part of the MetaRank 4 catalog from the [Meta-Analysis
Academy](https://metahub.metaanalysisacademy.com): one design, taught end to
end, per repo. The book is free. The video course, templates and support are
the paid half.

- [easyDR](https://github.com/vanioljantunes/easyDR), dose-response
- [easyGWA](https://github.com/vanioljantunes/easyGWA), genetic
