# Data used in this book

Two published data sets carry every worked example. Both are redistributed from
the **{metamisc}** R package (GPL-3), which assembled them from the original
reviews. The chapters cite the original papers, not the package.

## `her2-endometrial.csv`

Twenty studies of HER2 as a prognostic factor in endometrial cancer, with the
hazard ratio and its 95% confidence interval for overall or progression-free
survival.

Source: Zhang Y, Zhao D, Gong C, Zhang F, He J, Zhang W, et al. Prognostic role
of hormone receptors in endometrial cancer: a systematic review and
meta-analysis. World J Surg Oncol. 2015;13:208.

| Column | Meaning |
|---|---|
| `Study`, `PrimaryAuthor`, `year`, `Country` | study identification |
| `Disease` | EC, EEC or UPSC, the histology studied |
| `N` | participants |
| `HR`, `HR.025`, `HR.975` | hazard ratio and its 95% confidence interval |
| `outcome` | OS (overall survival) or PFS (progression-free survival) |

## `euroscore2.csv`

The EuroSCORE II development study and 22 external validations, with
discrimination and calibration for 30-day mortality after cardiac surgery.

Sources: Guida P, Mastro F, Scrascia G, Whitlock R, Paparella D. Performance of
the European System for Cardiac Operative Risk Evaluation II: a meta-analysis
of 22 studies involving 145,592 cardiac surgery procedures. J Thorac Cardiovasc
Surg. 2014;148(6):3049-3057.e1. Nashef SAM, Roques F, Sharples LD, Nilsson J,
Smith C, Goldstone AR, et al. EuroSCORE II. Eur J Cardiothorac Surg.
2012;41(4):734-744.

| Column | Meaning |
|---|---|
| `Study` | first author |
| `n`, `n.events` | participants and observed deaths |
| `c.index`, `se.c.index`, `c.index.95CIl`, `c.index.95CIu` | discrimination |
| `Po` | observed proportion dying within 30 days |
| `Pe`, `SD.Pe`, `e.events` | predicted risk: mean, standard deviation, expected deaths |
| `multicentre`, `mean.age`, `sd.age`, `pts.before.2010` | study characteristics |

The same table serves two of the three review types: `Po` alone answers an
overall prognosis question, while `c.index` and `Po` against `Pe` answer a
prediction model question.
