# Writes every figure the README and the tutorial sheets show.
#
#   Rscript tutorial/figures.R
#
# The data is metamisc's own, so a reader can paste any block of the README
# into a fresh session and get the picture that is printed beside it. Nothing
# here depends on a spreadsheet only the author has.

library(easyPR)

out <- file.path("man", "figures")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

data(Zhang, package = "metamisc")
data(EuroSCORE, package = "metamisc")

# A prognostic factor. Zhang holds hazard ratios for one marker, with the
# interval rather than the standard error, which is how most papers leave it.
easypr.object.hr <- easypr(data = Zhang, method.tau = "REML", sm = "HR",
                 method = "Inverse")
# label.e and label.c name the groups: they head the columns beside the
# study label and they name the two sides of the axis. direction = "bad"
# because death is the outcome, so a ratio above one favours the other group.
easypr_forest(easypr.object.hr, result = FALSE,
              label.e = "High expression", label.c = "Low expression",
              direction = "bad", xlab = "Overall survival",
              file.name = "fig-forest-hr", path = out)

# A whole model, validated 23 times. Discrimination first.
easypr.object.cstat <- easypr(EuroSCORE, sm = "cstat")
easypr_forest(easypr.object.cstat, result = FALSE, file.name = "fig-forest-cstat",
              path = out)

# Then calibration, which is the half a review most often leaves out.
easypr.object.oe <- easypr(EuroSCORE, sm = "OE")
easypr_forest(easypr.object.oe, result = FALSE, file.name = "fig-forest-oe",
              path = out)

# Is one validation carrying the pooled discrimination?
easypr_loo(easypr.object.cstat, result = FALSE, file.name = "fig-loo", path = out)

# Does where it was validated explain the spread?
easypr_metareg(easypr.object.cstat, "mean.age", result = FALSE, file.name = "fig-bubble",
               path = out)

# And are the small validations different from the large ones?
easypr_funnel(easypr.object.cstat, result = FALSE, file.name = "fig-funnel", path = out)

cat("\nfigures written to ", out, "\n", sep = "")
