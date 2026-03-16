library(epiR)
library(dplyr)

arr <- array(c(13, 20, 23, 22, 31, 16, 19, 21),
  dim = c(2, 2, 2),
  dimnames = list(
    Grupo      = c("Exposicion(+)", "Exposicion(-)"),
    Enfermedad = c("Si", "No"),
    Centro     = c("Estrato1", "Estrato2")
  ))
res3 <- epi.2by2(dat = as.table(arr), method = "cross.sectional", digits = 2,
                 conf.level = 0.95, units = 100, interpret = FALSE, outcome = "as.columns")

cat("PR.crude.miettinen columns:\n")
print(names(res3$massoc.detail$PR.crude.miettinen))
cat("PR.crude.miettinen:\n")
print(res3$massoc.detail$PR.crude.miettinen)

cat("\nOR.mh.wald columns:\n")
print(names(res3$massoc.detail$OR.mh.wald))
cat("OR.mh.wald:\n")
print(res3$massoc.detail$OR.mh.wald)

cat("\nAFRisk.strata.wald columns:\n")
print(names(res3$massoc.detail$AFRisk.strata.wald))
print(res3$massoc.detail$AFRisk.strata.wald)

cat("\nPAFRisk.strata.wald columns:\n")
print(names(res3$massoc.detail$PAFRisk.strata.wald))
print(res3$massoc.detail$PAFRisk.strata.wald)
