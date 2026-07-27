# CKD-EPI 2021 race-free creatinine equation, used in cohort derivation.
ckdepi_2021 <- function(scr, age, sex_female) {
  sex_female <- as.logical(sex_female)
  out <- ifelse(
    is.na(scr) | is.na(age),
    NA_real_,
    {
      k <- ifelse(sex_female, 0.7, 0.9)
      a <- ifelse(sex_female, -0.241, -0.302)
      s <- pmin(scr / k, 1) ^ a
      L <- pmax(scr / k, 1) ^ (-1.200)
      142 * s * L * (0.9938 ^ age) * ifelse(sex_female, 1.012, 1.0)
    }
  )
  as.numeric(out)
}
