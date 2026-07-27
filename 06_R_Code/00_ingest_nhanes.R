# Build 07_R_Data/raw_domains.rds from the NHANES public-use .xpt files.
# Run from the submission root. Reads 08_Raw_data_NHANES_1999_2018/, writes 07_R_Data/.
# Domains are stacked across cycles; each row keeps the file it came from.
suppressWarnings(suppressMessages({library(dplyr); library(haven)}))

RAW <- "08_Raw_data_NHANES_1999_2018"
OUT <- "07_R_Data/raw_domains.rds"

## Cycle letter -> years. 1999-2000 files carry no letter, hence the empty name.
cycle_years <- c("1999-2000", "2001-2002", "2003-2004", "2005-2006", "2007-2008",
                 "2009-2010", "2011-2012", "2013-2014", "2015-2016", "2017-2018")
names(cycle_years) <- c("", "B", "C", "D", "E", "F", "G", "H", "I", "J")

## File stems per domain, in the order NHANES released them.
## Names ending _legacy are the 1999-2004 files that were renamed from 2005 on.
domains <- list(
  demo   = c("DEMO", paste0("DEMO_", LETTERS[2:10])),
  bpx    = c("BPX",  paste0("BPX_",  LETTERS[2:10])),
  bpq    = c("BPQ",  paste0("BPQ_",  LETTERS[2:10])),
  bmx    = c("BMX",  paste0("BMX_",  LETTERS[2:10])),
  smq    = c("SMQ",  paste0("SMQ_",  LETTERS[2:10])),
  diq    = c("DIQ",  paste0("DIQ_",  LETTERS[2:10])),
  alb_cr = paste0("ALB_CR_", LETTERS[4:10]),
  biopro = paste0("BIOPRO_", LETTERS[4:10]),
  ghb    = paste0("GHB_",    LETTERS[4:10]),
  mcq    = c("MCQ",  paste0("MCQ_",  LETTERS[2:10])),
  hdl    = paste0("HDL_",    LETTERS[4:10]),
  tchol  = paste0("TCHOL_",  LETTERS[4:10]),
  trigly = paste0("TRIGLY_", LETTERS[4:10]),
  glu    = paste0("GLU_",    LETTERS[4:10]),
  cbc    = paste0("CBC_",    LETTERS[4:10]),
  crp    = paste0("CRP_",    LETTERS[4:6]),
  hscrp  = paste0("HSCRP_",  LETTERS[9:10]),
  rxq    = c("RXQ_RX", paste0("RXQ_RX_", LETTERS[2:10])),
  # Legacy files stack 2001-2004 first, then the 1999-2000 LAB* file.
  urine_alb_legacy = c("L16_B",   "L16_C",   "LAB16"),
  scr_legacy       = c("L40_B",   "L40_C",   "LAB18"),
  hba1c_legacy     = c("L10_B",   "L10_C",   "LAB10"),
  glu_legacy       = c("L10AM_B", "L10AM_C", "LAB10AM"),
  cbc_legacy       = c("L25_B",   "L25_C",   "LAB25"),
  lipid_legacy_tc  = c("L13_B",   "L13_C",   "LAB13"),
  lipid_legacy_tg  = c("L13AM_B", "L13AM_C", "LAB13AM")
)

## A trailing "_X" is the cycle letter; anything else is the 1999-2000 file.
suffix_of <- function(stem) {
  last <- sub(".*_", "", stem)
  if (grepl("_", stem) && nchar(last) == 1L && last %in% names(cycle_years)) last else ""
}

read_one <- function(stem) {
  path <- file.path(RAW, paste0(stem, ".xpt"))
  if (!file.exists(path)) stop("Missing raw file: ", path)
  s <- suffix_of(stem)
  d <- haven::read_xpt(path)
  d <- haven::zap_label(haven::zap_labels(haven::zap_formats(d)))
  d$cycle_suffix <- s
  d$cycle_years  <- unname(cycle_years[match(s, names(cycle_years))])
  d$source_file  <- paste0(stem, ".xpt")
  d
}

## Cycles differ in which questions were asked, so columns are unioned, not intersected.
raw_domains <- lapply(names(domains), function(nm) {
  cat(sprintf("%-18s %s\n", nm, paste(domains[[nm]], collapse = " ")))
  bind_rows(lapply(domains[[nm]], read_one)) %>% as.data.frame()
})
names(raw_domains) <- names(domains)

dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
saveRDS(raw_domains, OUT)

cat("\nSaved", OUT, "\n")
for (nm in names(raw_domains)) {
  cat(sprintf("  %-18s %7d rows %4d cols\n", nm,
              nrow(raw_domains[[nm]]), ncol(raw_domains[[nm]])))
}
