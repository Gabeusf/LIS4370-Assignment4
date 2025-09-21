## ===============================
## Assignment #4 – Hospital Patient Data
## ===============================

## 0) Setup: make a folder for outputs
out_dir <- "outputs_a4"
if (!dir.exists(out_dir)) dir.create(out_dir)

## 1) DATA PREP
## Option A: (recommended) read the CSV you downloaded from Canvas.
##   Put the file in your project folder and set the file name here.
csv_path <- "hospital_patients.csv"   # <-- rename to the actual file name

if (file.exists(csv_path)) {
  raw <- read.csv(csv_path, stringsAsFactors = FALSE)
  # Expecting columns something like: Frequency, BloodPressure, FirstAssess, SecondAssess, FinalDecision
} else {
  ## Option B: if you don't have the CSV handy, paste the vectors from Canvas here.
  ## Replace the c(...) contents with the exact values shown on the assignment page.
  Frequency    <- c(0.6, 0.3, 0.4, 0.4, 0.2, 0.6, 0.3, 0.4, 0.9, 0.2)               # example
  BloodPressure<- c(163, 87, 32, 42, 59, 109, 78, 205, 135, 176)                     # example
  FirstAssess  <- c(1, 1, 1, 1, 0, 0, 0, NA, 1, 0)                                   #  bad=1, good=0
  SecondAssess <- c(0, 0, 1, 1, 0, 0, 1, 1, 1, 1)                                    #  low=0, high=1  (example)
  FinalDecision<- c(0, 0, 1, 0, 1, 0, 1, 0, 1, 1)                                    #  low=0, high=1  (example)
  
  raw <- data.frame(
    Frequency, BloodPressure, FirstAssess, SecondAssess, FinalDecision,
    stringsAsFactors = FALSE
  )
}

## 1a) Convert any categorical strings to numeric codes
## If your CSV already uses strings like "bad"/"good" or "low"/"high", map them here.
to01 <- function(x, bad_is_1 = FALSE) {
  # Handles "bad/good" or "low/high" or already-numeric vectors, and keeps NA as NA
  if (is.numeric(x)) return(x)
  x <- trimws(tolower(x))
  if (any(x %in% c("bad","good"))) {
    res <- ifelse(x == "bad", 1, ifelse(x == "good", 0, NA))
  } else if (any(x %in% c("low","high"))) {
    res <- ifelse(x == "low", 0, ifelse(x == "high", 1, NA))
  } else {
    # fallback: try to coerce
    suppressWarnings(res <- as.numeric(x))
  }
  res
}

df_hosp <- within(raw, {
  FirstAssess   <- to01(FirstAssess)
  SecondAssess  <- to01(SecondAssess)
  FinalDecision <- to01(FinalDecision)
})

## 1b) Inspect & handle NA
cat("\n--- Summary BEFORE NA removal ---\n"); print(summary(df_hosp))
na_rows <- which(!complete.cases(df_hosp))
if (length(na_rows)) {
  cat("\nRows with NA that will be dropped:", paste(na_rows, collapse = ", "), "\n")
}
df_hosp_clean <- na.omit(df_hosp)
cat("\n--- Summary AFTER na.omit ---\n"); print(summary(df_hosp_clean))

## 2) BASIC VISUALS (Base R), including PNG exports
## Helper to save a base R plot to PNG with consistent size
save_png <- function(filename, expr, width=1200, height=900, res=150) {
  png(file.path(out_dir, filename), width=width, height=height, res=res)
  on.exit(dev.off(), add=TRUE)
  eval.parent(substitute(expr))
}

## A) Side-by-side boxplots
##  Blood Pressure by each assessment and final decision
par(mfrow=c(1,1))  # ensure single plot layout

# 1. BP by First Assess (names: Good/Bad per assignment order)
save_png("boxplot_bp_by_first.png", {
  boxplot(
    BloodPressure ~ FirstAssess,
    data = df_hosp_clean,
    names = c("Good","Bad"),
    ylab = "Blood Pressure",
    main = "BP by First MD Assessment"
  )
})

# 2. BP by Second Assess (names: Low/High)
save_png("boxplot_bp_by_second.png", {
  boxplot(
    BloodPressure ~ SecondAssess,
    data = df_hosp_clean,
    names = c("Low","High"),
    ylab = "Blood Pressure",
    main = "BP by Second MD Assessment"
  )
})

# 3. BP by Final Decision (names: Low/High)
save_png("boxplot_bp_by_final.png", {
  boxplot(
    BloodPressure ~ FinalDecision,
    data = df_hosp_clean,
    names = c("Low","High"),
    ylab = "Blood Pressure",
    main = "BP by Final Decision"
  )
})

## B) Histograms: Frequency and Blood Pressure
save_png("hist_frequency.png", {
  hist(
    df_hosp_clean$Frequency,
    breaks = seq(0, 1, by = 0.1),
    xlab = "Visit Frequency",
    main = "Histogram of Visit Frequency"
  )
})

save_png("hist_bloodpressure.png", {
  hist(
    df_hosp_clean$BloodPressure,
    breaks = 8,
    xlab = "Blood Pressure",
    main = "Histogram of Blood Pressure"
  )
})

## 3) Quick numeric summaries you can reference in your write-up
bp_by_final <- by(df_hosp_clean$BloodPressure, df_hosp_clean$FinalDecision, summary)
cat("\n--- BP summary by FinalDecision (0=Low, 1=High) ---\n"); print(bp_by_final)

## 4) Save a cleaned CSV you can push to GitHub
write.csv(df_hosp_clean, file.path(out_dir, "hospital_patients_clean.csv"), row.names = FALSE)

cat("\nAll plots saved in:", normalizePath(out_dir), "\n")
