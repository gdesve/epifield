

computeRiskCI <- function(risk, X1, N1, X2, N2)
{
  A = ((N1-X1)/X1)/N1;
  B = ((N2-X2)/X2)/N2;
  R1 = log(risk) + (1.96*sqrt(A + B));
  R2 = log(risk) - (1.96*sqrt(A + B));
  E1 = exp(R1);
  E2 = exp(R2);

  return(c(E2, E1));
}

# Comppute ODDS ratio
# -----------------------------------------------------------------------------
or <- function(.T)
{
  O <- (.T[1,1]/.T[1,2]) / (.T[2,1]/.T[2,2]);
  x <- matrix(.T, 2, byrow = TRUE);
  R <- fisher.test(x);
  CIL <- R$conf.int[1];
  CIH <- R$conf.int[2];
  return(c(O, CIL, CIH));
}


fmt <- function(V) {
  as.numeric(as.character(V))
}

rr <- function(Tb)
{

  TE = Tb[2,1]+Tb[2,2];
  TU = Tb[1,1]+Tb[1,2];
  CE = Tb[2,2];
  CU = Tb[1,2];
  TO = TE + TU;

  RE = CE / TE;
  RU = CU / TU;

  RR  = RE/RU;
  CI = computeRiskCI(RR, CE, TE, CU, TU);
  RRCIL = CI[1];
  RRCIH = CI[2];

  return(c(RR, RRCIL, RRCIH))
}

# rr2 <- function(Tb)
# {
#   TE = Tb[2,1]+Tb[2,2];
#   TU = Tb[1,1]+Tb[1,2];
#   CE = Tb[2,2];
#   CU = Tb[1,2];
#   TO = TE + TU;

#   X   The number of disease occurence among exposed cohort.
#   Y	  The number of disease occurence among non-exposed cohort.
#   m1  The number of individuals in exposed cohort group.
#   m2  The number of individuals in non-exposed cohort group.
#   conf.level  Probability for confidence intervals. Default is 0.95.

#   R <- riskratio(CE, CU, TE, TU, conf.level=0.95)
#   return(c(R$estimate, R$conf.int[1], R$conf.int[2]))
# }

# ========================================================================
# CASES CONTROLS STUDY
# ========================================================================
CC_AR <- function(C)
{
  T = epi.2by2(dat=C, method="case.control");
  S <- summary(T);
  return(c(S$AFest[1], S$AFest[2], S$AFest[3]));
}

CC_PAR <- function(C)
{
  .T = epi.2by2(dat=C, method="case.control", outcome="as.columns");
  S <- summary(.T);
  return(S$PAFest.strata.wald$est);
}

CC_STATS <- function(C)
{
  .T = epi.2by2(dat=C, method="case.control", outcome="as.columns", homogeneity="woolf");
  S <- summary(.T);
  return(S);
}

CS_STATS <- function(C)
{
  .T = epi.2by2(dat=C, method="cohort.count", outcome="as.columns");
  S <- summary(.T);
  return(S);
}

computeORCI <- function(OR, a,b,c,d)
{
  LNO = log(OR)
  R1 = sqrt((1/a)+(1/b)+(1/c)+(1/d))
  CIL = exp(LNO - 1.96 * R1)
  CIH = exp(LNO + 1.96 * R1)
  return(c(CIL, CIH))
}

computeExactORCI <- function(a,b,c,d)
{
  x <- matrix(c(a, b, c, d), 2, byrow = TRUE);
  R <- fisher.test(x);
  CIL <- R$conf.int[1];
  CIH <- R$conf.int[2];
  return(c(CIL, CIH, R$p.value));
}

computeDiffRiskCI <- function(RE, RU, NE, NU)
{
  A = RE - RU;
  B = (RE * (1-RE))/NE;
  C = (RU * (1-RU))/NU;
  D = 1.96*sqrt(B + C);
  R1 = A + D;
  R2 = A - D;

  return(c(R2, R1));
}

GetStrateVector <- function(A) {
  CE = A[2,2]    ; # Cases exposed
  CU = A[1,2]    ; # Cases unexposed
  HE = A[2,1]    ; # Healthy exposed
  HU = A[1,1]    ; # Healthy unexposed

  TE = CE + HE   ; # Total exposed
  TU = CU + HU   ; # Total unexposed
  TS = TE + TU   ; # Total strate
  H  = HU + HE   ; # Total healthy
  C  = CU + CE   ; # Total cases

  c(CE, CU, HE, HU, TE, TU, TS, H, C)
}

CMHrr <- function(A, B)
{
  # Stratum 1 =====================
  Ce1 = A[2,2]    ; # Cases exposed
  Cu1 = A[1,2]    ; # Cases unexposed
  He1 = A[2,1]    ; # Healthy exposed
  Hu1 = A[1,1]    ; # Healthy unexposed

  Te1 = Ce1 + He1 ; # Total exposed
  Tu1 = Cu1 + Hu1 ; # Total unexposed
  T1  = Te1 + Tu1 ; # Total strate 1
  H1  = Hu1 + He1 ; # Total healthy
  C1  = Cu1 + Ce1 ; # Total cases

  # Stratum 2 =====================
  Ce2 = B[2,2]    ; # Cases exposed
  Cu2 = B[1,2]    ; # Cases unexposed
  He2 = B[2,1]    ; # Healthy exposed
  Hu2 = B[1,1]    ; # Healthy unexposed

  Te2 = Ce2 + He2 ; # Total exposed
  Tu2 = Cu2 + Hu2 ; # Total unexposed
  T2  = Te2 + Tu2 ; # Total strate 2
  H2  = Hu2 + He2 ; # Total healthy
  C2  = Cu2 + Ce2 ; # Total cases



  R1 = ((Ce1 * Tu1) / T1) + ((Ce2 * Tu2) / T2);
  R2 = ((Cu1 * Te1) / T1) + ((Cu2 * Te2) / T2);
  rrmh = R1 / R2;

  R3 = ((C1*Te1*Tu1) - (Ce1*Cu1*T1)) / T1^2;
  R4 = ((C2*Te2*Tu2) - (Ce2*Cu2*T2)) / T2^2;
  R5 = R3 + R4;
  R6 = R5 / (R1 * R2);
  R7 = sqrt(R6);

  L = log(rrmh) - (1.96 * R7);
  H = log(rrmh) + (1.96 * R7);

  CIL = exp(L);
  CIH = exp(H);

  return(c(rrmh, CIL, CIH));
}

MH_HomogeneityTest <- function(mht)
{
  T = epi.2by2(dat=mht, homogeneity="woolf");
  print(summary(T))
  S <- summary(T);
  return(c(S$RR.homog[1], S$RR.homog[3]));
}



#' @title CC function
#'
#' @param cases cases vector
#' @param exposure vector
#' @param full display all
#'
#' @return The result
#' @export
#'
CC <- function(  cases,
                            exposure,
                            exact = F,
                            full = FALSE)

{
  if (length(cases) < 2 && length(exposure) < 2) {
    stop("Error: [cases] and [exposure] MUST be vectors. ")
  }
  .Cases <-cases
  .Exposure <- exposure
  .T1 = title
  .T2 = title

  Rownames1 <- c("Exposed", "Unexposed", "Total", "Proportion exposed")
  Colnames1 <- c("Cases", "Controls", "Total")
  Colnames2 <- c("Point estimate", "95%CI-ll", "95%CI-ul")
  PLabel <- c("chi2(1)", "Pr>chi2")


  # Contingency table
  # ===========================================================================
  FR = table(.Cases, .Exposure)
  I1E1 = FR[2,2] <- as.numeric(FR[2,2])
  I1E0 = FR[2,1] <- as.numeric(FR[2,1])
  I0E0 = FR[1,1] <- as.numeric(FR[1,1])
  I0E1 = FR[1,2] <- as.numeric(FR[1,2])

  STAT = computeKHI2(FR[1,1], FR[2,1], FR[1,2], FR[2,2]);

  FISHER <- NA
  if (exact == TRUE) {
    FISHER = computeFisher(FR[1,1], FR[2,1], FR[1,2], FR[2,2]);
    PLabel = c(PLabel, "Fisher p.value")
  }

  # Compute Total
  # ---------------------------------------------------------------------------
  TE = I0E1 + I1E1        ; # Total exposed
  TU = I0E0 + I1E0        ; # Total unexposed
  TCA = I1E1 + I1E0       ; # Total cases
  TNC = I0E1 + I0E0       ; # Total non-cases



  # Compute Proportions
  # ---------------------------------------------------------------------------
  PCAEX = I1E1/TCA
  PCTEX = I0E1/TNC
  PTOEX = TE/(TCA+TNC)

  COL_C <- as.character(c(I1E1, I1E0, TCA, sprintf("%2.2f",PCAEX)))
  COL_N <- as.character(c(I0E1, I0E0, TNC, sprintf("%2.2f",PCTEX)))
  COL_T <- as.character(c(TE, TU, TE+TU, sprintf("%2.2f",PTOEX)))
  #COL_R <- c(PCAEX, PCTEX, PTOEX)
  R1 <- data.frame(COL_C, COL_N, COL_T, row.names = Rownames1)
  colnames(R1) <- Colnames1


  # Estimate
  # ===========================================================================

  # ODD Ratio
  # ---------------------------------------------------------------------------

  R = or(FR);
  OREST = ODD <- R[1]
  ORCIL = R[2]
  ORCIH = R[3]


  rr = list(point_estimate = R, CI95.ll = ORCIL, CI95.ul = ORCIH)

  # Attr.frac.pop. <- Attr.frac.ex. x proportion.of.cases.exposed.
  # Prev.frac.pop. <- Prev.frac.ex. x proportion.of.controls.exposed
  if (R[1] >= 1.0) {
    R = CC_STATS(FR);
    # AFEST = as.numeric(R$AFest[1])

    AFEST = as.numeric((ODD-1) / ODD)
    AFCIL = as.numeric(R$AFest[2])
    AFCIH = as.numeric(round(R$AFest[3], 8))

    PAEST <- AFEST * PCAEX
    Rownames2 <- c("Odds ratio", "Attr. frac. ex.", "Attr. frac. pop", PLabel)

    RES <- c(point_estimate = AFEST, CI95.ll = AFCIL, CI95.ul = AFCIH)

    stats = list(odds_ratio=rr,
                 Attr.frac.ex = RES,
                 Attr.frac.pop = PAEST,
                 chi2 = as.numeric(STAT[1]),
                 p.chi2 = as.numeric(round(STAT[2], 8))    )
  }
  else {
    AFEST = as.numeric(1 - R[1])
    AFCIL = 1 - R[3]
    AFCIH = 1 - R[2]

    # Pe = TE / (TE + TU);
    # PAEST = Pe * (1 - R[1])
    PAEST <- AFEST * PCTEX
    Rownames2 <- c("Odds ratio", "Prev. frac. ex.", "Prev. frac. pop", PLabel)

    RES <- c(point_estimate = AFEST, CI95.ll = AFCIL, CI95.ul = AFCIH)

    stats = list(odds_ratio=rr,
                 Prev.frac.ex = RES,
                 Prev.frac.pop = PAEST,
                 chi2 = as.numeric(STAT[1]),
                 p.chi2 = as.numeric(round(STAT[2], 8))
    )

  }
  str_PCHI2 <- sprintf("%3.3f", as.numeric(round(STAT[2], 8)))
  str_FISH <- sprintf("%3.3f", round(FISHER, 5))

  .COL_PES <- c(S2(OREST), S2(AFEST), S2(PAEST), S2(as.numeric(STAT[1])), str_PCHI2)
  .COL_CIL <- c(ORCIL, AFCIL, NA, NA, NA)
  .COL_CIH <- c(ORCIH, AFCIH, NA, NA, NA)

  if (exact == TRUE) {
    .COL_PES <- c(.COL_PES, str_FISH)
    .COL_CIL <- c(.COL_CIL, NA)
    .COL_CIH <- c(.COL_CIH, NA)
  }

  R2 <- data.frame(.COL_PES, S2(.COL_CIL), S2(.COL_CIH), row.names = Rownames2)
  colnames(R2) <- Colnames2

  if (full == TRUE) {
    ret <- list(t1 = .T1, df1 = R1, df2 = R2,
                df1.align = c("rrr"),
                df2.align = c("rrr"),
                st = stats)
  } else {
    ret <- list(df1 = R1, df2 = R2)
  }

  ret

}
