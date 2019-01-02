

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


CCInter.data.frame <- function(  x,
                                 cases,
                                 exposure,
                                 by,
                                 table = FALSE,
                                 full = FALSE
)
{
  L_LABELS1   <- c()
  L_TAB       <- c()
  L_CASES     <- c()
  L_CONTROLS  <- c()
  L_CIL       <- c()
  L_CIH       <- c()
  L_STATS     <- c()
  L_ESTIMATE  <- c()

  NB_TOTAL    <- 0

  T.Controls  <- c()
  T.Cases     <- c()
  T.OR        <- c()
  T.Marks     <- c("++","+-","-+","reference   --", "Total")
  T.TCA <- 0
  T.TCO <- 0


  .strate <- as.factor(x[,by])
  .strateError = "One of your strata has zero cases in the cells."

  .df <- x
  # Return labels of columns of the output data.frame
  # ---------------------------------------------------------------------------
  getColnames <- function() {
    .Col1Label = sprintf("CCInter %s - %s by(%s)", cases, exposure, by)
    c(.Col1Label, c("Cases","Controls","P.est.","Stats","95%CI-ll","95%CI-ul"))
  }
  getColnames2 <- function() {
    c("P.estimate","Stats","95%CI-ll","95%CI-ul")
  }

  getPestNames <- function(ODD) {
    if (ODD > 1.0) {
      c("Odds ratio", "Attrib.risk.exp", "Attrib.risk.pop", NA, NA, NA)
    } else {
      c("Odds ratio", "Prev. frac. ex.", "Prev. frac. pop", NA, NA, NA)
    }
  }

  getCrudeOR <- function(d) {
    # df <- x[!is.na(x[cases]) & !is.na(x[exposure]) & !is.na(x[by]),
    #            c(cases, exposure)]
    .T <- table(d[,cases], d[,exposure])
    .r = or(.T)
    .r
  }


  # Returns labels for each level of 'by'
  # ---------------------------------------------------------------------------
  getRisksLabels <- function(.level) {
    .label = sprintf("%s = %s", by, .level);
    c(.label, "Exposed", "Unexposed", "Total", "Exposed %", "______________")
  }

  getMHLabels <- function() {
    label2 = sprintf("Crude OR for %s", exposure);
    label3 = sprintf("MH OR %s adjusted for %s", exposure, by);
    c("MH test of Homogeneity",
      label2, label3, "Adjusted/crude relative change")
  }

  # Loop on all levels of 'by' (strates)
  # -----------------------------------------------------------------
  getRRStats <- function() {
    if (!is.factor(x[, cases])) {
      .T = table(!x[, exposure], !x[, cases], .strate)
    } else {
      .d <- x
      .d[, cases] <- 1 - (as.numeric(x[, cases])-1)
      .d[, exposure] <- 1 - (as.numeric(x[, exposure])-1)
      .T = table(.d[, exposure], .d[, cases], .strate)
    }
    .loop = length(levels(.strate))
    .Compute = TRUE
    .T <- .T1 <- toNumeric(.T, .loop)

    retrieveLast <- function(.T) {
      i <- length(.T[1,2,])
      if (.T[1,1, i] == 0 | .T[2,1, i] == 0 | .T[1,2, i] == 0 | .T[2,2, i] == 0) {
        msg <- sprintf("Stratum %d has values = 0 and has been removed", i)
        warning(msg)
        .T <- .T[, , -i]
        .T <- retrieveLast(.T)
      }
      .T
    }

    .T <- retrieveLast(.T)
    S_  <- summary(epi.2by2(.T, method = "case.control",
                            outcome="as.columns",
                            homogeneity = "woolf"))

    .loop = length(.T[1,2,])
    NB_LEVELS = .loop
    .ind <- .loop:1
    for (i in .loop:1) {
      j <- .ind[[i]]
      .level <- levels(.strate)[i]

      A_CE = .T[1,1, i]    ; # Cases exposed
      C_CU = .T[2,1, i]    ; # Cases unexposed
      B_HE = .T[1,2, i]    ; # Healthy exposed
      D_HU = .T[2,2, i]    ; # Healthy unexposed
      T_EX <- A_CE + B_HE
      T_UN <- C_CU + D_HU
      T_CT <- B_HE + D_HU  ; # Total Controls

      L_LABELS1 <- c(L_LABELS1, getRisksLabels(.level))

      # CASES
      # ------------------------------------------------------------
      L_CASES <- c(L_CASES, NA, A_CE, C_CU);
      TOTAL <-  A_CE + C_CU;
      NB_TOTAL = NB_TOTAL + TOTAL;
      EXPOSED_PC <-sprintf("%3.1f%%", (A_CE / TOTAL) * 100)
      L_CASES <- c(L_CASES, TOTAL, EXPOSED_PC, NA);
      # CONTROLS
      # ------------------------------------------------------------
      L_CONTROLS <- c(L_CONTROLS, NA, B_HE, D_HU);
      TOTAL <-  B_HE + D_HU;
      NB_TOTAL = NB_TOTAL + TOTAL;
      EXPOSED_PC <- sprintf("%3.1f%%", (B_HE / TOTAL) * 100)
      L_CONTROLS <- c(L_CONTROLS, TOTAL, EXPOSED_PC, NA);

      if (i < 3) {
        T.Cases <- c(T.Cases, A_CE, C_CU)
        T.Controls <- c(T.Controls, B_HE, D_HU)
        T.OR  <- c(T.OR, NA, NA)
        T.TCA <- T.TCA + A_CE + C_CU
        T.TCO <- T.TCO + B_HE + D_HU
      }

      # ODDS RATIO
      # ------------------------------------------------------------
      num <- NULL
      .d <- S_$OR.strata.score
      .d <- .d %>% mutate(num = 1:nrow(.d)) %>% arrange(desc(num))
      ODD  <- .d[j, "est"]
      .d <- S_$OR.strata.mle
      .d <- .d %>% mutate(num = 1:nrow(.d)) %>% arrange(desc(num))
      .CIL <- .d[j, "lower"]
      .CIH <- .d[j, "upper"]
      L_STATS <- c(L_STATS, ODD);
      L_CIL = c(L_CIL, .CIL);
      L_CIH = c(L_CIH, .CIH);

      # print(i)
      # if (i == 2) {
      #   return(L_STATS)
      # }
      # P.est.
      # -------------------------------------------------------------
      L_ESTIMATE <- c(L_ESTIMATE, getPestNames(round(ODD, 8)))

      # Attribuable Risk Ext.
      # ------------------------------------------------------------
      if (ODD >= 1.0) {
        .d <- S_$AFest.strata.wald
        .d <- .d %>% mutate(num = 1:nrow(.d)) %>% arrange(desc(num))
        #R <- CC_AR(.T);
        V_AR  = .d[j, "est"]   # Attrib.risk.exp
        V_CIL = .d[j, "lower"] # Confidence interval low
        V_CIH = .d[j, "upper"] # Confidence interval hight
        L_STATS <- c(L_STATS, V_AR);
        L_CIL = c(L_CIL, V_CIL, NA, NA, NA, NA);
        L_CIH = c(L_CIH, V_CIH, NA, NA, NA, NA);

        # Attribuable Risk Pop.
        # ------------------------------------------------------------
        .d <- S_$PAFest.strata.wald
        .d <- .d %>% mutate(num = 1:nrow(.d)) %>% arrange(desc(num))
        AFP <- .d[j, "est"]
        L_STATS <- c(L_STATS, AFP, NA, NA, NA);
      } else {
        V_AR <- 1 - ODD
        V_CIL <- 1 - .CIH
        V_CIH <- 1 - .CIL
        L_STATS <- c(L_STATS, V_AR);
        L_CIL = c(L_CIL, V_CIL, NA, NA, NA, NA);
        L_CIH = c(L_CIH, V_CIH, NA, NA, NA, NA);
        # Prev.frac.pop ---------------------------------------------
        Pe <- B_HE / T_CT
        AFP <- Pe * (1-ODD)
        L_STATS <- c(L_STATS, AFP, NA, NA, NA)
      }
    }

    if (table == TRUE) {
      T.Cases <- c(T.Cases, T.TCA)
      T.Controls <- c(T.Controls, T.TCO)
      T.OR  <- c(T.OR, NA)
    }


    # Number of obs
    # ------------------------------------------------------------
    L_CASES = c(L_CASES, NB_TOTAL);

    # MISSING
    # ------------------------------------------------------------
    .nrow <- nrow(x)
    MIS_TO = .nrow - NB_TOTAL;
    MIS_PC = sprintf("%3.2f%s", (MIS_TO / .nrow)*100, '%');
    L_CASES = c(L_CASES, MIS_TO);

    L_LABELS1 <- c(L_LABELS1, "Number of obs", "Missing")
    L_CONTROLS <- c(L_CONTROLS, NA, NA)
    L_ESTIMATE <- c(L_ESTIMATE, NA, NA)
    L_STATS <- c(L_STATS, NA, NA)
    L_CIL <- c(L_CIL, NA, NA)
    L_CIH <- c(L_CIH, NA, NA)
    # print(L_STATS)
    DF1 <- data.frame(L_LABELS1, L_CASES, L_CONTROLS, L_ESTIMATE, S2(L_STATS), S2(L_CIL), S2(L_CIH))
    colnames(DF1) <- getColnames()

    # return(DF1)
    df <- x[!is.na(x[,exposure]),]
    df <- df[!is.na(df[,by]),]
    df <- df[!is.na(df[,cases]),]

    .T <- table(df[,cases], df[,exposure], df[,by]);
    .T <- toNumeric(.T, .loop)
    R <- CC_STATS(.T);

    # MH test of Homogeneity pvalue
    # ------------------------------------------------------------
    STAT = R$OR.homog$p.value;
    L_STATS <- c(STAT);
    #    return(R)

    # Crude OR for exposure
    # ------------------------------------------------------------
    .ror <- getCrudeOR(df)
    STAT = .ror[1]
    CIL = .ror[2]
    CIH = .ror[3]
    L_STATS <- c(L_STATS, STAT);
    L_CIL = c(NA, CIL);
    L_CIH = c(NA, CIH);
    OR.crude = STAT

    # MH OR for exposure adjusted for by
    # ------------------------------------------------------------
    STAT = R$OR.mh.wald$est;
    CIL = R$OR.mh.wald$lower
    CIH = R$OR.mh.wald$upper
    OR.mh = STAT

    L_STATS <- c(L_STATS, STAT);
    L_CIL = c(L_CIL, CIL, NA);
    L_CIH = c(L_CIH, CIH, NA);

    # Adjusted/crude relative change
    # ------------------------------------------------------------
    STAT = 100 * ((OR.mh - OR.crude)/OR.crude);
    L_STATS <- c(L_STATS, STAT);

    L_LABELS1 = getMHLabels()

    DF2 <- data.frame(L_LABELS1, S2(L_STATS), S2(L_CIL), S2(L_CIH))
    colnames(DF2) <- getColnames2()

    if (table == TRUE) {
      .Col1 <- sprintf("%s / %s", by, exposure)
      T.Col <- c(.Col1, "Cases", "Controls", "OR")

      P11 <- T.Cases[1] / (T.Cases[1]+T.Controls[1])
      P10 <- T.Cases[2] / (T.Cases[2]+T.Controls[2])
      P01 <- T.Cases[3] / (T.Cases[3]+T.Controls[3])
      P00 <- T.Cases[4] / (T.Cases[4]+T.Controls[4])

      # print(P11 - P10 - P01 + 1)
      OR11 <- (P11/(1-P11)) / (P00/(1-P00))
      OR10 <- (P10/(1-P10)) / (P00/(1-P00))
      OR01 <- (P01/(1-P01)) / (P00/(1-P00))
      T.OR <- c(round(OR11,2), round(OR10,2), round(OR01,2), NA, NA)

      DF3 <- data.frame(T.Marks, T.Cases, T.Controls, T.OR)
      colnames(DF3) <- T.Col

      # -------------------- STATS -------------------------------------------
      # local _inter = (`_rr10' -1) + (`_rr01' - 1) + 1
      # inter = (`_rr11' - 1 ) - (`_rr10' -1) - (`_rr01' - 1)
      .Labs <- c("Observed OR when exposed to both",
                 "Expected OR if exposed to both and no interaction",
                 "Interaction")
      S.OBOR <- OR11
      S.EXOR <- (OR10 - 1) + (OR01 - 1) + 1
      S.INTR <- OR11 - S.EXOR

      DF4 = data.frame(.Labs, c(round(S.OBOR,2), round(S.EXOR,2), round(S.INTR,2)))
      colnames(DF4) <- c("Statistic","Value")

    }
    if (full == TRUE) {
      if (.Compute == TRUE) {
        ret <- list(df1 = DF1, df2=DF2, df1.align="lccrlrrr", df2.align="lccrrr")
      } else {
        ret <- list(df1 = DF1, df2=.strateError, df1.align="lccrlrrr", df2.align="lccrrr")
      }
      if (table == TRUE) {
        if (.Compute == TRUE) {
          ret <- list(df1 = DF1, df2=DF2, df1.align="lccrlrrr", df2.align="lccrrr",
                      df3 = DF3, df4 = DF4)
        } else {
          ret <- list(df1 = DF1, df2=.strateError, df1.align="lccrlrrr", df2.align="lccrrr",
                      df3 = DF3, df4 = DF4)
        }
      }
    } else {
      if (.Compute == TRUE) {
        ret <- list(df1 = DF1, df2=DF2)
      } else {
        ret <- list(df1 = DF1, df2=.strateError)
      }
      if (table == TRUE) {
        if (.Compute == TRUE) {
          ret <- list(df1 = DF1, df2=DF2, df3 = DF3, df4 = DF4)
        } else {
          ret <- list(df1 = DF1, df2=.strateError, df3 = DF3, df4 = DF4)
        }
      }
    }

    ret

  }

  getRRStats()

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
