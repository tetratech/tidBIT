# new stuff for notebooks
# Erik, 20240910
# move to other files later
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Prepare table of coefficients for GAM analysis
#'
#' @param lmo output from gam model
#' @keywords internal
#' @export
#'
gamCoeff <- function(lmo) {

  # lmo <- gs

  lm.sum <- summary(lmo)
  p.se <- lm.sum$se[1:length(lm.sum$p.coeff)]
  lm.coeff <- data.frame(
    source    = names(lm.sum$p.coeff),
    estimate  = round(lm.sum$p.coeff,6),
    std.error = round(p.se,6),
    t.value   = round(lm.sum$p.t,4),
    p.value   = .fmtPval(lm.sum$p.pv), stringsAsFactors = FALSE)
  rownames(lm.coeff) <- NULL

  return(lm.coeff)
}


#' Format pvalues
#'
#' @param pval pvalue to format
#' @keywords internal
#' @export
#'
.fmtPval <- function(pval) {

  pval <- ifelse((pval<0.0001),"<0.0001",sprintf('%6.4f',pval))
  pval[is.na(pval)] <- "-"
  fmtPval <- pval
}


#' Prepare ANOVA table for GAM analysis
#'
#' @param lmo output from gam model
#' @keywords internal
#' @export
#'
gamANOVA <- function(gamo) {

  # gamo <- gs

  anov.gamo     <- anova(gamo)
  if (length(anov.gamo$pTerms.table)>0) {
    agamp <- data.frame(anov.gamo$pTerms.table)
  } else {
    agamp <- data.frame(df=NA_real_,F=NA_real_,p.value=NA_real_)
    rownames(agamp) <- "NA"
  }
  agamp$type    <- '   "      " '
  agamp$type[1] <- 'parametric terms'
  agams         <- data.frame(anov.gamo$s.table)
  agams$type    <- '   "      " '
  agams$type[1] <- 'smoothed terms'
  names(agams)[names(agams)=='edf'] <- 'df' # rename edf
  agams         <- agams[,names(agams)!="Ref.df"]   # drop ref.df
  agamo         <- rbind(agamp,agams)
  agamo$source  <- rownames(agamo)
  agamo[,1]     <- round(agamo[,1],2)  # round edf
  agamo[,2]     <- round(agamo[,2],4)  # round F-stat
  agamo[,3]     <- .fmtPval(agamo[,3]) # format p-value
  rownames(agamo) <- NULL
  agamo <- agamo[,c(c(4,5,1,2,3))]
  return(agamo)
}

