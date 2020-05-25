hq_tbl <- data.table::data.table(
          HQ = c("HQ1","HQ2","HQ5","HQ10","HQ20","HQ50",
                 "HQ100","HQ200","HQ500","HQExtrem"),
       Basel = c(NA, 2697L, 3074L, 3702L, 4240L, 4560L, 4780L, 4980L, NA, 5480L),
       Maxau = c(NA, 3150L, 3594L, 4100L, 4500L, 4900L, 5300L, 5700L, NA, 6500L),
       Worms = c(3048L, 3579L, 4145L, 4750L, NA, 5750L, 6300L, 6700L, NA, 7600L),
       Mainz = c(3553L,4250L,5008L,5680L,NA,7060L,
                 7900L,8700L,9500L,10300L),
        Kaub = c(3689L,4412L,5204L,5780L,NA,7120L,
                 8000L,8800L,9600L,10400L),
   Andernach = c(5325L,6526L,7918L,8810L,NA,10950L,
                 11850L,12670L,13960L,15250L),
        Bonn = c(5305L,6493L,7874L,8880L,NA,11000L,
                 11910L,12720L,14010L,15300L),
        Köln = c(5486L,6678L,8026L,9010L,NA,11100L,
                 12000L,12900L,14100L,15300L),
  Düsseldorf = c(5591L,6799L,8131L,9100L,NA,10870L,
                 12000L,13400L,14100L,15300L),
     Ruhrort = c(5714L,6959L,8370L,9470L,NA,11380L,
                 12400L,13400L,14600L,15800L),
       Wesel = c(5679L,6898L,8285L,9450L,NA,11500L,
                 12400L,13400L,14600L,15800L),
        Rees = c(5743L,6976L,8377L,9410L,NA,11400L,
                 12300L,13300L,14500L,15700L),
    Emmerich = c(5697L,6918L,8293L,9380L,NA,11310L,
                 12200L,13100L,14300L,15800L)
)
#' @export
find_hq <- function(value, pegel) {
  if (is.na(value) | is.infinite(value)) return('')
  p_hq <- hq_tbl[!is.na(get(pegel)), get(pegel)]
  p_hq_101 <- p_hq * 1.01
  p_hq_099 <- p_hq * 0.99
  p_hq_tbl <- data.table(p_hq_099, p_hq_101)
  p_hq_tbl[, orig := .I]
  hq_t <- hq_tbl[!is.na(get(pegel)), HQ]
  stopifnot(length(p_hq) > 0 & length(hq_t) > 0)
  p_pos <- p_hq_tbl[value >= p_hq_099 & value <= p_hq_101, orig]
  if (length(p_pos) == 1) {
    ret <- hq_t[p_pos]
  } else {
    p_hq2 <- sort(c(value, p_hq))
    p_hq_len <- length(p_hq)
    p_pos <- which(p_hq2 == value)
    if (p_pos == 1) {
      ret <- paste("<", hq_t[1])
    } else if (p_pos > p_hq_len) {
      ret <- paste(">", hq_t[p_hq_len])
    } else {
      ret <- paste0(hq_t[p_pos - 1], '-', hq_t[p_pos])
    }
  }
  return(ret)
}

