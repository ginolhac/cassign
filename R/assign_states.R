#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @import S4Vectors
#' @import GenomicRanges
NULL

#' Assign states to genes
#' @param patient string of patient to keep
#' @param P column name for patients
#' @param ref data.frame of genes
#' @param seg data.frame of cnv
#' @return A data.frame of assigned cnvs
#' @export
#' @examples
#' \dontrun{
#' library(cassign)
#'
#' # Import file
#' e <- read.plate("od_measure.xls")
#' e <- elisa.analyze(e)
#' e <- elisa.analyze(e, blank = TRUE, transform = TRUE)
#' }
#'
assign_state <- function(patient, P, ref, seg) {

  p_seg <- seg %>%
    dplyr::filter(P == patient) %>%
    dplyr::select(-P)
  p_gr <- GenomicRanges::GRanges(p_seg)
  ref_gr <- GenomicRanges::GRanges(ref)
  # overlap for all genes, associated a CNV is completely WITHIN it
  # ignore strand
  ov <- GenomicRanges::findOverlaps(query = ref_gr, subject = p_gr,
                     type = "within", ignore.strand = TRUE)
  # test nrows
  #testthat::expect_equal(nrow(as.data.frame(ov)), nrow(p_seg[GenomicRanges::subjectHits(ov), ]))
  res <- dplyr::bind_cols(p_seg[subjectHits(ov), ],
                   ref[queryHits(ov), ])
  return(patient = res)
}
