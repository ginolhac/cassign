#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr select_
#' @importFrom dplyr %>%
#' @import S4Vectors
#' @import GenomicRanges
NULL

#' Assign states to genes
#' @param seg data.frame of cnv
#' @param column column name for patients
#' @param patient string of patient to keep
#' @param ref data.frame of genes
#' @return A data.frame of assigned cnvs
#' @export
#' @examples
#' \dontrun{
#' library(cassign)
#'
#' res <- assign_state("A", "pat", y, x)
#' }
#'
assign_state <- function(seg, column, patient, ref) {

  stopifnot (is.data.frame(seg) | is.data.frame(ref))
  args <- as.list(match.call())
  p_seg <- seg %>%
    dplyr::filter(eval(args$column, seg) == patient) %>%
    dplyr::select_(paste0("-", deparse(args$column)))
  p_gr <- GenomicRanges::GRanges(p_seg)
  ref_gr <- GenomicRanges::GRanges(ref)
  # overlap for all genes, associated a CNV is completely WITHIN it
  # ignore strand
  ov <- GenomicRanges::findOverlaps(query = ref_gr, subject = p_gr,
                     type = "within", ignore.strand = TRUE)

  res <- dplyr::bind_cols(p_seg[subjectHits(ov), ],
                   ref[queryHits(ov), ])
  return(patient = res)
}

