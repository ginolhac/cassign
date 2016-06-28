context("cassign main")

test_that("overlap state and reference", {
  x <- data.frame(pat   = c("A", "A", "B"),
                  chr   = rep("chr1", 3),
                  start = c(10, 15, 20),
                  end   = c(15, 18, 25),
                  state = 1:3)
  #P   chr  start    end state length_kb
  #1 P2_diffNHEM100     1      0  10000     0        10
  #2 P2_diffNHEM100     1  10000 178000     2       168
  #3 P2_diffNHEM100     1 178000 227000     0        49

  y <- data.frame(CHR   = rep("chr1", 3),
                  START = c(10, 15, 20),
                  STOP  = c(15, 18, 25),
                  GENE  = c("ABC", "BCD", "CDE"))
  #CHR START  STOP STRAND      GENE     REFID
  #1     1 11873 14409      +   DDX11L1 NR_046018
  #2     1 14361 29370      -    WASH7P NR_024540
  #3     1 17368 17436      - MIR6859-4 NR_128720
  res <- assign_state(seg = x, column = pat, patient = "A", ref = y)
  # test nrows
  #testthat::expect_equal(nrow(as.data.frame(ov)), nrow(p_seg[GenomicRanges::subjectHits(ov), ]))

  expect_equal(res$state, 1:2)
  #expect_equal(res$GENE, c("ABC", "BCD"))
})