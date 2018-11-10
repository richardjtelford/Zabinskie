#helper functions

#force figures to go in correct place
float_tex <- function(tex, clean = TRUE) {
  x <- readLines(tex)
  pos <- grep('begin\\{figure\\}\\[htbp\\]', x)
  x[pos] <- gsub('htbp', 'H', x[pos])
  writeLines(x, tex)
}

#numbers to words
as.english <- function(x){ # problem with new version of rmarkdown
  as.character(english::as.english(x))
}

#numbers to Words
as.English <- function(x){ #sentence case
  stringi::stri_trans_totitle(
    as.character(as.english(x)), 
    opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))
}

#format pvalue
format_p <- function(p) {
  ifelse(p < 0.001, "< 0.001", paste("=", signif(p, 2)))
}

