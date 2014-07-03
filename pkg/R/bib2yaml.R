
bib2yaml <- function(bib, keys) {

    x <- ReadBib(bib)[keys]
    as.yaml(x)
}


