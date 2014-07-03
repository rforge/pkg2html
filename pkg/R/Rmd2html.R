
Rmd2html <- function(file) {

    x <- readLines(file)

    header <- grep("^---$", x)

    knit(file)
    markdownToHTML(gsub("Rmd$", "md", file),
                   output = gsub("Rmd$", "html", file),
                   fragment.only = TRUE)

    rval <- c(x[header[1]:header[2]], " ", readLines(gsub("Rmd$", "html", file)))

    rval
}
    