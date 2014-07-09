
.person_has_maintainer_role <- function(x)
    "cre" %in% x$role

.people <- function(pkg, filter) {

    desc <- packageDescription(pkg = pkg)

    if (is.null(desc[["Authors@R"]])) {
        rval <- as.personList(desc$Author)
    } else {
        rval <- utils:::.read_authors_at_R_field(desc[["Authors@R"]])
        rval <- Filter(filter, rval)
    }
    rval
}

.authors <- function(pkg)
    .people(pkg, utils:::.person_has_author_role)

.maintainer <- function(pkg) {
    desc <- packageDescription(pkg)
    if (is.null(desc[["Authors@R"]]))
        return(as.personList(maintainer(pkg)))
    .people(pkg, .person_has_maintainer_role)
}

person2yaml <- function(x) {

    rval <- lapply(x, function(r) {
            name <- format(r, include = c("given", "family"))
            r <- unclass(r)
            r[[1]]$name <- name
            r[[1]]$email <- gsub("@", "_@_", r[[1]]$email)
            r[[1]][c("name", names(r[[1]])[names(r[[1]]) != "name"])]	
        })
    as.yaml(rval)
}

desc2yaml <- function(pkg) {

    desc <- packageDescription(pkg)
    desc$Download <- paste("http://CRAN.R-project.org/package=", pkg, sep = "")
    desc$Manual <- paste("http://CRAN.R-project.org/web/packages/", 
                         pkg, "/", pkg, ".pdf", sep = "")
    desc$Source <- paste("http://CRAN.R-project.org/src/contrib/", pkg, "_", 
                         desc$Version, ".tar.gz", sep = "")
    desc$Devel <- paste("https://r-forge.r-project.org/projects/", pkg, sep = "")
    desc$License <- desc$License

    ### <FIXME> vignettes </FIXME>

    as.yaml(desc[c("Package", "Title", "Version", "Date", "Description", 
                   "Download", "Manual", "Source", "Devel", "License")])
}


NEWS2md <- function(pkg) {

    db <- news(package = pkg)

    versions <- unique(db$Version)

    for (v in versions) {

        tmp <- subset(db, Version == v)

        txt <- c("---",
                 "layout: post",
                 paste("title:  Version", v),
                 paste("date: ", unique(tmp$Date)),
                 "---",
                 "",
                 "<ul>",
                 paste("<li>", tmp$Text, "</li>"),
                 "</ul>")

        writeLines(txt, con = paste(unique(tmp$Date), "-", v, ".html", sep = ""))

    }
}

depends2yaml <- function(pkg, field = c("Depends", "Imports", "Suggests")) {

    x <- available.packages(contriburl = contrib.url("http://cran.at.r-project.org"))

    fun <- function(field) {

        x <- x[grep(pkg, x[, field]), "Package"]
        url <- paste("http://CRAN.R-project.org/package=", x, sep = "")

        rval <- vector(mode = "list", length = length(x))
        for (i in 1:length(x))
            rval[[i]] <- list(pkg = x[i], url = url[i])
        rval
    }

    as.yaml(do.call("c", lapply(field, fun)))
}

R2yaml <- function(pkg) {

    writeLines(desc2yaml(pkg), con = "pkg.yml")
    writeLines(person2yaml(.authors(pkg)), con = "authors.yml")
    writeLines(person2yaml(.maintainer(pkg)), con = "maintainer.yml")
    writeLines(depends2yaml(pkg), con = "rdepends.yml")

}
