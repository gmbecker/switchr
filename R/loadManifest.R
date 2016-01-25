##' loadManifest
##'
##' Load a package or session manifest from a file (local or URL)
##'
##' @param fil The path or URL to the file or a gist containing it
##' @return A PkgManifest or SessionManifest object
##' @export
loadManifest = function(fil) {
    if(is(fil, "character") && url.exists(fil)) {

            
        newfil = tempfile(pattern = "manifest")
        download.file2(fil, newfil)
        if(grepl("gist.github.com", fil)) {
            lnk = grep(".*raw.*manifest\\.rman", readLines(newfil),
                value = TRUE)
            lnk = paste("https://gist.github.com",
                gsub("[^/]*(/.*/manifest\\.rman).*",
                     "\\1", lnk, ignore.case=TRUE),
                sep="")
            newfil = tempfile(pattern = "manifest")
            download.file2(lnk, newfil)
        }
            
        fil = newfil
    }
    txt = readLines(fil)
    pat = "^#"
    headerInds = grep(pat, txt)
    header = txt[headerInds]
    body = txt[-headerInds]
    depRepos = gsub(".*repo: (.*)", "\\1", header[grep("repo:", header)])
    colcl = sapply(ManifestRow(), class)
    if(any(grepl("# Manifest type: session", header)))
        colcl= c(colcl, version="character")
    
    df = read.table(file = textConnection(body, "r"),
        header = TRUE, sep = ",", stringsAsFactors=FALSE, comment.char="#",
                    colClasses =  colcl,
                    row.names=NULL)
    if("version" %in% names(df)) {
        sess = TRUE
        vdf = df[!is.na(df$version), c("name", "version")]
        df = df[, -which(names(df) == "version")]
    } else {
        sess = FALSE
    }

    man = PkgManifest(manifest =df, dep_repos = depRepos)
    if(sess)
        man = SessionManifest(man, vdf)
    man
}
