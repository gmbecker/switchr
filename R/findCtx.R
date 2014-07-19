##' @export
findCompEnv = function(url, name, rvers) {
    if(missing(url) && missing(name))
        stop("Must specify either a url or a name for the desired context")

   
    man = switchrManifest()
    if(!is.null(rvers))
        man = man[man$rversion == rvers,]
    
    i = which(url == man$url)
    if(!length(i))
        i = which(url == man$name)

    if(!length(i))
        return(NULL)
    else {
        manrow = man[i,]
        return(RComputingEnv(name = manrow$name, src_url = manrow$url, libpaths = strsplit(manrow$paths, ";")[[1]], exclude.site = manrow$excl.site))
    }
                
        
}
            


switchrBaseDir = function(value) {
    if(missing(value))
        if(is.null(switchrOpts$basedir)) "~/.switchr" else switchrOpts$basedir
    else {
        if(!file.exists(value))
            dir.create(value, recursive=TRUE)
        switchrOpts$basedir = value
    }
}

switchrManifest = function() {
    dir = switchrBaseDir()
    manfile = file.path(dir, "manifest.dat")
    
    if(!file.exists(manfile))
        data.frame(url = character(), name = character(), libpaths = character(), stringsAsFactors = FALSE, rversion = character())
    else
        read.table(file.path(dir, "manifest.dat"), header=TRUE, stringsAsFactors=FALSE)
}

updateManifest = function() {
    fils = list.files(switchrBaseDir(), recursive = TRUE, full.names = TRUE, pattern = "lib_info")
    man = do.call(rbind.data.frame, lapply(fils, function(x) as.data.frame(read.dcf(x))))
    write.table(man, file = file.path(switchrBaseDir(), "manifest.dat"))
}
