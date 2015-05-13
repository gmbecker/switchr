##' Parse text output from printing SessionInfo objects
##' @param string The text output from sessionInfo()
##' @export
parseSessionInfoString = function(string)
{
    Rvers = getRVersion(string)
    Platform = getPlatform(string)
    ##locales are hard :(
    ##locale = getLocale(string)
    attached = getPkgs(string, "other attached packages:")
    loaded = getPkgs(string, "loaded via a namespace")
    new("parsedSessionInfo",version = Rvers, platform = Platform,
        attached = attached, loaded = loaded)
}

getRVersion = function(string)
{
    gsub("R version ([^ ]*).*", "\\1", string[1])
}

getPlatform = function(string)
{
    pformstuff = gsub("Platform: ([^ ]*).*", "\\1", string[2])
    pformstuff
}

getPkgs = function(string, pattern = "other attached packages:")
{
    start = grep(pattern, string)
    if(!length(start))
        return(data.frame())
    ##what we care about starts AFTER the other attacehd pkgs line
    start = start + 1
    end = min(c( grep("^$", string[start:length(string)]),
        grep(":", string[start:length(string)]),
        length(string) - start))
    end = end[1] - 1
    ##end is relative to start!!!
    string = string[start + 0:end]
    stuff = strsplit(paste(string, collapse = " "), split = "[[:space:]]" )[[1]]
    stuff = stuff[grep("_", stuff)]
    pkgnames = gsub("([^_]*)_.*", "\\1", stuff)
    versions = gsub("[^_]*_(.*)", "\\1", stuff)

    ret = mapply(function(x,y) data.frame(Package = x, Version = y, stringsAsFactors = FALSE), x = pkgnames, y = versions, SIMPLIFY=FALSE)
    ret = do.call(rbind.data.frame, ret)
    ##    names(ret) = pkgnames
    ret
}

