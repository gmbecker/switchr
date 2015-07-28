##' Create a checkout of a package and all it's dependencies from a manifest
##'
##' @param pkgs character - The packages you will be working on
##' @param pkg_manifest Pkgmanifest|SessionManifest - The manifest containing the pkgs and dependencies to checkout
##' @param dir character - The directory in which to place the checkouts of packages
##' @param get_suggests character - Should 'Suggests' dependencies be retrieved? Options are "none" (never), "first" (for packages in \code{pkgs} but not for dependencies, or "all" (always).
##' @param param SwitchrParam - The SwitchrParam to use during the checkout process,
##' @param scm_auths list -  A named list of user-password pairs to use during the checkout process
##' @param repos  character - The package repositories to retrieve dependnecy information from for pkgs/dependnecies which do not appear in \code{pkg_manifest}
##' @return a character vector of all packages (incl. recursive dependnecies) checked out into \code{dir}
##' @export
makePkgCheckout = function(pkgs, pkg_manifest, dir, get_suggests = c("none", "first", "all"), param = SwitchrParam(),
    scm_auths = list(bioconductor=c("readonly", "readonly")),
                     repos = defaultRepos()) {
    get_suggests = match.arg(get_suggests)
    if(get_suggests != "none")
        suggests = TRUE
    avl = available.packages(contrib.url(repos))
    avl = avl[!avl[,"Package"] %in% manifest_df(pkg_manifest)$name,]
    pkgsneeded = pkgs
    pkgscheckedout = character()
    i = 1
    ## two stop conditions: no needed packages left, or only needed packages are not in manifest (error)
    while(i<= length(pkgsneeded)) {
        pkg = pkgsneeded[1]

        if(pkg %in% manifest_df(pkg_manifest)$name) {
            src = sourceFromManifest(pkg, pkg_manifest)
            res = makePkgDir(pkg, src, dir)
            pkgscheckouted = c(pkgscheckedout, pkg)
        }

        pkgsneeded = c(pkgsneeded, .getDepsThing(pkg, dir, src, avl, pkg_manifest,
            suggests = suggests, exclude = pkgsneeded, param = param))
        i = i + 1
        if(suggests && get_suggests == "first" && i > length(pkgs))
            suggests = FALSE
        
    }
    pkgscheckedout
}


.getDepsThing = function(pkg, dir, src, avail, man, suggests = FALSE, exclude = basepkgs, param) {
    mandf = manifest_df(man)
    if(pkg %in% mandf$name) {
        pkgdir = findPkgDir(file.path(dir, pkg), branch(src), subdir(src), param)
        desc = as.data.frame(read.dcf(file.path(pkgdir, "DESCRIPTION")))
        deps = .grabdeps(desc, suggests)
    } else if(pkg %in% avail[,"Package"])  {
        deps = .grabdeps(avail[pkg,,drop=FALSE], suggests)
    } else {
        stop("Unable to find packgae information (dependencies) for package ", pkg)
    }
    unique(setdiff(deps, exclude))
    
}

.grabdeps = function(desc, suggests) {
    if(!is(desc, "data.frame"))
        desc = as.data.frame(desc, stringsAsFactors=FALSE)
    depcols = c("Imports", "Depends", "LinkingTo")
    if(suggests)
        depcols = c(depcols, "Suggests")

   res = unique(unlist(lapply(depcols, function(col) {
                             if(col %in% names(desc) && !is.na(desc[[col]]))
                                 strsplit(desc[[col]], "[[:space:]]*,[[:space:]]*")[[1]]
                             else
                                 character()
                         })))
}
    

        
