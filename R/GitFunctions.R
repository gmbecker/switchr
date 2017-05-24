updateGit = function(dir, source, param)
{
    oldwd = setwd(dir)
    on.exit(setwd(oldwd))
    ##cmd = paste("git checkout", branch(source), "; git pull origin", branch(source))

    curb = gitCurrentBranch(param)

    dirty = gitWDIsDirty(param)
    stash = FALSE
    if(curb == branch(source)) {
        cmds = "git"
        args = list("fetch --all", sprintf("merge origin/%s", curb))
        if(dirty) {
            stash = TRUE
            system_w_init("git", args = "stash", intern = TRUE, param = param)
        }
    } else {
        if(dirty) {
            stop("Uncommitted changes in local checkout of a different branch.  Stash or commit these before continuing")
        }
        else {
            cmds = "git"
            args = list(c("fetch origin ", branch(source), ":", branch(source)),
                        c("checkout ", branch(source)))
            ## cmds = c(paste0("git fetch origin ", branch(source), ":", branch(source)), paste("git checkout ", branch(source)))
            ## args = list(character(), character())
        }
    }

    out = tryCatch(mapply(system_w_init, cmds, args = args, intern = TRUE,
                 param = list(param)), error= function(e) e)

#    out = tryCatch(system_w_init(cmds, stdout = TRUE, stderr = TRUE, param=param),
 #       error = function(x) x)
    if(errorOrNonZero(out))
    {

        logfun(param)(pkg = basename(dir), msg = paste("update of git checkout failed! cmd:", cmds),
                     type = "both")
        return(FALSE)
    }
    if(stash)
        system_w_init("git", args= "stash pop", intern = TRUE, param = param)


    TRUE
}

gitChangeBranch = function(codir, branch, param = SwitchrParam()) {

    oldwd = setwd(codir)
    on.exit(setwd(oldwd))
    logfun(param)(basename(codir), paste("GIT: Switching to branch", branch,
                                        "in checked out local copy."))
    out = tryCatch(system_w_init("git", args = c("checkout", branch),
                                 intern = TRUE, param = param),
        error = function(x) x)
    if(errorOrNonZero(out)) {
        logfun(param)(basename(codir), c("GIT: switching to branch failed."),
                     type = "both")
        logfun(param)(basename(codir), c("git error output:", out), type="error")
        FALSE
    } else {
        TRUE
    }
}

gitCurrentBranch = function(param) {
    res = system_w_init("git", args = "branch", intern = TRUE, param = param)
    br = res[grepl("\\*", res)]
    br = gsub("\\*[[:space:]]*", "", br)
    br
}

gitWDIsDirty = function(param) {
    prstat = system_w_init("git", args = c("status", "--porcelain"), intern = TRUE, param = param)
    if(length(prstat)  && !all(grepl("\\?\\?", prstat)))
        TRUE
    else
        FALSE

}
