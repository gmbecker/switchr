updateGit = function(dir, source, param)
{
    oldwd = getwd()
    on.exit(setwd(oldwd))
    setwd(dir)
    cmd = paste("git checkout", branch(source), "; git pull origin", branch(source))
    out = tryCatch(system_w_init(cmd, intern=TRUE, param=param),
        error = function(x) x)
    if(errorOrNonZero(out))
    {
        
        logfun(param)(pkg = basename(dir), msg = paste("git update failed! cmd:", cmd),
                     type = "both")
        return(FALSE)
    }

    TRUE
}


gitChangeBranch = function(codir, branch, param = SwitchrParam()) {
    oldwd = getwd()
    on.exit(setwd(oldwd))
    setwd(codir)
    cmd=paste("git checkout", branch)
    logfun(param)(basename(codir), paste("GIT: Switching to branch", branch,
                                        "in checked out local copy."))
    out = tryCatch(system_w_init(cmd, intern=TRUE, param = param),
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
