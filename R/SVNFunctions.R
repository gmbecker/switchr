updateSVN = function(dir, source,  param)
{
    oldwd = getwd()
    setwd(dir)
    on.exit(setwd(oldwd))
    usr = pwd = character()
    if(!is.na(source@user) && nchar(source@user))
        usr = paste0(" --username=", source@user)
    ## if(nchar(source@password))
    if(!is.na(source@password) && nchar(source@password))
        pwd = source@password
    
    if(is(source, "GitSVNSource"))
        {
            if(length(pwd) && nchar(pwd))
                pwd = paste("echo", pwd, "| ")
            cmd = paste(pwd,"git svn rebase", usr)
        } else {
            if(length(pwd) && nchar(pwd) )
                pwd = paste0("--password=", pwd)
            cmd = paste("svn update", usr, pwd)
        }
    out = tryCatch(system_w_init(cmd, intern=TRUE, param = param),
        error = function(x) x)
    if(is(out, "error"))
    {
        logfun(param)(basename(dir), "SVN update failed!", type = "both")
        return(FALSE)
    }
    
    TRUE
}

checkStdSVN = function(dir)
{
    dirs = list.dirs(dir, full.names=FALSE)
    dirs = dirs[!grepl("^\\.", dirs)]

    any(grepl("^trunk$", dirs))
}
