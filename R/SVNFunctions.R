updateSVN = function(dir, source,  param)
{
    oldwd = getwd()
    setwd(dir)
    on.exit(setwd(oldwd))
    usr = pwd = args = character()
    ## if(nchar(source@password))
    if(!is.na(source@password) && nchar(source@password))
        pwd = source@password

    if(!is.na(source@user) && nchar(source@user))
        usr = source@user
    else if(!is.na(pwd) && nchar(pwd))
        usr = system2("whoami", stdout = TRUE, stderr = TRUE)
    
    if(is(source, "GitSVNSource"))
        {
            if(length(pwd) && nchar(pwd))
                pwd = shQuote(paste("echo", pwd, "| "))
            cmd = paste(pwd,"git")
            args = c("svn rebase", usr)
        } else {
            args = c("update", args)
            cmd = "svn"
            if(length(pwd) && nchar(pwd) )
                args = c(args, paste0("--password=", pwd))
            if(length(usr) && nchar(usr))
                args = c(args, paste0("--username=", usr))
        }
   
    out = tryCatch(system_w_init(cmd, args = args, intern=TRUE, param = param),
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
