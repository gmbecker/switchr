
setMethod("show", "SwitchrCtx", function(object) {
    cat(paste(sprintf("A SwitchrCtx object defining the '%s' computing environment", object@name),
              "\n\n\t", sprintf("Primary library location(s): %s", paste(object@libpaths, collapse=";")),
              "\n\t", sprintf("Packages: %d packages installed in %d directories (including R's base library)",
                              nrow(object@packages),
                              length(c(unique(object@packages$LibPath, .Library)))),
              "\n\t", paste("This environment DOES ", ifelse(object@exclude.site, "NOT ", ""), "combine with the current site library location when loaded.", sep=""),
              "\n\n"))
})



setMethod("show", "PkgManifest",
          function(object) {
              cat("A package manifest (PkgManifest object)\n\n")
              df = manifest_df(object)[,c("name", "type")]
              rownames(df) = NULL
              cat(sprintf("Contains %d packages and %d dependency repositories\n",
                  nrow(df), length(dep_repos(object))))
              if(nrow(df) > 8) {
                  df = .sketch_df(df, 4, 4)
                  
              }
              if(nrow(df)) {
                  cat("\nPackages:\n")
                  show(df)
              }
          })


setMethod("show", "SessionManifest",
          function(object) {
              cat("A seeding manifest (SessionManifest object)\n\n")
              df = versions_df(object)[,c("name", "version")]
              rownames(df) = NULL
              cat(sprintf("Describes a cohort of %d package versions. \n%d packages are listed in the underlying package manifest\n",
                          nrow(df), nrow(manifest_df(object, session_only=FALSE))))
              if(nrow(df) > 8) {
                  df = .sketch_df(df, 4, 4)
                  
              }
              if(nrow(df)) {
                  cat("\nPackage versions:\n")
                  show(df)
              }
          })




## Lifted from (old) IRanges
.sketch_df= function (x, nhead, ntail, rownames = FALSE) 
{

    
    len <- nrow(x)

    p1 <- ifelse(nhead == 0, 0L, 1L)
    p2 <- ifelse(ntail == 0, 0L, ntail - 1L)
    if (nhead > 0) {
        df1 = as.matrix(x[1:nhead,])
    }
    if (ntail > 0) 
        df2 = as.matrix(x[(len - ntail):len,])

    middle = matrix(rep("...", times = length(names(x))), nrow=1)
    rownames(middle) = "..."
    ret = rbind(df1, middle, df2)
    
}
