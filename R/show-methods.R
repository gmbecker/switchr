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
              cat("\nPackages:\n")
              show(df)
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
