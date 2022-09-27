.domessage = function(msg) message(msg)
.onLoad = function(libname, pkgname) {
    ns = asNamespace(pkgname)
    ##graceful_inet(TRUE, TRUE)
    delayedAssign("BiocRelease", tryCatch(BiocVers(getBiocReleaseVr()),
                                          error = function(x) {
                                       NULL
                                 }),
                  eval.env = ns,
                  assign.env = ns)
    delayedAssign("develVers",  tryCatch(getBiocDevelVr(),
                                        error = function(x) {
                                   character()
                               }
                                   ), eval.env = ns,
                  assign.env = ns)

    delayedAssign("BiocDevel", tryCatch(BiocVers(getBiocDevelVr()),
                                        error = function(x) {
                                   NULL
                               }
                                   ), eval.env = ns,
                  assign.env = ns)
    delayedAssign("getBiocYaml",  tryCatch(doyamlsetup(),
                                        error = function(x) {
                                   NULL
                               }
                                   ), eval.env = ns,
                  assign.env = ns)

    delayedAssign("defaultBiocRepos", tryCatch(getBiocReposFromRVers(),
                                        error = function(x) {
                                   character()
                               }
                                   ), eval.env = ns,
                  assign.env = ns)


    namespaceExport(ns, c("BiocRelease", "BiocDevel"))
}


