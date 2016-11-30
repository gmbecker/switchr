.onLoad = function(libname, pkgname) {
    ns = asNamespace(pkgname)

    delayedAssign("BiocRelease", tryCatch(BiocVers(getBiocReleaseVr()),
                                          error = function(x)
                                     warning("Unable to populate the BiocDevel object")),
                  eval.env = ns,
                  assign.env = ns)
    delayedAssign("BiocDevel", tryCatch(BiocVers(getBiocDevelVr()),
                                          error = function(x)
                                     warning("Unable to populate the BiocDevel object")), eval.env = ns,
                  assign.env = ns)

    namespaceExport(ns, c("BiocRelease", "BiocDevel"))
}
