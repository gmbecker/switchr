## copied from tools:::.extract_dependency_package_names to avoid check NOTE.
## seems pretty simple and safe

extract_dep_pkg_names = function (x) 
{
    if (is.na(x)) 
        return(character())
    x <- unlist(strsplit(x, ",[[:space:]]*"))
    x <- sub("[[:space:]]*([[:alnum:].]+).*", "\\1", x)
    x[nzchar(x) & (x != "R")]
}
