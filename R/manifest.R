
                  


emptyManifest = data.frame(name = character(),
    url = character(),
    type = character(),
    branch = character(),
    subdir = character(),

    extra = character(),
    stringsAsFactors = FALSE
    )
    

##'ManifestRow
##'
##' Create one or more rows of a manifest data.frame
##'
##' @param name name of the package. 
##' @param url location of the package sources
##' @param type type of location (svn, git, local, etc)
##' @param branch name of the branch to use to build the package
##' @param subdir subdirectory to use to build the package
##' @param extra currently ignored. extra commands for building or
##' installing the package
##' @details If name is missing, an empty (0 row) manifest data.frame
##' is returned. All other fields default to values indicating no information-
##'  \code{NA_character} in most cases, and \code{"."} for \code{subdir}
##' @return A valid Package manifest data.frame
##' @export
ManifestRow = function(name,
    url = NA_character_,
    type = NA_character_,
    branch = NA_character_,
    subdir = ".",
    extra = NA_character_
    ) {

    if(missing(name) || length(name) == 0)
        return(emptyManifest)
    if( is.na(type) && !is.na(url))
        type = .inferType(url)
    if(is.na(branch) && !is.na(type))
        branch = .inferDefaultBranch(branch, type)
    data.frame(name = name, url = url, type = type,
           branch = branch, subdir = subdir, extra = extra,
           stringsAsFactors = FALSE)
}




##' Manifest constructor
##' 
##' Create a package manifest
##' @param ... Vectors containing package information. Passed to \code{\link{ManifestRow}}
##' @param dep_repos The dependency repos for the package.
##' @export
makeManifest = function(..., dep_repos = defaultRepos()) {
    rows = mapply(ManifestRow, ..., SIMPLIFY=FALSE)
    maniman = do.call(rbind.data.frame, rows)
    maniman$url = ifelse(maniman$type == "local",
        normalizePath2(maniman$url), maniman$url)
    PkgManifest(manifest = do.call(rbind.data.frame, rows), dep_repos = dep_repos)
}

##XXX can't specify non-defaults in a lot of the columns


##' GithubManifest
##'
##' Create a package manifest containing only github packages
##'
##' @param ... Combined to populate \code{pkgrepos}
##' @param pkgrepos Github repositories in the form "<user>/<reponame>"
##' @details Any names of the pkgrepos vector are assumed to be
##' pkg names for the manifest. For unnamed elements, the pkg
##' name is assumed to be the repository name.
##' @note This is a convenience wrapper for \code{\link{makeManifest}}.
##' It uses the \code{username/repo[/subdir][@@ref]} shorthand for specifying
##' package locations in github repositories introduced by Wickham's
##' devtools. Unlike devtools, username is not optional, and  only branch
##' names are currently supported in the \code{@@ref}
##'
##' @examples
##' ghman = GithubManifest("gmbecker/switchr", "hadley/devtools")
##' ghman
##'
##' 
##' @export
GithubManifest = function( ..., pkgrepos) {

    if(missing(pkgrepos)) {
        pkgrepos = as.character(list(...))
        names(pkgrepos) = names(list(...))
    }
   ## nms = gsub("[^/]*/([^/]*)/*$", "\\1", pkgrepos)
  ##  nms = gsub("\\.git", "", nms)
    args = lapply(pkgrepos, parse_git_repo)
    nms = vec_get(args, "repo")
    vecnms = names(pkgrepos)
    if(!is.null(vecnms)) {
        inds = nchar(vecnms) > 0
        nms[inds] = vecnms[inds]
    }
        
    res = makeManifest(url = sapply(args, const_git_url), subdir = vec_get(args, "subdir"),
             type = "git", branch = vec_get(args, "ref"), name = nms)
    as(res, "GithubPkgManifest")
}
 


        
gitregex = "^(git:.*|http{0,1}://(www.){0,1}(github|bitbucket)\\.com.*|.*\\.git)$"

vec_get = function(lst, ind)
    sapply(lst, function(x) x[[ind]])



.inferType = function(urls) {
    types = character(length(urls))
    gits = grep(gitregex, urls)
    types[gits] = "git"
    types
}

.inferDefaultBranch= function(branch, type) {
    switch(type,
           git = "master",
           svn = "trunk",
           NA_character_)
}
           

const_git_url = function(args) {
    ret = "http://github.com/"
    ret = paste0(ret, args$user, "/", args$repo)
    ret
}




## Copyright Hadley Wickham. Lifted from devtools. Used under
## GPL 2.
# Parse concise git repo specification: [username/]repo[/subdir][#pull|@ref|@*release]
# (the *release suffix represents the latest release)

##We don't support the release or pull-request aspects of install_github currently.
## Refs are assumed to be branch names
##' @importFrom stats setNames
parse_git_repo <- function(path) {
    username_rx <- "(?:([^/]+)/)?"
    repo_rx <- "([^/@#]+)"
    subdir_rx <- "(?:/([^@#]*[^@#/]))?"
    ref_rx <- "(?:@([^*].*))"
    pull_rx <- "(?:#([0-9]+))"
        release_rx <- "(?:@([*]release))"
    ref_or_pull_or_release_rx <- sprintf("(?:%s|%s|%s)?", ref_rx, pull_rx, release_rx)
    github_rx <- sprintf("^(?:%s%s%s%s|(.*))$",
                         username_rx, repo_rx, subdir_rx, ref_or_pull_or_release_rx)
    param_names <- c("username", "repo", "subdir", "ref", "pull", "release", "invalid")
    replace <- setNames(sprintf("\\%d", seq_along(param_names)), param_names)
    params <- lapply(replace, function(r) gsub(github_rx, r, path, perl = TRUE))
    if (params$invalid != "")
        stop(sprintf("Invalid git repo: %s", path))
    if(!nchar(params$ref))
        params$ref = "master"
    if(!nchar(params$subdir))
        params$subdir = "."

    params <- params[sapply(params, nchar) > 0]
  
    params
}

