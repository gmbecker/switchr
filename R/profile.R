setGeneric("lib_profile", function(x) standardGeneric("lib_profile"))

setMethod("lib_profile", "SwitchrCtx",
          function(x) {

              LibraryProfile(autoload = autoload_pkgs(x), script = autorun_script(x))
          })

setMethod("lib_profile", "missing",
          function(x) {
              lib_profile(currentCompEnv())
          })


setGeneric("autoload_pkgs", function(x) standardGeneric("autoload_pkgs"))
setMethod("autoload_pkgs", "SwitchrCtx",
          function(x) {
              dir = library_paths(x)[1]
              if(file.exists(file.path(dir, "autoloads.txt")))
                  autoloads = readLines(file.path(dir, "autoloads.txt"))
              else
                  autoloads = character()
              autoloads
          })

setMethod("autoload_pkgs", "missing",
          function(x) autoload_pkgs(currentCompEnv()))

setGeneric("autorun_script", function(x) standardGeneric("autorun_script"))
setMethod("autorun_script", "SwitchrCtx",
          function(x) {
              dir = library_paths(x)[1]
              if(file.exists(file.path(dir, "autorun.R")))
                  script = readLines(file.path(dir, "autorun.R"))
              else
                  script = character()
          })

setMethod("autorun_script", "missing",
          function(x) autorun_script(currentCompEnv()))
