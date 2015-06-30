library(switchr)

man = BiocSVNManifest()

## use whatever permanent directory you want to place checkouts in.
## You can then work on the working copies within that directory and
## rebuilding the lazyRepo will reflect those changes.
repo = lazyRepo("rtracklayer", man, dir = "~/localcheckoutest", verbose=TRUE)



