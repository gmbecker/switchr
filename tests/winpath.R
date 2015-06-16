
checkUrlRoundtrip = function(pth) {
    pth = switchr:::normalizePath2(pth)
    furl = switchr:::makeFileURL(pth)
    pth2 = switchr:::fileFromFileURL(furl)
    if(pth != pth2)
        stop("Round trip result (", pth2, ") does not match original (", pth, ") when creating file URLs")
}

checkUrlRoundtrip(getwd())
checkUrlRoundtrip(tempdir())
if(switchr:::isWindows())
    checkUrlRoundtrip("\\\\laptop\\My Documents\\")

        
