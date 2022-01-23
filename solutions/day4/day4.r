library(openssl)

exit <- function() {
    invokeRestart("abort")
}

secret_key <- "bgvyzdsv"

for (i in 0:9999999) {
    pasted <- paste(secret_key, toString(i), sep="")
    hash <- md5(pasted)

    first_five <- substr(hash, 1, 5)
    if (first_five == "00000") {
        print(i)
        exit()
    }
}

for (i in 0:99999999) {
    pasted <- paste(secret_key, toString(i), sep="")
    hash <- md5(pasted)

    first_six <- substr(hash, 1, 6)
    if (first_six == "000000") {
        print(i)
        exit()
    }
}
