parse_mat <- function(mat) {
    remove_space <- function(expr) {
        gsub("[ \t]+", "", expr)
    }
    extract_coef <- function(expr) {
        a <- strsplit(remove_space(expr), "\\*")[[1]]
        if (length(a) == 0) {
            return (0.0)
        } else if (length(a) == 1) {
            val <- as.numeric(a[1])
            if (is.na(val)) {
                return (1.0)
            } else {
                return (val)
            }
        } else {
            for(i in 1:2) {
                val <- as.numeric(a[i])
                if (!is.na(val))
                    return (val)
            }
            return (NA)
        }
    }
    extract_var <- function(expr) {
        a <- strsplit(remove_space(expr), "\\*")[[1]]
        if (length(a) == 0) {
            return ("")
        } else if (length(a) == 1) {
            val <- as.numeric(a[1])
            if (is.na(val)) {
                return (a[1])
            } else {
                return ("")
            }
        } else {
            for(i in 1:2) {
                val <- as.numeric(a[i])
                if (is.na(val))
                    return (a[i])
            }
            return ("")
        }
    }
    
    w = options('warn')$warn
    options(warn = -1)
    A = matrix(sapply(mat, extract_coef), nrow = nrow(mat))
    B = matrix(sapply(mat, extract_var), nrow = nrow(mat))
    options(warn = w)
    return (list(A = A, B = B))
}

mat <- matrix(c("a*2", "-3*b", ".2*x", "0.213 * d",
                "e * -2.34",  "   2.2 ",  "x", ""),
              nrow = 2, ncol = 4)
parse_mat(mat)
parse_mat(matrix(mat, 4, 2))
mat
