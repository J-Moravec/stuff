all_identical = function(...){
    lst = list(...)
    n = length(lst)

    for(i in seq_len(n - 1)){
        for(j in seq(i + 1, n)){
            if(!identical(lst[[i]], lst[[j]])) return(FALSE)
            }
        }
    TRUE
    }


dir.remove = function(x, force = FALSE){
    unlink(x, recursive = TRUE, force = force)
    }
