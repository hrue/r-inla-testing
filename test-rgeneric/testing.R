f1 = function(ab) {
    print(paste("a", a))
    print(paste("b", b))
    print(paste("ab", ab))
    return (invisible())
}

wrapper = function(fun, init.file) {
    env = new.env()
    source(init.file, local = env)
    f= get(fun, envir = .GlobalEnv)
    environment(f) = env
    return (f)
}

init.file = "init.R"

f1.copy = f1
ff1 = f1

cat("a=1;b=2", file=init.file)
f1 = wrapper("f1", init.file)

cat("a=10;b=20", file=init.file)
f2 = wrapper("f1.copy", init.file)
    

    
