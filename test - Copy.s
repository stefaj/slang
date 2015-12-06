let fib a = {if a==1 
    then 1 
    else 
    {
        if a==2 then 1 else {fib (a-1) + fib (a-2)}}}
fib (10)
