// exercise 11.2.1

u=(); U=(); UU=()-> ();
t = fix (\t:Nat->U. \n:Nat. 
    if iszero n 
        then (\x:U. x) u
        else (\f:UU. f(f(u))) (\x:U. t(pred n))
    );

t 2; 
(t 15; t 2);
