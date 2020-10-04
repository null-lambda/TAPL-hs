/* Examples for testing */

 x:(Bool -> Bool) -> Bool -> Bool;
 a: Bool;

 lambda x:Bool. x;
 (lambda x:Bool->Bool. if x false then true else false)
   (lambda x:Bool. if x then false else true);

\x:Bool. 
  if x then 
    if false 
      then \ x:Bool y:Bool. x 
    else \ x:Bool y:Bool.y
  else \ a:Bool b:Bool. true;
    

(\m:Bool->Bool n:Bool->Bool f:Bool.m (n f));
(\b:Bool->Bool e:(Bool->Bool) ->Bool . e b);

true false;
if true then (\x:Bool. x) else false;

(\x:Bool -> Bool. x x);
(\x:Bool -> Bool. x x) (\x:Bool -> Bool. x x);