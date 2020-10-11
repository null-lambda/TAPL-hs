
// base class
CounterRep = {x: Ref Nat};
Counter = {get:()->Nat, inc:()->()};
SetCounter = {get:()->Nat, set:Nat->(), inc:()->()};
setCounterClass = \r:CounterRep. 
  fix 
    (\self:SetCounter.
      { get = \_:(). !r.x
      , set = \i:Nat. r.x:=i
      , inc = \_:(). self.set (succ(self.get ()))
      } as SetCounter 
    );

inc3 = λc:Counter. (c.inc (); c.inc (); c.inc ());

c = setCounterClass {x=ref 1};
{ c0=c.get (), 
  c1=(inc3 c; c.get ())
};