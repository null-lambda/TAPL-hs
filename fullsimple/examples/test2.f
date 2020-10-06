/* test 2. sum and product type */

lambda x:<a:Bool,b:Bool>. x;


{x=true, y=false};
{x=true, y=false}.x;
{true, false};
{true, false}.1;
({{"00","11"},{"22","33"}}.2 as {String, String}).1;

PhysicalAddr = {firstlast:String, addr:String};
VirtualAddr= {name:String, email:String};
Addr = <physical:PhysicalAddr, virtual:VirtualAddr>;
pa = {firstlast="Mike", addr="Unknown"}: PhysicalAddr;
a = <physical= pa> as Addr;

getName = \a:Addr.
case a of
	<physical=x> => x.firstlast
	| <virtual=y> => y.name;
    a;
{hell= a, c=getName a};
getName a;