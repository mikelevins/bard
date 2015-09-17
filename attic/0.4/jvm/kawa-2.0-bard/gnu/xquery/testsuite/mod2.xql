module namespace m = "gnu.xquery.testsuite.Mod2";

declare variable $m:vv := 10;

declare function local:add($x, $y) { $x,$y};

declare function m:twice($x) { local:add($x, $x) };
