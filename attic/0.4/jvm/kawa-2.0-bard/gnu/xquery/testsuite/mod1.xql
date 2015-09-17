module namespace m = "http://xquery.gnu/testsuite/Mod1";

declare variable $m:vv := 3;

declare function local:add($x, $y) { $x+$y};

declare function m:twice($x) { local:add($x, $x) };
