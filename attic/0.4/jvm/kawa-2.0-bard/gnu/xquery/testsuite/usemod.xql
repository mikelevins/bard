import module namespace M1 = "http://xquery.gnu/testsuite/Mod1";
import module namespace M2 = "gnu.xquery.testsuite.Mod2";
declare variable $one := 1;

($M1:vv+$one),M2:twice(M1:twice($M2:vv)+$M2:twelve)
