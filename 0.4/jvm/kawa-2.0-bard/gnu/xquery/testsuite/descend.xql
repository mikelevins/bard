declare function local:descendant-or-self ($x) {
  $x, for $z in children($x) return local:descendant-or-self($z) };
local:descendant-or-self (<a>text1<b>text2</b></a>)
