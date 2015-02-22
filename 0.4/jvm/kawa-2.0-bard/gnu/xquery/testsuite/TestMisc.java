package gnu.xquery.testsuite;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.kawa.io.CharArrayInPort;
import gnu.kawa.io.CharArrayOutPort;
import gnu.kawa.io.InPort;
import gnu.xquery.lang.*;
import gnu.text.*;

public class TestMisc
{
  static { XQuery.registerEnvironment(); }
  static XQuery interp = XQuery.getInstance();
  static Environment env = Environment.getCurrent();

  static int expectedPasses = 0;
  static int unexpectedPasses = 0;
  static int expectedFailures = 0;
  static int unexpectedFailures = 0;
  static boolean verbose = false;
  static String failureExpectedNext = null;

  public static void main(String[] args)
  {
      String srcdir = System.getProperty("srcdir");
      String srcprefix = srcdir == null ? "" : (srcdir+"/");

    // gnu.expr.ModuleExp.dumpZipPrefix = "kawa-zip-dump-";
    // Compilation.debugPrintExpr = true;
    // Compilation.debugPrintFinalExpr = true;

    evalTest("3.5+1", "4.5");
    evalTest("3.5+1 ,4*2.5", "4.5 10");
    evalTest("3<5", "true");
    evalTest("let $x:=3+4 return $x", "7");
    evalTest("let $x:=3+4 return <a>{$x}</a>", "<a>7</a>");
    // We resolve $request and $response to servlet request/response,
    // but only when they're not lexially bound.
    evalTest("let $request:=2, $response:=3 return ($request+$response)", "5");

    evalTest("some $x in (1, 2, 3), $y in (2, 3, 4)"
	     + " satisfies $x + $y = 4",
	     "true");
    evalTest("every $x in (1, 2, 3), $y in (2, 3, 4)"
	     + " satisfies $x + $y = 4",
	     "false");
    evalTest("every $x in (11, 12, 13), $y in (2, 3, 4)"
	     + " satisfies $x > $y",
	     "true");

    evalTest("for $y in (4,5,2+4) return <b>{10+$y}</b>",
	     "<b>14</b><b>15</b><b>16</b>");
    evalTest("for $i in (1 to 10) where ($i mod 2)=1 return 20+$i",
	     "21 23 25 27 29");
    evalTest("for $car at $i in ('Ford', 'Chevy')," +
	     "$pet at $j in ('Cat', 'Dog') " +
	     "return ($i, '/', $car, '/', $j, '/', $pet, ';')",
	     "1/Ford/1/Cat;1/Ford/2/Dog;2/Chevy/1/Cat;2/Chevy/2/Dog;");

    evalTest("(3,4,5)[3]", "5");
    evalTest("1,((2,3)[false()]),5", "1 5");
    evalTest("1,((2 to 4)[true()]),5", "1 2 3 4 5");
    evalTest("(for $y in (5,4) return <b>{10+$y}</b>)[2]", "<b>14</b>");
    evalTest("for $a in (<a><b c='1' d='3'/><b c='2' d='6'/></a>)/b/@c"
	     + " return concat('c: ', $a, ' d: ', $a/../@d, ';')",
	     "c: 1 d: 3;c: 2 d: 6;");

    String tabNsNodes = " xmlns:h=\"H\" xmlns:j=\"J\" xmlns:k=\"J\"";
    evalTest("doc('tab.xml')/result",
	     "<result"+tabNsNodes+">\n" +
	     "<row>\n" +
	     "<fld1>a1</fld1>\n" +
	     "<fld2 align=\"right\"><!--ignore-this-comment-->12</fld2>\n" +
	     "</row>\n" +
	     "<row>\n" +
	     "<fld1 align=\"left\">b1</fld1>\n" +
	     "<fld2 align=\"right\">22</fld2>\n" +
	     "</row>\n" +
	     "<h:row>\n" +
	     "<j:fld1><![CDATA[c]]><![CDATA[1]]></j:fld1>\n" +
	     "<h:fld2><![CDATA[33]]></h:fld2>\n" +
	     "<j:fld3>44</j:fld3>\n" +
	     "<k:fld1>c2</k:fld1>\n" +
	     "</h:row>\n" +
	     "</result>");
    evalTest("doc('tab.xml')/result/row/fld2",
	     "<fld2"+tabNsNodes+" align=\"right\"><!--ignore-this-comment-->12</fld2>"
	     +"<fld2"+tabNsNodes+" align=\"right\">22</fld2>");
    evalTest("doc('tab.xml')/result/row[fld2]",
	     "<row"+tabNsNodes+">\n" +
	     "<fld1>a1</fld1>\n" +
	     "<fld2 align=\"right\"><!--ignore-this-comment-->12</fld2>\n</row>" +
	     "<row"+tabNsNodes+">\n" +
	     "<fld1 align=\"left\">b1</fld1>\n" +
	     "<fld2 align=\"right\">22</fld2>\n" +
	     "</row>");
    evalTest("doc('tab.xml')/result/row/*",
	     "<fld1"+tabNsNodes+">a1</fld1><fld2"+tabNsNodes+" align=\"right\"><!--ignore-this-comment-->12</fld2><fld1"+tabNsNodes+" align=\"left\">b1</fld1><fld2"+tabNsNodes+" align=\"right\">22</fld2>");

    evalTest("doc('tab.xml')/result/row[2]",
	     "<row"+tabNsNodes+">\n" +
	     "<fld1 align=\"left\">b1</fld1>\n" +
	     "<fld2 align=\"right\">22</fld2>\n" +
	     "</row>");
    evalTest("for $x in doc('tab.xml')/result/row[2]/node()" +
	     "  return ('[',$x,']')",
	     "[\n][<fld1"+tabNsNodes+" align=\"left\">b1</fld1>][\n" +
	     "][<fld2"+tabNsNodes+" align=\"right\">22</fld2>][\n]");
    evalTest("for $x in doc('tab.xml')/result/row[2]/text()" +
	     "  return ('[',$x,']')",
	     "[\n][\n][\n]");
    evalTest("for $x in doc('tab.xml')/result/row[2]//text()" +
	     "  return ('[',$x,']')",
	     "[\n][b1][\n][22][\n]");
    evalTest("doc('tab.xml')/result/row/*[2]",
	     "<fld2"+tabNsNodes+" align=\"right\"><!--ignore-this-comment-->12</fld2>"
	     + "<fld2"+tabNsNodes+" align=\"right\">22</fld2>");

    evalTest("for $x in <T>r1<fld1>a1</fld1><fld3/>r2<fld2>12</fld2></T>" +
	     "  /node()" +
	     "    return ('[',$x,']')",
	     "[r1][<fld1>a1</fld1>][<fld3 />][r2][<fld2>12</fld2>]");

    evalTest("(doc('tab.xml')/result/row/*)[2]",
	     "<fld2"+tabNsNodes+" align=\"right\"><!--ignore-this-comment-->12</fld2>");
    evalTest("(doc('tab.xml')/result/row/*)[position()>1]",
	     "<fld2"+tabNsNodes+" align=\"right\"><!--ignore-this-comment-->12</fld2>"
	     +"<fld1"+tabNsNodes+" align=\"left\">b1</fld1>"
	     +"<fld2"+tabNsNodes+" align=\"right\">22</fld2>");
    evalTest("(doc('tab.xml')/result/row/*)[position()>1][2]",
	     "<fld1"+tabNsNodes+" align=\"left\">b1</fld1>");

    evalTest("doc('tab.xml')/result/row/(fld2,fld1)",
	     "<fld1"+tabNsNodes+">a1</fld1>"
	     +"<fld2"+tabNsNodes+" align=\"right\"><!--ignore-this-comment-->12</fld2>"
	     +"<fld1"+tabNsNodes+" align=\"left\">b1</fld1>"
	     +"<fld2"+tabNsNodes+" align=\"right\">22</fld2>");

    evalTest("string(doc('tab.xml'))",
	     "\n\na1\n12\n\n\nb1\n22\n\n\nc1\n33\n44\nc2\n\n\n");
    evalTest("string(doc('tab.xml'))",
	     "\n\na1\n12\n\n\nb1\n22\n\n\nc1\n33\n44\nc2\n\n\n");
    evalTest("string(doc('tab.xml')/result/row/fld1/@align)", "left");
    evalTest("doc('tab.xml')/result/row/fld2/@align/string()",
	     "rightright");
 
    evalTest("for $x in children(<a>xy{3+4}kl<c>def</c>{9}{11}</a>)" +
	     "  return ('[',$x,']')",
	     "[xy 7 kl][<c>def</c>][9 11]");
    evalTest("children(<a>xy{3+4}kl<c>def</c>{9}{11}</a>)",
	     "xy 7 kl<c>def</c>9 11");

    evalTest("<a>aab</a> ='aab'", "true");
    evalTest("<a>abc</a>='abb'", "false");

    evalTest("string(<a>{'aa''bb&#88;cc&#x5a;dd'}</a>)", "aa'bbXccZdd");

    evalTest("doc('tab.xml')/result/row[fld1]",
	     "<row"+tabNsNodes+">\n" +
	     "<fld1>a1</fld1>\n" +
	     "<fld2 align=\"right\"><!--ignore-this-comment-->12</fld2>\n</row>" +
	     "<row"+tabNsNodes+">\n" +
	     "<fld1 align=\"left\">b1</fld1>\n" +
	     "<fld2 align=\"right\">22</fld2>\n" +
	     "</row>");
    evalTest("doc('tab.xml')/result/row[fld3]", "");
    evalTest("doc('tab.xml')/result/row/fld1[@align]",
	     "<fld1"+tabNsNodes+" align=\"left\">b1</fld1>");
    evalTest("doc('tab.xml')/result/row/fld2[@align]",
	     "<fld2"+tabNsNodes+" align=\"right\"><!--ignore-this-comment-->12</fld2>"
	     +"<fld2"+tabNsNodes+" align=\"right\">22</fld2>");
    evalTest("'a',doc('tab.xml')/result/row/fld1[@align='left']",
	     "a<fld1"+tabNsNodes+" align=\"left\">b1</fld1>");
    evalTest("'a',doc('tab.xml')/result/row/fld1[@align='right']", "a");

    evalTest("let $x:=12,\n" +
	     "    $y:=<a>{$x+$x}</a>\n" +
	     "  return <b atr1='11' atr2=\"{$x}\">{($y,99,$y)}</b>",
	     "<b atr1=\"11\" atr2=\"12\"><a>24</a>99<a>24</a></b>");

    evalTest("let $el := 'elm' return "
	     + "document{element {$el} {attribute at{\"abc\"}, \"data\"}}/elm",
	     "<elm at=\"abc\">data</elm>");

    evalTest("let $a := <a at1='val1'><b/><c/></a>,"
	     + "  $b0 := <b/>,"
	     + "  $b := $a/b return"
	     + " ($a is $a, $a << $b, $b >> $b,"
	     + "  $a isnot $b, $b, $b0, $b is $b0)",
	     "true true false true <b /> <b /> false");
    evalTest("let $a := <a at1='val1'><b/><c/></a>,"
	     + " $b := $a/b, $c := $a/c return"
	     + " for $n in distinct-nodes(($c, $a/@at1, $a, $c, $b, $b, $c))"
	     + " return ('[', $n, ']')",
	     "[<a at1=\"val1\"><b /><c /></a>][ at1=\"val1\"][<b />][<c />]");

    // Boundary whitsapce (boundary-space) tests:
    evalTest("declare boundary-space preserve;\n"
	     + "for $n in (<a> <b/> {' x '} </a>)/node() return ($n,';')",
	     " ;<b/>;  x  ;");
    evalTest("declare boundary-space strip;\n"
	     + "for $n in (<a> <b/> {' x '} </a>)/node() return ($n,';')",
	     "<b/>; x ;");
    evalTest("declare boundary-space strip;\n"
	     + "for $n in (<a> x <b/> y<c/>&#x20;</a>)/node() return ($n,';')",
	     " x ;<b/>; y;<c/>; ;");
    evalTest("for $n in (<a> <b/> </a>)/node() return ($n,';')",
	     "<b/>;");
    evalTest("<a> {3}  {4}  </a>", "<a>34</a>");
    // This actually succeeds because evalTest ignores spaces.
    // failureExpectedNext = "fix space handling in constructors";
    evalTest("<a>{3,4}{5,6}</a>", "<a>3 45 6</a>");
    failureExpectedNext = "fix space handling in constructors";
    evalTest("let $x := <a>{3,4}{5,6}</a> return <b>{$x, $x}</b>",
	     "<ba><a>3 45 6</a><a>3 45 6</a></b>");
    evalTest("for $n in <a><?xq doit?>abc<![CDATA[<X>]]>d<!--a comment--></a>/node()"
	     + " return ($n,';')",
	     "<?xq doit?>;abc<![CDATA[<X>]]>d;<!--a comment-->;");
    evalTest("for $n in <a><?xq doit?>abc<![CDATA[<X>]]>d<!--a comment--></a>/node()"
	     + " return (string($n),';')",
	     "doit;abc&lt;X&gt;d;a comment;");
    evalTest("string(<a><?xq doit?>abc<![CDATA[<X>]]>d<!--a comment--></a>)",
	     "abc&lt;X&gt;d");

    // Simple namespace tests.
    evalTest("declare namespace xx='XXX';\n <xx:a>XX</xx:a>",
	     "<xx:a xmlns:xx=\"XXX\">XX</xx:a>");
    evalTest("declare namespace x1='XXX';\n declare namespace x2='XXX';\n"
	     + "(<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/x2:ab/string()",
	     "X1X2");
    evalTest("declare namespace x1='XXX';\n declare namespace x2='YYY';\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/x2:ab)",
	     "X2");
    evalTest("declare namespace x1='XXX';\n declare namespace x2='YYY';\n"
	     + "(<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/*/string()",
	     "X1X2");
    evalTest("declare namespace x1='XXX';\n declare namespace x2='YYY';\n"
	     + "(<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/*:*/string()",
	     "X1X2");
    evalTest("declare namespace x1='XXX';\n declare namespace x2='YYY';\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/x1:*)",
	     "X1");
    evalTest("declare namespace x1='XXX';\n declare namespace x2='YYY';\n"
	     + "(<top><x1:ab>X1</x1:ab><x2:ab>X2</x2:ab></top>)/*:ab/string()",
	     "X1X2");
    evalTest("declare namespace x1='XXX';\n declare namespace x2='YYY';\n"
	     + "string((<top><x1:ab>X1</x1:ab><x2:cd>X2</x2:cd></top>)/*:cd)",
	     "X2");
    evalTest("declare namespace h='H';\n"
	     + "string(doc('tab.xml')/result/h:row)",
	     "\nc1\n33\n44\nc2\n");
    evalTest("declare namespace xx='H';\n"
	     + "doc('tab.xml')/result/xx:row/string()",
	     "\nc1\n33\n44\nc2\n");
    evalTest("doc('tab.xml')/result/*:row/string()",
	     "\na1\n12\n\nb1\n22\n\nc1\n33\n44\nc2\n");
    evalTest("doc('tab.xml')/result/*:row/*:fld1/string()",
	     "a1b1c1c2");
    evalTest("declare namespace k='J';\n"
	     + "doc('tab.xml')/result/*:row/k:fld1/string()",
	     "c1c2");
    evalTest("declare namespace k='J';\n"
	     + "string(doc('tab.xml')/result/*:row[k:fld1])",
	     "\nc1\n33\n44\nc2\n");
    evalTest("declare namespace m1 = 'bb'; declare namespace m2 = 'cc';"
	     + "let $m1:x := 3 return let $m2:x := 4 return"
	     + "  <m2:a a:c='{$a:x}' xmlns:a='bb'>{ count($a:x) }</m2:a>",
	     "<m2:a xmlns:a=\"bb\" xmlns:m2=\"cc\" a:c=\"3\">1</m2:a>");

    evalTest("doc('tab.xml')/result/row[1]/descendant::*",
	     "<fld1"+tabNsNodes+">a1</fld1>"
	     +"<fld2"+tabNsNodes+" align=\"right\"><!--ignore-this-comment-->12</fld2>");
    evalTest("for $x in doc('tab.xml')/result/row[1]/descendant::node() return ($x,';')",
	     "\n;<fld1"+tabNsNodes+">a1</fld1>;a1;\n;"
	     + "<fld2"+tabNsNodes+" align=\"right\"><!--ignore-this-comment-->12</fld2>;<!--ignore-this-comment-->;12;\n;");
    evalTest("doc('tab.xml')/result/row[1]/descendant::text()",
	     "a112");
    evalTest("doc('tab.xml')/result/row[1]/descendant-or-self::*",
	     "<row"+tabNsNodes+"><fld1>a1</fld1>"
	     + "<fld2 align=\"right\"><!--ignore-this-comment-->12</fld2></row>"
	     + "<fld1"+tabNsNodes+">a1</fld1>"
	     + "<fld2"+tabNsNodes+" align=\"right\"><!--ignore-this-comment-->12</fld2>");

    evalTest("for $n in doc('tab.xml')/result/* return node-name($n)",
	     "row row h:row");

    evalTest("for $n in doc('tab.xml')/result/row/* "
	     + "return local-name-from-QName(node-name($n))",
	     "fld1 fld2 fld1 fld2");
    evalTest("declare namespace h='H';\n"
             +" for $n in doc('tab.xml')/result/*:row/* "
	     + "return (prefix-from-QName(node-name($n)),';')",
	     " ;  ;  ;  ; j ; h ; j ; k ;");
    evalTest("for $n in doc('tab.xml')/result/*:row/*:fld1 "
	     + "return <n>{namespace-uri-from-QName(node-name($n))}</n>",
	     "<n /><n /><n>J</n><n>J</n>");
    evalTest("for $n in doc('tab.xml')/result/*:row/*:fld1 return "
	     + "('[', for $p in ('', 'k', 'h') return"
	     + " (namespace-uri-for-prefix($p,$n),';'), ']')",
	     "[;J;H;][;J;H;][;J;H;][;J;H;]");

    // Based on bugs reported by Francois Leygues <vizawalou@wanadoo.fr>:
    evalTest("let $bx := <b x='xx'></b> return"
	     + " let $x := <a>{for $y in $bx return $y}</a>"
	     + "  return $x/b",
	     "<b x=\"xx\" />");
    evalTest("element r {let $y := <b x='1'/>"
	     + " let $x:=<a>{$y}</a> return $x/b/@x}",
	     "<r x=\"1\" />");
    evalTest("declare function local:x(){<a><b x='1'/><b x='2'/></a>};"
	     + " let $i := <a>{for $a in local:x()/b return $a}</a>  return $i/b/@x",
	     " x=\"1\" x=\"2\"");

    evalTest("declare function local:s(){ <a x='10'>{for $n in (<b x='2'/>) return ($n) }</a>};"
	     + " let $st := local:s()/b return ("
	     + " '[',$st/@x ,'] [',$st ,']')",
	     "[ x=\"2\"] [<b x=\"2\" />]");

    // Testcase from <Seshukumar_Adiraju@infosys.com>:
    evalTest("let $books := "
	     + "<books><book id='book1'/><book id='book2'/></books> "
	     + "for $book in $books/book return <p>{string($book/@id)}</p>",
	     "<p>book1</p><p>book2</p>");

    evalTest("for $n in children(<a>xx<b/>yy</a>) return $n instance of node()",
	     "true true true");
    evalTest("for $n in children(<a>xx<b/>yy</a>) return $n instance of text ( )",
	     "true false true");
    evalTest("for $n in children(<a>xx<b/>yy</a>) return $n instance of element(a)",
	     "false false false");
    evalTest("for $n in <a>xx<b/>yy</a>/node() return $n instance of element(b)",
	     "false true false");
    // FIXME: evalTest("<a>xx<b/>yy</a>/node() instance of node()", "false");
    evalTest("<a>xx<b/>yy</a>/node() instance of node()?", "false");
    evalTest("<a>xx<b/>yy</a>/node() instance of node()+", "true");
    evalTest("<a>xx<b/>yy</a>/node() instance of node()*", "true");
    evalTest("<a>xx<b/>yy</a>/node() instance of item()+", "true");
    evalTest("(3,4,5) instance of item()+", "true");
    evalTest("('a','b') instance of string+", "true");
    evalTest("(2,3) instance of string?", "false");
    evalTest("(2,3) instance of string+", "false");
    evalTest("() instance of string?", "true");
    evalTest("() instance of string+", "false");
    evalTest("() instance of string*", "true");
    evalTest("('2') instance of string?", "true");
    evalTest("('2') instance of string+", "true");
    evalTest("('2') instance of string*", "true");
    evalTest("('2','3') instance of string?", "false");
    evalTest("('2','3') instance of string+", "true");
    evalTest("('2','3') instance of string*", "true");

    // Simplification of
    // https://bugzilla.redhat.com/show_bug.cgi?id=370851
    evalTest("document {<A id='a'>10</A>}/A[attribute::node()='10']", "");

    evalTest("declare namespace Int='class:java.lang.Integer';\n"
	     + "Int:toHexString(266)", "10a");
    evalTest("declare namespace File='class:java.io.File';\n"
	     + "declare function local:make-file ($x as string) {File:new($x)};\n"
	     + "declare function local:parent ($x) {java.io.File:getParent($x)};\n"
	     + "local:parent(local:make-file('dir/mine.txt'))", "dir");
    evalTest("java.lang.Integer:toHexString(255)", "ff");

    // String functions
    evalTest("substring('motor car', 6)", "car");
    evalTest("substring('metadata', 4, 3)", "ada");
    // evalTest("substring('metadata', -INF, 3)", "met");

    evalTest("(1 to 20)[. mod 5 = 0]", "5 10 15 20");
    evalTest("(1 to 20)[. mod 5 ge 3]", "3 4 8 9 13 14 18 19");
    evalTest("1,(99 to 0),3", "1 3");
    evalTest("-10 to -2", "-10 -9 -8 -7 -6 -5 -4 -3 -2");

    String some_elements =
      "let $top := <top><a/><b/><c/><d/></top>,"
      + " $a:=$top/a, $b:=$top/b, $c:=$top/c, $d:=$top/d return ";
    evalNodeNames(some_elements+"($b, $a) union ($a, $b)", "a;b;");
    evalNodeNames(some_elements+"($b, $a) union ($b, $c)", "a;b;c;");
    evalNodeNames(some_elements+"($b, $a) intersect ($a, $b)", "a;b;");
    evalNodeNames(some_elements+"($b, $a) intersect ($b, $c)", "b;");
    evalNodeNames(some_elements+"($b, $a) except ($a, $b)", "");
    evalNodeNames(some_elements+"($b, $a) except ($b, $c)", "a;");
    evalNodeNames(some_elements+"($b, $a, $b, $d) intersect ($b, $d)", "b;d;");
    evalNodeNames(some_elements+"($b, $a, $b, $d) except ($b, $d)", "a;");
    evalNodeNames(some_elements+"($b, $a, $b, $d) except ()", "a;b;d;");

    // Check for catching errors:
    evalTest("+ +", "*** syntax error - <string>:1:4: missing expression [XPST0003]");

    evalTest("declare namespace x1='XXX",
	     "*** caught SyntaxException - <string>:1:22: "
	     + "unexpected end-of-file in string starting here [XPST0003]");

    evalTest("unescaped-data('<?--->'),let $x:=unescaped-data('an &amp;oslash;') return <b>{unescaped-data('<![CDATA[saw]]>')} {$x}</b>",
	     "<?---><b><![CDATA[saw]]> an &oslash;</b>");

    evalTest("doc('"+srcprefix+"latin1b.xml')", "<p>Hell ae:&#xE6; oe:&#xF8; aa:&#xE5;</p>");

    evalTestIdAttrs("doc('outline.xml')/book/part/chapter/ancestor::*",
		    "b1;P1;");
    evalTestIdAttrs("doc('outline.xml')/book/part/"
		    +"chapter/ancestor-or-self::node()",
		    ";b1;P1;c1;c2;");
    evalTestIdAttrs("doc('outline.xml')//"
		    +"section[@id='s1']/following-sibling::*",
		    "s2;s3;");
    evalTestIdAttrs("doc('outline.xml')//chapter/self::*",
		    "c1;c2;");
    evalTestIdAttrs("doc('outline.xml')//"
		    +"para[@id='p31']/preceding::*",
		    "s1;s11;s2;");
    evalTestIdAttrs("doc('outline.xml')//"
		    +"section[@id='s5']/preceding-sibling::*",
		    "s4;");
    evalTestIdAttrs("doc('outline.xml')//"
		    +"chapter[@id='c1']/following::*",
		    "c2;s4;s5;");
    evalTestIdAttrs("doc('outline.xml')//"
		    +"section[@id='s1']/(/book)",
		    "b1;");
    evalTestIdAttrs("doc('outline.xml')//"
		    +"section[@id='s1']/(//chapter)",
		    "c1;c2;");

    evalTest("declare namespace XQuery = 'class:gnu.xquery.lang.XQuery';"
	     + "XQuery:eval-with-focus(XQuery:getInstance(),"
	     + "  '<r pos=\"{position()}\">{.}</r>', (<b/>, 3))",
	     "<r pos=\"1\"><b /></r><r pos=\"2\">3</r>");
    evalTest("declare namespace XQuery = 'class:gnu.xquery.lang.XQuery';"
	     + "XQuery:eval-with-focus(XQuery:getInstance(),"
	     + "  '<r pos=\"{position()}\">{.}</r>', <b/>, 3, 4)",
	     "<r pos=\"3\"><b /></r>");

    Object r;
    String e = "<r pos='{position()}' size='{last()}'>{.}</r>";
    try
      {
	r = toString(interp.evalWithFocus(e, interp.eval("2,3,4")));
      }
    catch (Throwable ex)
      {
	r = ex;
      }
    matchTest(e, r,
	      "<r pos=\"1\" size=\"3\">2</r>"
	      + "<r pos=\"2\" size=\"3\">3</r>"
	      + "<r pos=\"3\" size=\"3\">4</r>");
    try
      {
	r = toString(interp.evalWithFocus(e, interp.eval("<b/>"), 4, 10));
      }
    catch (Throwable ex)
      {
	r = ex;
      }
    matchTest(e, r,
	      "<r pos=\"4\" size=\"10\"><b/></r>");

    printSummary();
  }

  public static boolean printSummary ()
  {
    System.out.println("# of expected passes      " + expectedPasses);
    if (expectedFailures > 0)
      System.out.println("# of expected failures    " + expectedFailures);
    if (unexpectedPasses > 0)
      System.out.println("# of unexpected passes    " + unexpectedPasses);
    if (unexpectedFailures > 0)
      System.out.println("# of unexpected failures  " + unexpectedFailures);
    return unexpectedFailures != 0;
  }

  /** True if the two string match, ignoring unquoted white-space. */
  public static boolean matches(String str1, String str2)
  {
    int i = 0;
    int j = 0;
    char quote = 0;
    for (;;)
      {
	char x, y;
	for (;;)
	  {
	    if (i >= str1.length())
	      {
		x = 0;
		break;
	      }
	    x = str1.charAt(i++);
	    if (quote != 0 || ! Character.isWhitespace(x))
	      break;
	  }
	for (;;)
	  {
	    if (j >= str2.length())
	      {
		y = 0;
		break;
	      }
	    y = str2.charAt(j++);
	    if (quote != 0 || ! Character.isWhitespace(y))
	      break;
	  }
	if (x != y)
	  return false;
	if (x == 0)
	  return true;
	if (x == '\'' || x == '\"')
	  {
	    if (quote == 0)
	      quote = x;
	    else if (x == quote)
	      quote = 0;
	  }
      }
  }

  private static void evalTestIdAttrs(String expr, String expected)
  {
    evalTest("for $x in (" + expr + ") return (string($x/@id),';')", expected);
  }

  private static void evalNodeNames(String expr, String expected)
  {
    evalTest("for $node in (" + expr + ") return concat(node-name($node),';')",
	     expected);
  }

  public static void evalTest(String expr, String expected)
  {
    Object result;
    try
      {
	result = eval(expr);
      }
    catch (Throwable ex)
      {
	result = ex;
      }
    matchTest(expr, result, expected);
  }

  public static String toString (Object value)
  {
    CharArrayOutPort wr = new CharArrayOutPort();
    gnu.xml.XMLPrinter xp = new gnu.xml.XMLPrinter(wr);
    xp.writeObject(value);
    xp.close();
    return wr.toString();
  }

  public static void matchTest(String expr, Object returned, String expected)
  {
    String result;
    Throwable throwable;
    if (returned instanceof Throwable)
      {
	if (returned instanceof WrappedException)
	  {
	    throwable = ((WrappedException) returned).getException();
	    if (throwable != null)
	      returned = throwable;
	  }
	throwable = (Throwable) returned;
        // throwable.printStackTrace();
	if (returned instanceof SyntaxException)
	  result = "*** caught SyntaxException - "
	    + ((SyntaxException) returned).getMessages().getErrors();
	else
	  result = "*** caught " + returned.getClass().getName() + " ***";
      }
    else
      {
	result = returned.toString();
	throwable = null;
      }

    boolean failureExpected = failureExpectedNext != null;
    if (matches(expected, result))
      {
	if (failureExpected)
	  unexpectedPasses++;
	else
	  expectedPasses++;
	if (verbose || failureExpected)
	  System.out.println((failureExpected ? "XPASS: \"" : "PASS: \"")
			     + expr + "\" evaluated to \"" + result + "\"");
      }
    else
      {
	if (failureExpectedNext != null)
	  expectedFailures++;
	else
	  unexpectedFailures++;
	if (verbose || ! failureExpected)
	  System.out.println((failureExpected ? "XFAIL: \"" : "FAIL: \"")
			     + expr + "\" evaluated to \"" + result
			     + "\" but expected \"" + expected + "\"");
	if (verbose && ! failureExpected && throwable != null)
	  throwable.printStackTrace(System.out);
      }
    failureExpectedNext = null;
  }

  public static String eval(String expr)
    throws Throwable
  {
    CharArrayOutPort out = new CharArrayOutPort();
    InPort in = new CharArrayInPort(expr);
    SourceMessages messages = new SourceMessages();

    Compilation comp = interp.parse(in, messages, Language.PARSE_IMMEDIATE);
    SourceError firstError = messages.getErrors();
    if (firstError != null)
      return "*** syntax error - " + firstError;

    CallContext ctx = CallContext.getInstance();
    gnu.lists.Consumer save = ctx.consumer;

    try
      {
	ctx.consumer = interp.getOutputConsumer(out);
	ModuleExp mod = comp.getModule();
	mod.setName("atInteractiveLevel");  // FIXME
	ModuleExp.evalModule(env, ctx, comp, null, null);
      }
    finally
      {
	ctx.consumer = save;
      }

    String result = new String(out.toCharArray());
    out.close();
    return result;
  }

}
