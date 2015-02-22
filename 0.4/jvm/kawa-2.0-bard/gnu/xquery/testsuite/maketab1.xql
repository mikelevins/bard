declare boundary-space preserve;
let $newline := "
",
$result := (doc("tab.xml")/result)
  return
    (<table>
{for $x in ($result/row)
      return (<tr>{
        for $y in ($x/fld1) return (<td><b>{$y/child::node()}</b></td>),
        for $y in ($x/fld2) return (<td>{(100,$y/child::node())}</td>)}</tr>,
        $newline)
}</table>,$newline)
