declare boundary-space preserve;
response-header("X-Count", 6+7),
response-header("Content-Type", "text/html"),
unescaped-data("<?xml version='1.0'?>
"),
<body>
  <p>The translated path was: {request-path-translated()}</p>,
  <p>{let $query := qexo:request-query-string() return
      if ($query)
      then ("The query string was: ",$query)
      else "There was no query string."}</p>
</body>
