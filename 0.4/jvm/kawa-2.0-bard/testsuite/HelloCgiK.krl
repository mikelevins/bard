[(response-content-type "text/xml")
]<p>The request URL was: [(request-url)]</p>
<p>[(let ((query (request-query-string)))
    (if query
      (begin ]The query string was: [query)
      ]There was no query string.[))]</p>
