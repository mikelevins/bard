(define as-xml (make <gnu.kawa.xml.OutputAsXML>))

(define-alias comment gnu.kawa.xml.KComment)

; http://download.plt-scheme.org/doc/html/xml/index.html#%28def._%28%28lib._xml/main..ss%29._element-name%29%29 has these:
; element-name
; element-attributes
; element-content
; atribute-name
; atribute-value
; p-i-target-name
; p-i-instruction

(define-alias processing-instruction gnu.kawa.xml.KProcessingInstruction)

;;; Parse an XML file (specified by a URL or url string), giving a <document>.
(define (parse-xml-from-url url) :: document
  (gnu.kawa.xml.Document:parse url))

(define (element-name element::gnu.kawa.xml.KElement) ::symbol
  (element:getNodeSymbol))

(define (attribute-name attr::gnu.kawa.xml.KAttr) ::symbol
  (attr:getNodeSymbol))

#|
(define (parse-nsxml-from-url url) :: <document>
  (if (not (instance? url <java.net.URL>))
      (set! url (make <java.net.URL> (invoke url 'toString))))
  (let* ((doc :: <document> (make <document>))
	 (parser :: <gnu.xml.XMLParser>
		 (make <gnu.xml.XMLParser> url
		       (make <gnu.xml.ParsedXMLToConsumer>
			 (make <gnu.xml.NamespaceResolver> doc)))))
    (invoke parser 'parse)
    doc))

(define (print-nsxml-from-url url #!optional (out (current-output-port)))
  (if (not (instance? url <java.net.URL>))
      (set! url (make <java.net.URL> (invoke url 'toString))))
  (let ((parser :: <gnu.xml.XMLParser>
		(make <gnu.xml.XMLParser> url
		      (make <gnu.xml.ParsedXMLToConsumer>
			(make <gnu.xml.NamespaceResolver>
			  (make <gnu.xml.XMLPrinter> out))))))
    (invoke parser 'parse)))
|#
