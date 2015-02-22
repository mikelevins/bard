(define-alias JAXBContext javax.xml.bind.JAXBContext)
(define-alias StringReader java.io.StringReader)
(define-alias XmlRegistry javax.xml.bind.annotation.XmlRegistry)
(define-alias XmlRootElement javax.xml.bind.annotation.XmlRootElement)
(define-alias XmlElement javax.xml.bind.annotation.XmlElement)
(define-alias XmlAttribute javax.xml.bind.annotation.XmlAttribute)
(define-alias BigDecimal java.math.BigDecimal)
(define-alias ArrayList java.util.ArrayList)
;; Enable stronger compile-time error-checking:
(module-compile-options warn-undefined-variable: #t
                        warn-unknown-member: #t
                        warn-as-error: #t)

;; See http://www.w3.org/TR/xquery-use-cases/#xmp-dtd

(define-simple-class Bib ( ) (@XmlRootElement name: "bib")
  (books (@XmlElement name: "book" type: Book) ::ArrayList))

(define-simple-class Book ()
  (year (@XmlAttribute required: #t) ::String)
  (title (@XmlElement) ::String)
  (authors (@XmlElement name: "author" type: Author) ::ArrayList)
  (editors (@XmlElement name: "editor" type: Editor) ::ArrayList)
  (publisher (@XmlElement) ::String)
  (price (@XmlElement) ::BigDecimal))

(define-simple-class Person ()
  (last (@XmlElement) ::String)
  (first (@XmlElement) ::String))

(define-simple-class Author (Person))

(define-simple-class Editor (Person)
  (affiliation (@XmlElement) ::String))

(define jaxb-context ::JAXBContext
  (JAXBContext:newInstance Bib Book Person Editor))
;; Alternatively use an ObjectFactory, as in the following.
;; (Replace "testsuite" by the name of the package this is compiled to.)
;(define jaxb-context ::JAXBContext (JAXBContext:newInstance "testsuite"))
;(define-simple-class ObjectFactory () (@XmlRegistry)
;  ((createBib) ::Bib (Bib)))

;; Read in an XML-formatted <bib> document.
(define (parse-xml (in ::java.io.Reader)) ::Bib
  ((jaxb-context:createUnmarshaller):unmarshal in))

;; Multiply old by ratio to yield an updated 2-decimal-digit BigDecimal.
(define (adjust-price old::BigDecimal ratio::double)::BigDecimal
  (BigDecimal (round (* (old:doubleValue) ratio 100)) 2))

;; Multiply the price of all the books in bb by ratio.
(define (adjust-prices (bb::Bib) (ratio::double))
  (let* ((books bb:books)
         (nbooks (books:size)))
    (do ((i :: int 0 (+ i 1))) ((>= i nbooks))
      (let* ((book ::Book (books i)))
        (set! book:price (adjust-price book:price ratio))))))

;; Write bb as pretty-printed XML to an output port.
(define (write-bib (bb ::Bib) (out ::output-port))::void
  (let ((m (jaxb-context:createMarshaller)))
    (fluid-let ((*print-xml-indent* 'pretty))
      ;; We could marshal directly to 'out' (which extends java.io.Writer).
      ;; However, XMLPrinter can pretty-print the output more readably.
      ;; We use XMLFilter as glue that implements org.xml.sax.ContentHandler.
      (m:marshal bb (gnu.xml.XMLFilter (gnu.xml.XMLPrinter out))))))

(define in (current-input-port))
(define bb (parse-xml in))
(adjust-prices bb 1.1)
(bb:books:add
 (Book year: "2006"
       title: "JavaScript: The Definitive Guide (5th edition)"
       authors: [(Author last: "Flanagan" first: "David")]
       publisher: "O'Reilly"
       price: (BigDecimal "49.99")))

(write-bib bb (current-output-port))

