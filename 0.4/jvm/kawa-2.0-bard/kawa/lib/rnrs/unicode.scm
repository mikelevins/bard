(module-name (rnrs unicode))
(module-export char-upcase char-downcase char-titlecase char-foldcase
	       char-alphabetic? char-numeric? char-whitespace?
	       char-upper-case? char-lower-case? char-title-case?
	       char-general-category
	       string-upcase string-downcase string-titlecase string-foldcase
	       string-normalize-nfd string-normalize-nfkd
	       string-normalize-nfc string-normalize-nfkc)
(require <kawa.lib.characters>)
(require <kawa.lib.compile_misc>)

(define (char-upcase (ch :: character)) :: character
  (integer->char (java.lang.Character:toUpperCase (char->integer ch))))

(define (char-downcase (ch :: character)) :: character
  (integer->char (java.lang.Character:toLowerCase (char->integer ch))))

(define (char-titlecase (ch :: character)) :: character
  (integer->char (java.lang.Character:toTitleCase (char->integer ch))))

(define (char-alphabetic? (ch :: character)) :: boolean
  (java.lang.Character:isLetter (char->integer ch)))

(define (char-numeric? (ch :: character)) :: boolean
  (java.lang.Character:isDigit (char->integer ch)))

(define (char-whitespace? (ch :: character)) :: boolean
  (gnu.kawa.functions.UnicodeUtils:isWhitespace (char->integer ch)))

(define (char-upper-case? (ch :: character)) :: boolean
  (java.lang.Character:isUpperCase (char->integer ch)))

(define (char-lower-case? (ch :: character)) :: boolean
  (java.lang.Character:isLowerCase (char->integer ch)))

(define (char-title-case? (ch :: character)) :: boolean
  (java.lang.Character:isTitleCase (char->integer ch)))

(define (char-foldcase (ch :: character)) :: character
  (let ((val (char->integer ch)))
    (if (or (= val #x130) (= val #x131))
        ch
        (integer->char
         (java.lang.Character:toLowerCase
          (java.lang.Character:toUpperCase val))))))

(define (char-general-category (ch :: character)) :: symbol
  (gnu.kawa.functions.UnicodeUtils:generalCategory (char->integer ch)))

(define (string-upcase (str :: string)) :: string
  (gnu.lists.FString ((str:toString):toUpperCase java.util.Locale:ENGLISH)))

(define (string-downcase (str :: string)) :: string
  (gnu.lists.FString ((str:toString):toLowerCase java.util.Locale:ENGLISH)))

(define (string-titlecase (str :: string)) :: string
  (gnu.lists.FString (gnu.kawa.functions.UnicodeUtils:capitalize str)))

(define (string-foldcase (str :: string)) :: string
  (gnu.lists.FString (gnu.kawa.functions.UnicodeUtils:foldCase str)))

(define-syntax string-normalize
  (syntax-rules ()
    ((string-normalize str kind)
     (cond-expand (string-normalize-unicode
		   (try-catch
		    (java.text.Normalizer:normalize str (static-field java.text.Normalizer$Form 'kind))
		    (ex java.lang.NoClassDefFoundError
			(error "unicode string normalization not available"))))
		  (else (error "unicode string normalization not available"))))))

(define (string-normalize-nfd (str :: string)) :: string
  (string-normalize str NFD))

(define (string-normalize-nfkd (str :: string)) :: string
  (string-normalize str NFKD))

(define (string-normalize-nfc (str :: string)) :: string
  (string-normalize str NFC))

(define (string-normalize-nfkc (str :: string)) :: string
  (string-normalize str NFKC))
