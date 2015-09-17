(module-name (scheme char))
(require kawa.lib.characters)
(require kawa.lib.strings)
(require kawa.lib.rnrs.unicode)
(export
 char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
 char-downcase char-foldcase char-lower-case? char-numeric?
 char-upcase char-upper-case? char-whitespace?
 digit-value
 string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
 string-downcase string-foldcase string-upcase)

