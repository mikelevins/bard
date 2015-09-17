(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.reflection>)
(require <kawa.lib.syntax>)
(require <kawa.lib.ports>)

(define (path? path) :: <boolean>
  (instance? path gnu.kawa.io.Path))
(define (filepath? path) :: <boolean>
  (instance? path gnu.kawa.io.FilePath))
(define (URI? path) :: <boolean>
  (instance? path gnu.kawa.io.URIPath))
(define (absolute-path? (path :: path)) :: <boolean>
  (path:isAbsolute))
(define (path-scheme (p :: path))
  (let ((s (p:getScheme)))
    (if (eq? s #!null) #f s)))
(define (path-authority (p :: path))
  (let ((s (p:getAuthority)))
    (if (eq? s #!null) #f s)))
(define (path-user-info (p :: path))
  (let ((s (p:getUserInfo)))
    (if (eq? s #!null) #f s)))
(define (path-host (p :: path))
  (p:getHost))
(define (path-file (p :: path))
  (let ((s (p:getPath)))
    (if (eq? s #!null) #f s)))
(define (path-directory (p :: path))
  (let ((s (p:getDirectory)))
    (if (eq? s #!null) #f (s:toString))))
(define (path-parent (p :: path))
  (let ((s (p:getParent)))
    (if (eq? s #!null) #f (s:toString))))
(define (path-last (p :: path))
  (let ((s (p:getLast)))
    (if (eq? s #!null) #f s)))
(define (path-extension (p :: path))
  (let ((s (p:getExtension)))
    (if (eq? s #!null) #f s)))
(define (path-port (p :: path)) :: <int>
  (p:getPort))
(define (path-query (p :: path))
  (let ((s (p:getQuery)))
    (if (eq? s #!null) #f s)))
(define (path-fragment (p :: path))
  (let ((s (p:getFragment)))
    (if (eq? s #!null) #f s)))

#|
(resolve-path path) ;; resolves symlinks
(path->complete-path path [base-path])
(path->directory-path path)
(string->path string)
(path->string path)
(build-path base-path sub-path ...)
(expand-path)
(simplify-path)
|#

(define-procedure path-bytes
  setter: (lambda ((p ::path) b::bytevector)::void
                  (let ((out (p:openOutputStream)))
                    (try-finally
                     (b:writeTo 0 (b:size) out)
                     (out:close))))
  (lambda (p ::path) ::bytevector name: 'path-bytes
          (gnu.lists.U8Vector (p:readAllBytes))))

(define (path-data-setter (p ::path) newvalue)::void
  (let ((out (p:openOutputStream))
        (in (gnu.kawa.functions.RunProcess:getInputStreamFrom newvalue)))
    (gnu.kawa.functions.RunProcess:copyStream in out #t)))

(define (path-data-appender (p ::filepath) newvalue)::void
  (let ((out (p:openAppendStream))
        (in (gnu.kawa.functions.RunProcess:getInputStreamFrom newvalue)))
    (gnu.kawa.functions.RunProcess:copyStream in out #t)))

(define-procedure path-data
  setter: path-data-setter
  (lambda (p ::path) ::gnu.lists.Blob name: 'path-data
           (gnu.lists.Blob (p:readAllBytes))))

(define-syntax path-data-setter-curried
  (syntax-rules ()
    ((_ p) (lambda (newvalue) ::void (path-data-setter p newvalue)))))

(define-syntax path-data-appender-curried
  (syntax-rules ()
    ((_ p) (lambda (newvalue) ::void (path-data-appender p newvalue)))))

(define-simple-constructor PD path-data $string$)
(define-simple-constructor set_PD path-data-setter-curried $string$)
(define-simple-constructor append_PD path-data-appender-curried $string$)

(define (file-exists? (file :: path)) :: <boolean>
  (file:exists))

(define (file-directory? (file :: path)) :: <boolean>
  (file:isDirectory))

(define (file-readable? (file :: filepath)) :: <boolean>
  ((file:toFile):canRead))

(define (file-writable? (file :: filepath)) :: <boolean>
  ((file:toFile):canWrite))

;(define (file-modification-time (filename :: path)) :: <long>
;  (filename:getLastModified))

(define (delete-file (file :: filepath)) :: <void>
  (file:deleteFile))

(define (rename-file (oldname :: filepath) (newname :: filepath))
  ((oldname:toFile):renameTo (newname:toFile)))

(define (copy-file (from :: path) (to :: path)) :: <void>
  (let ((in (from:openInputStream))
        (out (to:openOutputStream))
        (buf (byte[] length: 8192)))
    (let loop ()
      (let ((n (in:read buf)))
        (cond ((>= n 0)
               (out:write buf 0 n)
               (loop)))))))

(define (create-directory (dirname :: filepath))
  ((dirname:toFile):mkdir))

;; In Scsh and Gambit.
(define (directory-files (dir :: filepath))
  (let ((files ((java.io.File (dir:toFile)):list)))
     (if (eq? files #!null) #f
         (gnu.lists.LList:makeList files 0)))) 

;; (define (directory-for-each proc directory) ...)

; Taken from MIT Scheme
(define (->pathname filename) :: path
  (path filename))
  
(define (%file-separator)
  (invoke-static <java.lang.System> 'getProperty 'file.separator))

(define (system-tmpdir)
  (let ((name :: <java.lang.String> ; Java2 only
	 (invoke-static <java.lang.System> 'getProperty 'java.io.tmpdir)))
    (if (not (eq? name #!null))
	name
	(let ((sep (%file-separator)))
	  (if (equal? sep "\\") "C:\\temp" "/tmp")))))

; From scsh
;(define (directory-files [dir [dotfiles?]]) ...)

(define (resolve-uri (uri :: path) (base :: path)) :: path
  (base:resolve uri))

; From MzLib.  Scsh has (create-temp-file [prefix]).
(define (make-temporary-file #!optional (format :: <string> "kawa~d.tmp"))
  :: filepath
  (let* ((fmt (format:toString))
         (tilde (fmt:indexOf #\~))
         (prefix::java.lang.String (if (< tilde 0) fmt (fmt:substring 0 tilde)))
         (suffix (if (< tilde 0) ".tmp" (fmt:substring (+ 2 tilde))))
         (sep (prefix:indexOf java.io.File:separatorChar))
         (directory #!null))
    (cond ((>= sep 0)
           (set! directory (java.io.File (prefix:substring 0 sep)))
           (set! prefix (prefix:substring (+ sep 1)))))
    (java.io.File:createTempFile prefix suffix directory)))
