(module-name (scheme load))
(require kawa.lib.syntax) ;; needed for import
(import (only (kawa standard load) load))
(export
 load)
