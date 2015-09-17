(module-name (scheme file))
(require kawa.lib.files)
(require kawa.lib.ports)
(export
 call-with-input-file call-with-output-file
 delete-file file-exists?
 open-binary-input-file open-binary-output-file
 open-input-file open-output-file
 with-input-from-file with-output-to-file)
