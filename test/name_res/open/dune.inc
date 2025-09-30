(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to open.mli.output
   (run %{checker} %{dep:open.mli}))))

(rule
 (alias runtest)
 (action
  (diff open.mli open.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to open_gospelstdlib_module.mli.output
   (run %{checker} %{dep:open_gospelstdlib_module.mli}))))

(rule
 (alias runtest)
 (action
  (diff open_gospelstdlib_module.mli open_gospelstdlib_module.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to positive.mli.output
   (run %{checker} %{dep:positive.mli}))))

(rule
 (alias runtest)
 (action
  (diff positive.mli positive.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to positive_scope.mli.output
   (run %{checker} %{dep:positive_scope.mli}))))

(rule
 (alias runtest)
 (action
  (diff positive_scope.mli positive_scope.mli.output)))

