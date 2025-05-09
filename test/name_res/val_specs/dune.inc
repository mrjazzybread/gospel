(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_header_nm.mli.output
   (run %{checker} %{dep:invalid_header_nm.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_header_nm.mli invalid_header_nm.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to not_consumed1.mli.output
   (run %{checker} %{dep:not_consumed1.mli}))))

(rule
 (alias runtest)
 (action
  (diff not_consumed1.mli not_consumed1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to not_consumed2.mli.output
   (run %{checker} %{dep:not_consumed2.mli}))))

(rule
 (alias runtest)
 (action
  (diff not_consumed2.mli not_consumed2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to not_produced.mli.output
   (run %{checker} %{dep:not_produced.mli}))))

(rule
 (alias runtest)
 (action
  (diff not_produced.mli not_produced.mli.output)))

