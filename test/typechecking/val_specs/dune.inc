(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_arg_number1.mli.output
   (run %{checker} %{dep:invalid_arg_number1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_arg_number1.mli invalid_arg_number1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_arg_number2.mli.output
   (run %{checker} %{dep:invalid_arg_number2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_arg_number2.mli invalid_arg_number2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_arg_number3.mli.output
   (run %{checker} %{dep:invalid_arg_number3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_arg_number3.mli invalid_arg_number3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_arg_number4.mli.output
   (run %{checker} %{dep:invalid_arg_number4.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_arg_number4.mli invalid_arg_number4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_ret_number1.mli.output
   (run %{checker} %{dep:invalid_ret_number1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_ret_number1.mli invalid_ret_number1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_ret_number2.mli.output
   (run %{checker} %{dep:invalid_ret_number2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_ret_number2.mli invalid_ret_number2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_ret_number3.mli.output
   (run %{checker} %{dep:invalid_ret_number3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_ret_number3.mli invalid_ret_number3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_unit1.mli.output
   (run %{checker} %{dep:invalid_unit1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_unit1.mli invalid_unit1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_unit2.mli.output
   (run %{checker} %{dep:invalid_unit2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_unit2.mli invalid_unit2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to valid_ret_number.mli.output
   (run %{checker} %{dep:valid_ret_number.mli}))))

(rule
 (alias runtest)
 (action
  (diff valid_ret_number.mli valid_ret_number.mli.output)))

