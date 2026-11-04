(in-package :cl-test)
(deftest native-exception (funcall (ffi:ref "JSON" "parse") (ffi:cl->js "{")) nil)
(deftest after-native-exception (+ 2 3) 5)
