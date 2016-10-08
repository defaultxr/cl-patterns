(defpackage #:cl-patterns
  (:use #:cl #:utilities)
  ;; (:import-from :utilities #:)
  (:export
   #:plist-keys
   #:as-stream
   #:next-n
   #:play-plist
   #:play-pattern

   #:defpattern
   
   #:pbind
   #:pk
   #:pseq
   #:prand
   #:pfunc))
