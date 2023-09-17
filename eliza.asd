(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  )

(defpackage :eliza-system
  (:use :common-lisp :asdf))

(in-package :eliza-system)

(defsystem :eliza
  :name "Eliza"
  :author "Peter Novig"
  :maintainer "Seiji Koide <koide@ontolonomy.co.jp>"
  :version "0.0.1"
  :license "PAIP"
  :description "Eliza program from PAIP, but modernized by Seiji"
  :long-description "Modified version of Eliza in Common Lisp from 'Paradigms of Artificiall Intelligence Programming' by Peter Norvig. This is modernized according to modern lisp by Seiji Koide."
  :components
  ((:file "elizaaux")
   (:file "eliza1")
   (:file "eliza")))