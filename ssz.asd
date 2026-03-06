;;;; SSZ (Simple Serialize) - Ethereum's serialization format
;;;; Implementation in Coalton

(asdf:defsystem "ssz"
  :description "SSZ (Simple Serialize) - Ethereum's serialization format implemented in Coalton"
  :author "SSZ-Lisp Project"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("coalton"
               "named-readtables"
               "ironclad")  ; For SHA256
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "constants")
               (:file "types")
               (:file "serialize")
               (:file "deserialize")
               (:file "merkle")
               (:file "merkle-cache")
               (:file "composite")
               (:file "typeclass")
               (:file "container")))

;;; Test system
(asdf:defsystem "ssz/tests"
  :description "Tests for SSZ library"
  :depends-on ("ssz" "cl-json")
  :pathname "test/"
  :serial t
  :components ((:file "package")
               (:file "helpers")
               (:file "unit-tests")
               (:file "spec-tests"))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:ssz-tests/runner '#:run-tests)))

;;; Spec test runner (separate system for running just spec tests)
(asdf:defsystem "ssz/spec-tests"
  :description "Consensus spec tests for SSZ library"
  :depends-on ("ssz/tests")
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:ssz-tests/runner '#:run-spec-tests)))

;;; Benchmark system
(asdf:defsystem "ssz/bench"
  :description "Performance benchmarks for SSZ library"
  :depends-on ("ssz")
  :pathname "bench/"
  :serial t
  :components ((:file "benchmarks")))
