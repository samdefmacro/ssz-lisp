(in-package #:ssz)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;;; SSZ Constants

  ;; Number of bytes per chunk (for merkleization)
  (define +bytes-per-chunk+ 32)

  ;; Number of bytes per serialized length offset
  (define +bytes-per-length-offset+ 4)

  ;; Number of bits per byte
  (define +bits-per-byte+ 8)

  ;; Maximum offset value (2^32 - 1)
  (define +max-offset+ 4294967295))
