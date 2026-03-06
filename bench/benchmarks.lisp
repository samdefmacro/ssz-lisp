;;;; SSZ Performance Benchmarks
;;;;
;;;; Benchmarks for serialization, deserialization, and merkleization operations.

(defpackage #:ssz-bench
  (:documentation "Performance benchmarks for SSZ library")
  (:use #:cl)
  (:export #:run-all-benchmarks
           #:run-serialization-benchmarks
           #:run-deserialization-benchmarks
           #:run-merkle-benchmarks
           #:run-composite-benchmarks
           #:run-container-benchmarks
           #:benchmark-summary))

(in-package #:ssz-bench)

(named-readtables:in-readtable coalton:coalton)

;;; =========================================================================
;;; Benchmark Configuration
;;; =========================================================================

(defparameter *warmup-iterations* 100
  "Number of warmup iterations before timing")

(defparameter *benchmark-iterations* 10000
  "Number of iterations for each benchmark")

;;; =========================================================================
;;; Benchmark Infrastructure
;;; =========================================================================

(defstruct benchmark-result
  name
  iterations
  total-time-ms
  avg-time-us
  ops-per-sec
  bytes-consed)

(defvar *benchmark-results* nil
  "List of benchmark results from the current run")

(defmacro with-benchmark ((name &key (iterations nil iterations-p)) &body body)
  "Run a benchmark and record results"
  (let ((start-time (gensym "START-TIME"))
        (end-time (gensym "END-TIME"))
        (bytes-before (gensym "BYTES-BEFORE"))
        (bytes-after (gensym "BYTES-AFTER"))
        (iters (gensym "ITERS"))
        (i (gensym "I")))
    `(let ((,iters ,(if iterations-p iterations '*benchmark-iterations*)))
       ;; Warmup
       (dotimes (,i *warmup-iterations*)
         ,@body)
       ;; Force GC before measurement
       #+sbcl (sb-ext:gc :full t)
       #+ccl (ccl:gc)
       ;; Measure
       (let ((,bytes-before #+sbcl (sb-ext:get-bytes-consed) #-sbcl 0)
             (,start-time (get-internal-real-time)))
         (dotimes (,i ,iters)
           ,@body)
         (let* ((,end-time (get-internal-real-time))
                (,bytes-after #+sbcl (sb-ext:get-bytes-consed) #-sbcl 0)
                (total-ms (/ (* 1000.0 (- ,end-time ,start-time))
                             internal-time-units-per-second))
                (avg-us (/ (* 1000.0 total-ms) ,iters))
                (ops-sec (if (> total-ms 0)
                             (/ (* 1000.0 ,iters) total-ms)
                             0))
                (result (make-benchmark-result
                         :name ,name
                         :iterations ,iters
                         :total-time-ms total-ms
                         :avg-time-us avg-us
                         :ops-per-sec ops-sec
                         :bytes-consed (- ,bytes-after ,bytes-before))))
           (push result *benchmark-results*)
           (format t "  ~40A ~10,2F us/op  ~12,0F ops/sec~%"
                   ,name avg-us ops-sec)
           result)))))

(defun print-separator ()
  (format t "~%~A~%" (make-string 70 :initial-element #\=)))

(defun print-header (title)
  (print-separator)
  (format t "~A~%" title)
  (print-separator))

;;; =========================================================================
;;; Coalton Helper Functions (defined in SSZ package for type access)
;;; =========================================================================

(cl:in-package #:ssz)

(coalton-toplevel
  ;; Basic serialization wrappers
  (declare bench-serialize-u8 (Unit -> Bytes))
  (define (bench-serialize-u8) (serialize-u8 42))

  (declare bench-serialize-u16 (Unit -> Bytes))
  (define (bench-serialize-u16) (serialize-u16 12345))

  (declare bench-serialize-u32 (Unit -> Bytes))
  (define (bench-serialize-u32) (serialize-u32 123456789))

  (declare bench-serialize-u64 (Unit -> Bytes))
  (define (bench-serialize-u64) (serialize-u64 9876543210))

  (declare bench-serialize-u128 (Unit -> Bytes))
  (define (bench-serialize-u128) (serialize-u128 (u128-make 12345 67890)))

  (declare bench-serialize-u256 (Unit -> Bytes))
  (define (bench-serialize-u256) (serialize-u256 (u256-make 1 2 3 4)))

  (declare bench-serialize-bool-true (Unit -> Bytes))
  (define (bench-serialize-bool-true) (serialize-bool True))

  (declare bench-serialize-bool-false (Unit -> Bytes))
  (define (bench-serialize-bool-false) (serialize-bool False))

  ;; SHA256 wrappers
  (declare bench-sha256-empty (Unit -> Bytes))
  (define (bench-sha256-empty) (sha256 (make-bytes 0)))

  (declare bench-sha256-32 (Unit -> Bytes))
  (define (bench-sha256-32) (sha256 (make-bytes 32)))

  (declare bench-sha256-64 (Unit -> Bytes))
  (define (bench-sha256-64) (sha256 (make-bytes 64)))

  (declare bench-sha256-1024 (Unit -> Bytes))
  (define (bench-sha256-1024) (sha256 (make-bytes 1024)))

  ;; Hash concat wrapper
  (declare bench-hash-concat (Unit -> Chunk))
  (define (bench-hash-concat)
    (let ((chunk (zero-chunk)))
      (hash-concat chunk chunk)))

  ;; Hash tree root wrappers
  (declare bench-hash-tree-root-bool (Unit -> Chunk))
  (define (bench-hash-tree-root-bool) (hash-tree-root True))

  (declare bench-hash-tree-root-u64 (Unit -> Chunk))
  (define (bench-hash-tree-root-u64)
    (hash-tree-root (the U64 123456789)))

  ;; Merkleize wrappers
  (declare bench-merkleize-1 (Unit -> Chunk))
  (define (bench-merkleize-1)
    (merkleize (Cons (zero-chunk) Nil)))

  (declare bench-merkleize-4 (Unit -> Chunk))
  (define (bench-merkleize-4)
    (let ((c (zero-chunk)))
      (merkleize (Cons c (Cons c (Cons c (Cons c Nil)))))))

  ;; Pack wrapper
  (declare bench-pack-100 (Unit -> (List Chunk)))
  (define (bench-pack-100) (pack (make-bytes 100)))

  ;; Vector helpers
  (declare bench-make-u32-list (UFix -> (List U32)))
  (define (bench-make-u32-list n)
    (if (== n 0)
        Nil
        (Cons (the U32 42) (bench-make-u32-list (- n 1)))))

  (declare bench-vector-serialize-10 (Unit -> Bytes))
  (define (bench-vector-serialize-10)
    (ssz-serialize (the (SSZVector U32) (SSZVector (bench-make-u32-list 10)))))

  (declare bench-vector-serialize-100 (Unit -> Bytes))
  (define (bench-vector-serialize-100)
    (ssz-serialize (the (SSZVector U32) (SSZVector (bench-make-u32-list 100)))))

  (declare bench-vector-serialize-1000 (Unit -> Bytes))
  (define (bench-vector-serialize-1000)
    (ssz-serialize (the (SSZVector U32) (SSZVector (bench-make-u32-list 1000)))))

  (declare bench-list-serialize-10 (Unit -> Bytes))
  (define (bench-list-serialize-10)
    (ssz-serialize (the (SSZList U32) (SSZList 10000 (bench-make-u32-list 10)))))

  (declare bench-list-serialize-100 (Unit -> Bytes))
  (define (bench-list-serialize-100)
    (ssz-serialize (the (SSZList U32) (SSZList 10000 (bench-make-u32-list 100)))))

  (declare bench-list-serialize-1000 (Unit -> Bytes))
  (define (bench-list-serialize-1000)
    (ssz-serialize (the (SSZList U32) (SSZList 10000 (bench-make-u32-list 1000)))))

  ;; Bitvector wrappers
  (declare bench-bitvector-64 (Unit -> Bytes))
  (define (bench-bitvector-64)
    (serialize-bitvector (ssz-bitvector-zeros 64)))

  (declare bench-bitvector-256 (Unit -> Bytes))
  (define (bench-bitvector-256)
    (serialize-bitvector (ssz-bitvector-zeros 256)))

  (declare bench-bitvector-1024 (Unit -> Bytes))
  (define (bench-bitvector-1024)
    (serialize-bitvector (ssz-bitvector-zeros 1024)))

  ;; Bitlist wrapper
  (declare bench-bitlist-empty (Unit -> Bytes))
  (define (bench-bitlist-empty)
    (serialize-bitlist (ssz-bitlist-empty 1024)))

  ;; Hash tree root for composites
  (declare bench-vector-htr-10 (Unit -> Chunk))
  (define (bench-vector-htr-10)
    (hash-tree-root (the (SSZVector U32) (SSZVector (bench-make-u32-list 10)))))

  (declare bench-vector-htr-100 (Unit -> Chunk))
  (define (bench-vector-htr-100)
    (hash-tree-root (the (SSZVector U32) (SSZVector (bench-make-u32-list 100)))))

  (declare bench-vector-htr-1000 (Unit -> Chunk))
  (define (bench-vector-htr-1000)
    (hash-tree-root (the (SSZVector U32) (SSZVector (bench-make-u32-list 1000)))))

  (declare bench-list-htr-10 (Unit -> Chunk))
  (define (bench-list-htr-10)
    (hash-tree-root (the (SSZList U32) (SSZList 10000 (bench-make-u32-list 10)))))

  (declare bench-list-htr-100 (Unit -> Chunk))
  (define (bench-list-htr-100)
    (hash-tree-root (the (SSZList U32) (SSZList 10000 (bench-make-u32-list 100)))))

  (declare bench-bitvector-htr-256 (Unit -> Chunk))
  (define (bench-bitvector-htr-256)
    (hash-tree-root (ssz-bitvector-zeros 256)))

  (declare bench-bitlist-htr (Unit -> Chunk))
  (define (bench-bitlist-htr)
    (hash-tree-root (ssz-bitlist-empty 1024)))

  ;; Container hash tree root
  (declare bench-container-htr-2 (Unit -> Chunk))
  (define (bench-container-htr-2)
    (hash-tree-root-container
     (Cons (hash-tree-root True)
      (Cons (hash-tree-root False) Nil))))

  (declare bench-container-htr-8 (Unit -> Chunk))
  (define (bench-container-htr-8)
    (let ((r (hash-tree-root True)))
      (hash-tree-root-container
       (Cons r (Cons r (Cons r (Cons r
        (Cons r (Cons r (Cons r (Cons r Nil))))))))))))

(cl:in-package #:ssz-bench)

;;; =========================================================================
;;; Basic Type Serialization Benchmarks
;;; =========================================================================

(defun run-serialization-benchmarks ()
  "Run serialization benchmarks for basic types"
  (print-header "SERIALIZATION BENCHMARKS")

  (with-benchmark ("serialize-u8")
    (ssz::bench-serialize-u8 coalton:Unit))

  (with-benchmark ("serialize-u16")
    (ssz::bench-serialize-u16 coalton:Unit))

  (with-benchmark ("serialize-u32")
    (ssz::bench-serialize-u32 coalton:Unit))

  (with-benchmark ("serialize-u64")
    (ssz::bench-serialize-u64 coalton:Unit))

  (with-benchmark ("serialize-u128")
    (ssz::bench-serialize-u128 coalton:Unit))

  (with-benchmark ("serialize-u256")
    (ssz::bench-serialize-u256 coalton:Unit))

  (with-benchmark ("serialize-bool (true)")
    (ssz::bench-serialize-bool-true coalton:Unit))

  (with-benchmark ("serialize-bool (false)")
    (ssz::bench-serialize-bool-false coalton:Unit)))

;;; =========================================================================
;;; Basic Type Deserialization Benchmarks
;;; =========================================================================

(defun run-deserialization-benchmarks ()
  "Run deserialization benchmarks for basic types"
  (print-header "DESERIALIZATION BENCHMARKS")

  ;; Pre-serialize data for deserialization benchmarks
  (let ((u8-data (ssz::bench-serialize-u8 coalton:Unit))
        (u16-data (ssz::bench-serialize-u16 coalton:Unit))
        (u32-data (ssz::bench-serialize-u32 coalton:Unit))
        (u64-data (ssz::bench-serialize-u64 coalton:Unit))
        (u128-data (ssz::bench-serialize-u128 coalton:Unit))
        (u256-data (ssz::bench-serialize-u256 coalton:Unit))
        (bool-data (ssz::bench-serialize-bool-true coalton:Unit)))

    (with-benchmark ("deserialize-u8")
      (ssz:deserialize-u8 u8-data))

    (with-benchmark ("deserialize-u16")
      (ssz:deserialize-u16 u16-data))

    (with-benchmark ("deserialize-u32")
      (ssz:deserialize-u32 u32-data))

    (with-benchmark ("deserialize-u64")
      (ssz:deserialize-u64 u64-data))

    (with-benchmark ("deserialize-u128")
      (ssz:deserialize-u128 u128-data))

    (with-benchmark ("deserialize-u256")
      (ssz:deserialize-u256 u256-data))

    (with-benchmark ("deserialize-bool")
      (ssz:deserialize-bool bool-data))))

;;; =========================================================================
;;; Merkle/Hash Tree Root Benchmarks
;;; =========================================================================

(defun run-merkle-benchmarks ()
  "Run merkleization and hash tree root benchmarks"
  (print-header "MERKLE / HASH TREE ROOT BENCHMARKS")

  (with-benchmark ("sha256 (empty)")
    (ssz::bench-sha256-empty coalton:Unit))

  (with-benchmark ("sha256 (32 bytes)")
    (ssz::bench-sha256-32 coalton:Unit))

  (with-benchmark ("sha256 (64 bytes)")
    (ssz::bench-sha256-64 coalton:Unit))

  (with-benchmark ("sha256 (1024 bytes)")
    (ssz::bench-sha256-1024 coalton:Unit))

  (with-benchmark ("hash-concat (two chunks)")
    (ssz::bench-hash-concat coalton:Unit))

  (with-benchmark ("hash-tree-root (Boolean)")
    (ssz::bench-hash-tree-root-bool coalton:Unit))

  (with-benchmark ("hash-tree-root (U64)")
    (ssz::bench-hash-tree-root-u64 coalton:Unit))

  (with-benchmark ("merkleize (1 chunk)")
    (ssz::bench-merkleize-1 coalton:Unit))

  (with-benchmark ("merkleize (4 chunks)")
    (ssz::bench-merkleize-4 coalton:Unit))

  (with-benchmark ("pack (100 bytes -> 4 chunks)")
    (ssz::bench-pack-100 coalton:Unit)))

;;; =========================================================================
;;; Composite Type Benchmarks
;;; =========================================================================

(defun run-composite-benchmarks ()
  "Run benchmarks for composite types"
  (print-header "COMPOSITE TYPE BENCHMARKS")

  (format t "~%--- Vector[U32] Serialization ---~%")

  (with-benchmark ("Vector[U32] serialize (10 elements)")
    (ssz::bench-vector-serialize-10 coalton:Unit))

  (with-benchmark ("Vector[U32] serialize (100 elements)")
    (ssz::bench-vector-serialize-100 coalton:Unit))

  (with-benchmark ("Vector[U32] serialize (1000 elements)" :iterations 1000)
    (ssz::bench-vector-serialize-1000 coalton:Unit))

  (format t "~%--- List[U32] Serialization ---~%")

  (with-benchmark ("List[U32] serialize (10 elements)")
    (ssz::bench-list-serialize-10 coalton:Unit))

  (with-benchmark ("List[U32] serialize (100 elements)")
    (ssz::bench-list-serialize-100 coalton:Unit))

  (with-benchmark ("List[U32] serialize (1000 elements)" :iterations 1000)
    (ssz::bench-list-serialize-1000 coalton:Unit))

  (format t "~%--- Bitvector Serialization ---~%")

  (with-benchmark ("Bitvector serialize (64 bits)")
    (ssz::bench-bitvector-64 coalton:Unit))

  (with-benchmark ("Bitvector serialize (256 bits)")
    (ssz::bench-bitvector-256 coalton:Unit))

  (with-benchmark ("Bitvector serialize (1024 bits)")
    (ssz::bench-bitvector-1024 coalton:Unit))

  (format t "~%--- Bitlist Serialization ---~%")

  (with-benchmark ("Bitlist serialize (empty, max=1024)")
    (ssz::bench-bitlist-empty coalton:Unit))

  (format t "~%--- Hash Tree Root (Composite) ---~%")

  (with-benchmark ("Vector[U32] hash-tree-root (10 elements)")
    (ssz::bench-vector-htr-10 coalton:Unit))

  (with-benchmark ("Vector[U32] hash-tree-root (100 elements)")
    (ssz::bench-vector-htr-100 coalton:Unit))

  (with-benchmark ("Vector[U32] hash-tree-root (1000 elems)" :iterations 1000)
    (ssz::bench-vector-htr-1000 coalton:Unit))

  (with-benchmark ("List[U32] hash-tree-root (10 elements)")
    (ssz::bench-list-htr-10 coalton:Unit))

  (with-benchmark ("List[U32] hash-tree-root (100 elements)")
    (ssz::bench-list-htr-100 coalton:Unit))

  (with-benchmark ("Bitvector hash-tree-root (256 bits)")
    (ssz::bench-bitvector-htr-256 coalton:Unit))

  (with-benchmark ("Bitlist hash-tree-root (empty, max=1024)")
    (ssz::bench-bitlist-htr coalton:Unit)))

;;; =========================================================================
;;; Container Benchmarks
;;; =========================================================================

(defun run-container-benchmarks ()
  "Run benchmarks for container serialization"
  (print-header "CONTAINER BENCHMARKS")

  ;; Small container: 3 fixed fields
  (format t "~%--- Small Container (3 fixed fields) ---~%")

  (with-benchmark ("Container serialize (3 fixed: U32, U16, U8)")
    (let ((f1 (ssz:fixed-field 4 (ssz:serialize-u32 12345)))
          (f2 (ssz:fixed-field 2 (ssz:serialize-u16 678)))
          (f3 (ssz:fixed-field 1 (ssz:serialize-u8 90))))
      (ssz:serialize-container (list f1 f2 f3))))

  ;; Container with variable field
  (format t "~%--- Container with Variable Field ---~%")

  (let ((var-data (make-array 100 :initial-element 0
                              :fill-pointer 100
                              :adjustable t)))
    (with-benchmark ("Container serialize (U32 + 100-byte var + U16)")
      (let ((f1 (ssz:fixed-field 4 (ssz:serialize-u32 999)))
            (f2 (ssz:variable-field var-data))
            (f3 (ssz:fixed-field 2 (ssz:serialize-u16 111))))
        (ssz:serialize-container (list f1 f2 f3)))))

  ;; Container deserialization
  (format t "~%--- Container Deserialization ---~%")

  (let* ((f1 (ssz:fixed-field 4 (ssz:serialize-u32 12345)))
         (f2 (ssz:fixed-field 2 (ssz:serialize-u16 678)))
         (f3 (ssz:fixed-field 1 (ssz:serialize-u8 90)))
         (serialized (ssz:serialize-container (list f1 f2 f3)))
         (field-types (list (ssz:FixedField 4)
                            (ssz:FixedField 2)
                            (ssz:FixedField 1))))
    (with-benchmark ("Container deserialize (3 fixed fields)")
      (ssz:deserialize-container field-types serialized)))

  ;; Container hash tree root
  (format t "~%--- Container Hash Tree Root ---~%")

  (with-benchmark ("Container hash-tree-root (2 fields)")
    (ssz::bench-container-htr-2 coalton:Unit))

  (with-benchmark ("Container hash-tree-root (8 fields)")
    (ssz::bench-container-htr-8 coalton:Unit)))

;;; =========================================================================
;;; Summary
;;; =========================================================================

(defun benchmark-summary ()
  "Print a summary of all benchmark results"
  (print-header "BENCHMARK SUMMARY")
  (format t "~%~50A ~12A ~15A~%" "Operation" "us/op" "ops/sec")
  (format t "~A~%" (make-string 80 :initial-element #\-))
  (dolist (result (reverse *benchmark-results*))
    (format t "~50A ~12,2F ~15,0F~%"
            (benchmark-result-name result)
            (benchmark-result-avg-time-us result)
            (benchmark-result-ops-per-sec result)))
  (format t "~%Total benchmarks: ~A~%" (length *benchmark-results*)))

(defun run-all-benchmarks ()
  "Run all SSZ benchmarks"
  (setf *benchmark-results* nil)
  (format t "~%")
  (format t "========================================~%")
  (format t "     SSZ Performance Benchmarks~%")
  (format t "========================================~%")
  (format t "Iterations: ~A (warmup: ~A)~%"
          *benchmark-iterations* *warmup-iterations*)
  (format t "Lisp: ~A ~A~%"
          (lisp-implementation-type)
          (lisp-implementation-version))

  (run-serialization-benchmarks)
  (run-deserialization-benchmarks)
  (run-merkle-benchmarks)
  (run-composite-benchmarks)
  (run-container-benchmarks)
  (benchmark-summary)

  (format t "~%Benchmark run complete.~%")
  *benchmark-results*)
