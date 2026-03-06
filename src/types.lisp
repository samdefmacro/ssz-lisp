(in-package #:ssz)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;;; Error types for SSZ operations

  (define-type SSZError
    "Errors that can occur during SSZ operations"
    (SerializationError String)
    (DeserializationError String)
    (MerkleizationError String))

  ;; Result type alias for SSZ operations
  (define-type-alias (SSZResult :a) (Result SSZError :a))

  ;;; Bytes type - represents a byte vector

  (define-type-alias Bytes (vec:Vector U8))

  (declare bytes-length (Bytes -> UFix))
  (define (bytes-length bytes)
    "Get the length of a byte vector"
    (vec:length bytes))

  (declare bytes-ref (UFix -> Bytes -> (Optional U8)))
  (define (bytes-ref idx bytes)
    "Get a byte at a specific index"
    (vec:index idx bytes))

  (declare bytes-ref-unsafe (UFix -> Bytes -> U8))
  (define (bytes-ref-unsafe idx bytes)
    "Get a byte at a specific index (unsafe - assumes valid index)"
    (vec:index-unsafe idx bytes))

  (declare make-bytes (UFix -> Bytes))
  (define (make-bytes len)
    "Create a byte vector of given length filled with zeros"
    (vec:with-initial-element len 0))

  (declare bytes-from-list ((List U8) -> Bytes))
  (define (bytes-from-list lst)
    "Create a byte vector from a list of bytes"
    (iter:collect! (iter:into-iter lst)))

  (declare bytes-append (Bytes -> Bytes -> Bytes))
  (define (bytes-append a b)
    "Concatenate two byte vectors"
    (let ((result (vec:copy a)))
      ;; Use push! in a loop instead of extend! due to a bug in vec:extend!
      ;; when operating on copies of extended vectors
      (for elem in (iter:into-iter b)
        (vec:push! elem result))
      result))

  (declare bytes-slice (UFix -> UFix -> Bytes -> Bytes))
  (define (bytes-slice start len bytes)
    "Extract a slice from bytes starting at start with given length"
    (lisp Bytes (start len bytes)
      (cl:let ((result (cl:make-array len :fill-pointer 0 :adjustable cl:t)))
        (cl:loop :for i :from 0 :below len
                 :for idx := (cl:+ start i)
                 :when (cl:< idx (cl:length bytes))
                 :do (cl:vector-push-extend (cl:aref bytes idx) result))
        result)))

  (declare bytes-set! (UFix -> U8 -> Bytes -> Unit))
  (define (bytes-set! idx val bytes)
    "Set a byte at a specific index"
    (vec:set! idx val bytes))

  ;;; U256 type - 256-bit unsigned integer (stored as vector of 4 U64s in little-endian)
  ;;; Format: [low64, mid-low64, mid-high64, high64]

  (repr :native (cl:simple-array cl:t (4)))
  (define-type U256
    "256-bit unsigned integer")

  (declare u256-make (U64 -> U64 -> U64 -> U64 -> U256))
  (define (u256-make w0 w1 w2 w3)
    "Create a U256 from 4 U64 words (w0 is lowest, w3 is highest)"
    (lisp U256 (w0 w1 w2 w3)
      (cl:let ((arr (cl:make-array 4)))
        (cl:setf (cl:aref arr 0) w0)
        (cl:setf (cl:aref arr 1) w1)
        (cl:setf (cl:aref arr 2) w2)
        (cl:setf (cl:aref arr 3) w3)
        arr)))

  (declare u256-word (UFix -> U256 -> U64))
  (define (u256-word idx u)
    "Get the idx-th word (U64) from U256 (0 = lowest)"
    (lisp U64 (idx u)
      (cl:aref u idx)))

  (declare u256-zero (Unit -> U256))
  (define (u256-zero)
    "Create a zero U256"
    (u256-make 0 0 0 0))

  ;;; U128 type - 128-bit unsigned integer (stored as vector of 2 U64s in little-endian)

  (repr :native (cl:simple-array cl:t (2)))
  (define-type U128
    "128-bit unsigned integer")

  (declare u128-make (U64 -> U64 -> U128))
  (define (u128-make w0 w1)
    "Create a U128 from 2 U64 words (w0 is low, w1 is high)"
    (lisp U128 (w0 w1)
      (cl:let ((arr (cl:make-array 2)))
        (cl:setf (cl:aref arr 0) w0)
        (cl:setf (cl:aref arr 1) w1)
        arr)))

  (declare u128-word (UFix -> U128 -> U64))
  (define (u128-word idx u)
    "Get the idx-th word (U64) from U128 (0 = low, 1 = high)"
    (lisp U64 (idx u)
      (cl:aref u idx)))

  (declare u128-zero (Unit -> U128))
  (define (u128-zero)
    "Create a zero U128"
    (u128-make 0 0)))
