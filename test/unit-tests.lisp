;;; Unit tests for SSZ library - Pure Common Lisp code

(in-package #:ssz-tests/runner)

;;; =========================================================================
;;; Test Infrastructure
;;; =========================================================================

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defmacro test-case (name &body body)
  "Run a test case and track results"
  `(handler-case
       (progn
         ,@body
         (incf *tests-passed*)
         (format t "  ~A: PASS~%" ,name))
     (error (e)
       (incf *tests-failed*)
       (format t "  ~A: FAIL - ~A~%" ,name e))))

(defun bytes-equal (a b)
  "Check if two byte arrays are equal"
  (and (= (length a) (length b))
       (every #'= (coerce a 'list) (coerce b 'list))))

(defun run-tests ()
  "Run all SSZ tests"
  (setf *tests-passed* 0)
  (setf *tests-failed* 0)
  (format t "~%========================================~%")
  (format t "Running Comprehensive SSZ Tests~%")
  (format t "========================================~%")

  (run-bytes-append-tests)
  (run-u8-tests)
  (run-u16-tests)
  (run-u32-tests)
  (run-u64-tests)
  (run-u128-tests)
  (run-u256-tests)
  (run-boolean-tests)
  (run-deserialization-tests)
  (run-bitvector-tests)
  (run-bitlist-tests)
  (run-merkle-tests)
  (run-container-tests)
  (run-hash-tree-root-tests)
  (run-vector-list-tests)
  (run-union-tests)
  (run-roundtrip-tests)
  (run-edge-case-tests)
  (run-additional-coverage-tests)

  (format t "~%========================================~%")
  (format t "Results: ~A passed, ~A failed~%" *tests-passed* *tests-failed*)
  (format t "========================================~%")
  (values *tests-passed* *tests-failed*))

;;; =========================================================================
;;; Bytes Append Tests (regression test for vec:extend! bug)
;;; =========================================================================

(defun run-bytes-append-tests ()
  (format t "~%=== Bytes Append Tests ===~%")

  (test-case "bytes-append - two vectors"
    (let* ((a (ssz:serialize-u32 100))
           (b (ssz:serialize-u32 200))
           (result (ssz:bytes-append a b)))
      (assert (= (length result) 8))
      (assert (= (aref result 0) 100))
      (assert (= (aref result 4) 200))))

  (test-case "bytes-append - three vectors (regression test)"
    (let* ((a (ssz:serialize-u32 100))
           (b (ssz:serialize-u32 200))
           (c (ssz:serialize-u32 300))
           (empty-bytes (ssz:make-bytes 0))
           (step1 (ssz:bytes-append empty-bytes a))
           (step2 (ssz:bytes-append step1 b))
           (step3 (ssz:bytes-append step2 c)))
      (assert (= (length step1) 4))
      (assert (= (length step2) 8))
      (assert (= (length step3) 12))
      (assert (= (aref step3 0) 100))
      (assert (= (aref step3 4) 200))
      (assert (= (aref step3 8) 44))
      (assert (= (aref step3 9) 1))))

  (test-case "bytes-append - empty vectors"
    (let* ((empty-bytes (ssz:make-bytes 0))
           (a (ssz:serialize-u32 42))
           (result1 (ssz:bytes-append empty-bytes a))
           (result2 (ssz:bytes-append a empty-bytes))
           (result3 (ssz:bytes-append empty-bytes empty-bytes)))
      (assert (= (length result1) 4))
      (assert (= (length result2) 4))
      (assert (= (length result3) 0)))))

;;; =========================================================================
;;; U8 Serialization Tests
;;; =========================================================================

(defun run-u8-tests ()
  (format t "~%=== U8 Tests ===~%")

  (test-case "U8 zero"
    (let ((result (coalton:coalton (ssz:serialize-u8 0))))
      (assert (= (length result) 1))
      (assert (= (aref result 0) 0))))

  (test-case "U8 max (255)"
    (let ((result (coalton:coalton (ssz:serialize-u8 255))))
      (assert (= (length result) 1))
      (assert (= (aref result 0) 255))))

  (test-case "U8 arbitrary (42)"
    (let ((result (coalton:coalton (ssz:serialize-u8 42))))
      (assert (= (length result) 1))
      (assert (= (aref result 0) 42)))))

;;; =========================================================================
;;; U16 Serialization Tests
;;; =========================================================================

(defun run-u16-tests ()
  (format t "~%=== U16 Tests ===~%")

  (test-case "U16 zero"
    (let ((result (coalton:coalton (ssz:serialize-u16 0))))
      (assert (= (length result) 2))
      (assert (= (aref result 0) 0))
      (assert (= (aref result 1) 0))))

  (test-case "U16 max (65535)"
    (let ((result (coalton:coalton (ssz:serialize-u16 65535))))
      (assert (= (length result) 2))
      (assert (= (aref result 0) 255))
      (assert (= (aref result 1) 255))))

  (test-case "U16 little-endian (0x0102 = 258)"
    (let ((result (coalton:coalton (ssz:serialize-u16 258))))
      (assert (= (length result) 2))
      (assert (= (aref result 0) 2))
      (assert (= (aref result 1) 1))))

  (test-case "U16 256 (0x0100)"
    (let ((result (coalton:coalton (ssz:serialize-u16 256))))
      (assert (= (aref result 0) 0))
      (assert (= (aref result 1) 1)))))

;;; =========================================================================
;;; U32 Serialization Tests
;;; =========================================================================

(defun run-u32-tests ()
  (format t "~%=== U32 Tests ===~%")

  (test-case "U32 zero"
    (let ((result (coalton:coalton (ssz:serialize-u32 0))))
      (assert (= (length result) 4))
      (assert (every #'zerop (coerce result 'list)))))

  (test-case "U32 max (4294967295)"
    (let ((result (coalton:coalton (ssz:serialize-u32 4294967295))))
      (assert (= (length result) 4))
      (assert (every (lambda (b) (= b 255)) (coerce result 'list)))))

  (test-case "U32 little-endian (0x01020304)"
    (let ((result (coalton:coalton (ssz:serialize-u32 16909060))))
      (assert (= (aref result 0) 4))
      (assert (= (aref result 1) 3))
      (assert (= (aref result 2) 2))
      (assert (= (aref result 3) 1))))

  (test-case "U32 65536 (0x00010000)"
    (let ((result (coalton:coalton (ssz:serialize-u32 65536))))
      (assert (= (aref result 0) 0))
      (assert (= (aref result 1) 0))
      (assert (= (aref result 2) 1))
      (assert (= (aref result 3) 0)))))

;;; =========================================================================
;;; U64 Serialization Tests
;;; =========================================================================

(defun run-u64-tests ()
  (format t "~%=== U64 Tests ===~%")

  (test-case "U64 zero"
    (let ((result (coalton:coalton (ssz:serialize-u64 0))))
      (assert (= (length result) 8))
      (assert (every #'zerop (coerce result 'list)))))

  (test-case "U64 max"
    (let ((result (coalton:coalton (ssz:serialize-u64 18446744073709551615))))
      (assert (= (length result) 8))
      (assert (every (lambda (b) (= b 255)) (coerce result 'list)))))

  (test-case "U64 little-endian (0x0102030405060708)"
    (let ((result (coalton:coalton (ssz:serialize-u64 72623859790382856))))
      (assert (= (aref result 0) 8))
      (assert (= (aref result 1) 7))
      (assert (= (aref result 2) 6))
      (assert (= (aref result 3) 5))
      (assert (= (aref result 4) 4))
      (assert (= (aref result 5) 3))
      (assert (= (aref result 6) 2))
      (assert (= (aref result 7) 1))))

  (test-case "U64 power of 2 (2^32)"
    (let ((result (coalton:coalton (ssz:serialize-u64 4294967296))))
      (assert (= (aref result 0) 0))
      (assert (= (aref result 1) 0))
      (assert (= (aref result 2) 0))
      (assert (= (aref result 3) 0))
      (assert (= (aref result 4) 1))
      (assert (= (aref result 5) 0))
      (assert (= (aref result 6) 0))
      (assert (= (aref result 7) 0)))))

;;; =========================================================================
;;; U128 Serialization Tests
;;; =========================================================================

(defun run-u128-tests ()
  (format t "~%=== U128 Tests ===~%")

  (test-case "U128 zero"
    (let ((result (coalton:coalton (ssz:serialize-u128 (ssz:u128-zero coalton:Unit)))))
      (assert (= (length result) 16))
      (assert (every #'zerop (coerce result 'list)))))

  (test-case "U128 from small value"
    (let ((result (coalton:coalton (ssz:serialize-u128 (ssz:u128-make 256 0)))))
      (assert (= (length result) 16))
      (assert (= (aref result 0) 0))
      (assert (= (aref result 1) 1))
      (assert (every #'zerop (subseq result 2))))))

;;; =========================================================================
;;; U256 Serialization Tests
;;; =========================================================================

(defun run-u256-tests ()
  (format t "~%=== U256 Tests ===~%")

  (test-case "U256 zero"
    (let ((result (coalton:coalton (ssz:serialize-u256 (ssz:u256-zero coalton:Unit)))))
      (assert (= (length result) 32))
      (assert (every #'zerop (coerce result 'list)))))

  (test-case "U256 from small value"
    (let ((result (coalton:coalton (ssz:serialize-u256 (ssz:u256-make 255 0 0 0)))))
      (assert (= (length result) 32))
      (assert (= (aref result 0) 255))
      (assert (every #'zerop (subseq result 1))))))

;;; =========================================================================
;;; Boolean Serialization Tests
;;; =========================================================================

(defun run-boolean-tests ()
  (format t "~%=== Boolean Tests ===~%")

  (test-case "Boolean True"
    (let ((result (coalton:coalton (ssz:serialize-bool coalton:True))))
      (assert (= (length result) 1))
      (assert (= (aref result 0) 1))))

  (test-case "Boolean False"
    (let ((result (coalton:coalton (ssz:serialize-bool coalton:False))))
      (assert (= (length result) 1))
      (assert (= (aref result 0) 0)))))

;;; =========================================================================
;;; Deserialization Tests (Round-trip)
;;; =========================================================================

(defun run-deserialization-tests ()
  (format t "~%=== Deserialization Tests ===~%")

  (test-case "U8 round-trip"
    (let ((result (coalton:coalton (ssz:deserialize-u8 (ssz:serialize-u8 42)))))
      (assert (not (typep result 'ssz:SSZError)))))

  (test-case "U16 round-trip"
    (let ((result (coalton:coalton (ssz:deserialize-u16 (ssz:serialize-u16 12345)))))
      (assert (not (typep result 'ssz:SSZError)))))

  (test-case "U32 round-trip"
    (let ((result (coalton:coalton (ssz:deserialize-u32 (ssz:serialize-u32 123456789)))))
      (assert (not (typep result 'ssz:SSZError)))))

  (test-case "U64 round-trip"
    (let ((result (coalton:coalton (ssz:deserialize-u64 (ssz:serialize-u64 9876543210)))))
      (assert (not (typep result 'ssz:SSZError)))))

  (test-case "Boolean True round-trip"
    (let ((result (coalton:coalton (ssz:deserialize-bool (ssz:serialize-bool coalton:True)))))
      (assert (not (typep result 'ssz:SSZError)))))

  (test-case "Boolean False round-trip"
    (let ((result (coalton:coalton (ssz:deserialize-bool (ssz:serialize-bool coalton:False)))))
      (assert (not (typep result 'ssz:SSZError)))))

  (test-case "U16 deserialize wrong size"
    (let ((result (coalton:coalton (ssz:deserialize-u16 (ssz:serialize-u8 42)))))
      (assert (search "ERR" (symbol-name (type-of result))))))

  (test-case "U32 deserialize wrong size"
    (let ((result (coalton:coalton (ssz:deserialize-u32 (ssz:serialize-u16 42)))))
      (assert (search "ERR" (symbol-name (type-of result))))))

  (test-case "Boolean deserialize invalid value"
    (let ((result (coalton:coalton
                   (ssz:deserialize-bool (ssz:serialize-u8 2)))))
      (assert (search "ERR" (symbol-name (type-of result)))))))

;;; =========================================================================
;;; Bitvector Tests
;;; =========================================================================

(defun run-bitvector-tests ()
  (format t "~%=== Bitvector Tests ===~%")

  (test-case "Bitvector zeros"
    (let* ((bv (coalton:coalton (ssz:ssz-bitvector-zeros 8)))
           (result (coalton:coalton (ssz:serialize-bitvector (ssz:ssz-bitvector-zeros 8)))))
      (declare (ignore bv))
      (assert (= (length result) 1))
      (assert (= (aref result 0) 0))))

  (test-case "Bitvector length"
    (let ((len (coalton:coalton (ssz:ssz-bitvector-length (ssz:ssz-bitvector-zeros 16)))))
      (assert (= len 16))))

  (test-case "Bitvector 8 bits serializes to 1 byte"
    (let ((result (coalton:coalton (ssz:serialize-bitvector (ssz:ssz-bitvector-zeros 8)))))
      (assert (= (length result) 1))))

  (test-case "Bitvector 9 bits serializes to 2 bytes"
    (let ((result (coalton:coalton (ssz:serialize-bitvector (ssz:ssz-bitvector-zeros 9)))))
      (assert (= (length result) 2))))

  (test-case "Bitvector 16 bits serializes to 2 bytes"
    (let ((result (coalton:coalton (ssz:serialize-bitvector (ssz:ssz-bitvector-zeros 16)))))
      (assert (= (length result) 2))))

  (test-case "Bitvector 256 bits serializes to 32 bytes"
    (let ((result (coalton:coalton (ssz:serialize-bitvector (ssz:ssz-bitvector-zeros 256)))))
      (assert (= (length result) 32)))))

;;; =========================================================================
;;; Bitlist Tests
;;; =========================================================================

(defun run-bitlist-tests ()
  (format t "~%=== Bitlist Tests ===~%")

  (test-case "Bitlist empty"
    (let ((result (coalton:coalton (ssz:serialize-bitlist (ssz:ssz-bitlist-empty 100)))))
      (assert (= (length result) 1))
      (assert (= (aref result 0) 1))))

  (test-case "Bitlist max length"
    (let ((max-len (coalton:coalton (ssz:ssz-bitlist-max-length (ssz:ssz-bitlist-empty 256)))))
      (assert (= max-len 256))))

  (test-case "Bitlist current length (empty)"
    (let ((len (coalton:coalton (ssz:ssz-bitlist-length (ssz:ssz-bitlist-empty 100)))))
      (assert (= len 0)))))

;;; =========================================================================
;;; Merkle Tests
;;; =========================================================================

(defun run-merkle-tests ()
  (format t "~%=== Merkle Tests ===~%")

  (test-case "SHA256 produces 32 bytes"
    (let ((result (coalton:coalton (ssz:sha256 (ssz:make-bytes 0)))))
      (assert (= (length result) 32))))

  (test-case "SHA256 empty input (known hash)"
    (let ((result (coalton:coalton (ssz:sha256 (ssz:make-bytes 0)))))
      (assert (= (aref result 0) #xe3))
      (assert (= (aref result 1) #xb0))
      (assert (= (aref result 2) #xc4))
      (assert (= (aref result 3) #x42))))

  (test-case "Zero chunk is 32 zeros"
    (let ((result (coalton:coalton (ssz:zero-chunk coalton:Unit))))
      (assert (= (length result) 32))
      (assert (every #'zerop (coerce result 'list)))))

  (test-case "Hash concat produces 32 bytes"
    (let ((result (coalton:coalton
                   (ssz:hash-concat (ssz:zero-chunk coalton:Unit)
                                    (ssz:zero-chunk coalton:Unit)))))
      (assert (= (length result) 32))))

  (test-case "Pack empty produces one chunk"
    (let ((result (coalton:coalton (ssz:pack (ssz:make-bytes 0)))))
      (assert (= (length result) 1))))

  (test-case "Pack 32 bytes produces one chunk"
    (let ((result (coalton:coalton (ssz:pack (ssz:make-bytes 32)))))
      (assert (= (length result) 1))))

  (test-case "Pack 33 bytes produces two chunks"
    (let ((result (coalton:coalton (ssz:pack (ssz:make-bytes 33)))))
      (assert (= (length result) 2))))

  (test-case "Pack 64 bytes produces two chunks"
    (let ((result (coalton:coalton (ssz:pack (ssz:make-bytes 64)))))
      (assert (= (length result) 2))))

  (test-case "Next power of two: 0 -> 1"
    (let ((result (coalton:coalton (ssz:next-pow-of-two 0))))
      (assert (= result 1))))

  (test-case "Next power of two: 1 -> 1"
    (let ((result (coalton:coalton (ssz:next-pow-of-two 1))))
      (assert (= result 1))))

  (test-case "Next power of two: 2 -> 2"
    (let ((result (coalton:coalton (ssz:next-pow-of-two 2))))
      (assert (= result 2))))

  (test-case "Next power of two: 3 -> 4"
    (let ((result (coalton:coalton (ssz:next-pow-of-two 3))))
      (assert (= result 4))))

  (test-case "Next power of two: 5 -> 8"
    (let ((result (coalton:coalton (ssz:next-pow-of-two 5))))
      (assert (= result 8))))

  (test-case "Next power of two: 16 -> 16"
    (let ((result (coalton:coalton (ssz:next-pow-of-two 16))))
      (assert (= result 16))))

  (test-case "Log2 ceiling: 1 -> 0"
    (let ((result (coalton:coalton (ssz:log2-ceil 1))))
      (assert (= result 0))))

  (test-case "Log2 ceiling: 2 -> 1"
    (let ((result (coalton:coalton (ssz:log2-ceil 2))))
      (assert (= result 1))))

  (test-case "Log2 ceiling: 4 -> 2"
    (let ((result (coalton:coalton (ssz:log2-ceil 4))))
      (assert (= result 2))))

  (test-case "Log2 ceiling: 5 -> 3"
    (let ((result (coalton:coalton (ssz:log2-ceil 5))))
      (assert (= result 3))))

  (test-case "Merkleize single chunk"
    (let ((result (coalton:coalton
                   (ssz:merkleize (coalton:Cons (ssz:zero-chunk coalton:Unit) coalton:Nil)))))
      (assert (= (length result) 32))))

  (test-case "Merkleize empty list"
    (let ((result (coalton:coalton (ssz:merkleize coalton:Nil))))
      (assert (= (length result) 32))))

  (test-case "Zero hash at depth 0"
    (let ((result (coalton:coalton (ssz:zero-hash 0))))
      (assert (every #'zerop (coerce result 'list)))))

  (test-case "Zero hash at depth 1 (hash of two zero chunks)"
    (let ((result (coalton:coalton (ssz:zero-hash 1))))
      (assert (= (length result) 32))
      (assert (not (every #'zerop (coerce result 'list))))))

  (test-case "Chunk count for bitvector"
    (let ((result (coalton:coalton (ssz:chunk-count-bitvector 256))))
      (assert (= result 1))))

  (test-case "Chunk count for bitvector 257 bits"
    (let ((result (coalton:coalton (ssz:chunk-count-bitvector 257))))
      (assert (= result 2)))))

;;; =========================================================================
;;; Container Tests
;;; =========================================================================

(defun run-container-tests ()
  (format t "~%=== Container Tests ===~%")

  (test-case "Fixed field creation"
    (let ((field (coalton:coalton (ssz:fixed-field 4 (ssz:serialize-u32 42)))))
      (assert (typep field 'ssz:SerializedField))))

  (test-case "Variable field creation"
    (let ((field (coalton:coalton (ssz:variable-field (ssz:serialize-u32 42)))))
      (assert (typep field 'ssz:SerializedField))))

  (test-case "FieldType FixedField creation"
    (let ((ft (coalton:coalton (ssz:FixedField 4))))
      (assert (typep ft 'ssz:FieldType))))

  (test-case "FieldType VariableField creation"
    (let ((ft (coalton:coalton ssz:VariableField)))
      (assert (typep ft 'ssz:FieldType))))

  (test-case "Container with all fixed fields - round trip"
    (let* ((f1 (ssz:fixed-field 4 (ssz:serialize-u32 12345)))
           (f2 (ssz:fixed-field 2 (ssz:serialize-u16 678)))
           (f3 (ssz:fixed-field 1 (ssz:serialize-u8 90)))
           (fields (list f1 f2 f3))
           (serialized (ssz:serialize-container fields))
           (ft1 (ssz:FixedField 4))
           (ft2 (ssz:FixedField 2))
           (ft3 (ssz:FixedField 1))
           (field-types (list ft1 ft2 ft3))
           (result (ssz:deserialize-container field-types serialized)))
      (assert (search "OK" (symbol-name (type-of result))))
      (assert (= (length serialized) 7))))

  (test-case "Container with variable fields - round trip"
    (let* ((var-data (make-array 5 :initial-contents '(1 2 3 4 5) :fill-pointer 5 :adjustable t))
           (f1 (ssz:fixed-field 4 (ssz:serialize-u32 999)))
           (f2 (ssz:variable-field var-data))
           (f3 (ssz:fixed-field 2 (ssz:serialize-u16 111)))
           (fields (list f1 f2 f3))
           (serialized (ssz:serialize-container fields))
           (ft1 (ssz:FixedField 4))
           (ft2 ssz:VariableField)
           (ft3 (ssz:FixedField 2))
           (field-types (list ft1 ft2 ft3))
           (result (ssz:deserialize-container field-types serialized)))
      (assert (search "OK" (symbol-name (type-of result))))
      (assert (= (length serialized) 15))))

  (test-case "Container with multiple variable fields - round trip"
    (let* ((var1 (make-array 3 :initial-contents '(10 20 30) :fill-pointer 3 :adjustable t))
           (var2 (make-array 4 :initial-contents '(40 50 60 70) :fill-pointer 4 :adjustable t))
           (f1 (ssz:variable-field var1))
           (f2 (ssz:variable-field var2))
           (fields (list f1 f2))
           (serialized (ssz:serialize-container fields))
           (ft1 ssz:VariableField)
           (ft2 ssz:VariableField)
           (field-types (list ft1 ft2))
           (result (ssz:deserialize-container field-types serialized)))
      (assert (search "OK" (symbol-name (type-of result))))
      (assert (= (length serialized) 15))))

  (test-case "Container deserialization - insufficient data error"
    (let* ((short-data (make-array 5 :initial-contents '(1 2 3 4 5) :fill-pointer 5 :adjustable t))
           (ft1 (ssz:FixedField 4))
           (ft2 (ssz:FixedField 4))
           (field-types (list ft1 ft2))
           (result (ssz:deserialize-container field-types short-data)))
      (assert (search "ERR" (symbol-name (type-of result))))))

  (test-case "Empty container - round trip"
    (let* ((serialized (ssz:serialize-container nil))
           (result (ssz:deserialize-container nil serialized)))
      (assert (search "OK" (symbol-name (type-of result))))
      (assert (= (length serialized) 0)))))

;;; =========================================================================
;;; Hash Tree Root Tests
;;; =========================================================================

(defun run-hash-tree-root-tests ()
  (format t "~%=== Hash Tree Root Tests ===~%")

  (test-case "Hash tree root of Boolean True"
    (let ((result (coalton:coalton (ssz:hash-tree-root coalton:True))))
      (assert (= (length result) 32))))

  (test-case "Hash tree root of Boolean False"
    (let ((result (coalton:coalton (ssz:hash-tree-root coalton:False))))
      (assert (= (length result) 32))))

  (test-case "Different booleans produce different roots"
    (let ((root1 (coalton:coalton (ssz:hash-tree-root coalton:True)))
          (root2 (coalton:coalton (ssz:hash-tree-root coalton:False))))
      (assert (not (bytes-equal root1 root2)))))

  (test-case "Same booleans produce same roots"
    (let ((root1 (coalton:coalton (ssz:hash-tree-root coalton:True)))
          (root2 (coalton:coalton (ssz:hash-tree-root coalton:True))))
      (assert (bytes-equal root1 root2))))

  (test-case "Hash tree root of bitvector"
    (let ((result (coalton:coalton (ssz:hash-tree-root (ssz:ssz-bitvector-zeros 64)))))
      (assert (= (length result) 32))))

  (test-case "Hash tree root of bitlist"
    (let ((result (coalton:coalton (ssz:hash-tree-root (ssz:ssz-bitlist-empty 100)))))
      (assert (= (length result) 32))))

  (test-case "Mix-in-length produces 32 bytes"
    (let ((result (coalton:coalton
                   (ssz:mix-in-length (ssz:zero-chunk coalton:Unit) 10))))
      (assert (= (length result) 32))))

  (test-case "Mix-in-length changes the root"
    (let ((chunk (coalton:coalton (ssz:zero-chunk coalton:Unit)))
          (mixed (coalton:coalton (ssz:mix-in-length (ssz:zero-chunk coalton:Unit) 10))))
      (assert (not (bytes-equal chunk mixed)))))

  (test-case "Container hash tree root"
    (let ((result (coalton:coalton
                   (ssz:hash-tree-root-container
                    (coalton:Cons (ssz:hash-tree-root coalton:True)
                                  (coalton:Cons (ssz:hash-tree-root coalton:False) coalton:Nil))))))
      (assert (= (length result) 32)))))

;;; =========================================================================
;;; Vector and List Type Class Instance Tests
;;; =========================================================================

(defun run-vector-list-tests ()
  (format t "~%=== Vector/List Type Class Tests ===~%")

  (test-case "Vector serialize - produces correct length"
    (let ((result (ssz-tests:ssz-tests-vector-serialize-length coalton:Unit)))
      (assert (= result 12))))

  (test-case "Vector element count - correct"
    (let ((result (ssz-tests:ssz-tests-vector-element-count coalton:Unit)))
      (assert (= result 3))))

  (test-case "Vector fixed-size? - returns true for fixed elements"
    (let ((result (ssz-tests:ssz-tests-vector-fixed-size-p coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Vector fixed-size - returns correct size"
    (let ((result (ssz-tests:ssz-tests-vector-fixed-size coalton:Unit)))
      (assert (= result 12))))

  (test-case "Vector hash-tree-root - produces 32 bytes"
    (let ((result (ssz-tests:ssz-tests-vector-hash-tree-root coalton:Unit)))
      (assert (= (length result) 32))))

  (test-case "List serialize - produces correct length"
    (let ((result (ssz-tests:ssz-tests-list-serialize-length coalton:Unit)))
      (assert (= result 8))))

  (test-case "List fixed-size? - always returns false"
    (let ((result (ssz-tests:ssz-tests-list-fixed-size-p coalton:Unit)))
      (assert (eq result coalton:False))))

  (test-case "List hash-tree-root - produces 32 bytes"
    (let ((result (ssz-tests:ssz-tests-list-hash-tree-root coalton:Unit)))
      (assert (= (length result) 32))))

  (test-case "Empty vector serialize - produces 0 bytes"
    (let ((result (ssz-tests:ssz-tests-empty-vector-serialize coalton:Unit)))
      (assert (= result 0))))

  (test-case "Empty list serialize - produces 0 bytes"
    (let ((result (ssz-tests:ssz-tests-empty-list-serialize coalton:Unit)))
      (assert (= result 0))))

  (test-case "Vector U32 roundtrip - correct element count"
    (let ((result (ssz-tests:ssz-tests-vector-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Vector U32 roundtrip - preserves values"
    (let ((result (ssz-tests:ssz-tests-vector-roundtrip-values coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "List U32 roundtrip - correct element count"
    (let ((result (ssz-tests:ssz-tests-list-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "List U32 roundtrip - preserves values"
    (let ((result (ssz-tests:ssz-tests-list-roundtrip-values coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Empty vector roundtrip"
    (let ((result (ssz-tests:ssz-tests-empty-vector-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Empty list roundtrip"
    (let ((result (ssz-tests:ssz-tests-empty-list-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Vector U8 roundtrip"
    (let ((result (ssz-tests:ssz-tests-vector-u8-roundtrip coalton:Unit)))
      (assert (eq result coalton:True)))))

;;; =========================================================================
;;; Union Tests
;;; =========================================================================

(defun run-union-tests ()
  (format t "~%=== SSZ Union Tests ===~%")

  (test-case "Union None - serializes to 1 byte"
    (let ((result (ssz-tests:ssz-tests-union-none-serialize coalton:Unit)))
      (assert (= result 1))))

  (test-case "Union None - has selector 0"
    (let ((result (ssz-tests:ssz-tests-union-none-selector coalton:Unit)))
      (assert (= result 0))))

  (test-case "Union None - is-none returns true"
    (let ((result (ssz-tests:ssz-tests-union-none-is-none coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Union Some - serializes to 5 bytes (1 selector + 4 U32)"
    (let ((result (ssz-tests:ssz-tests-union-some-serialize-length coalton:Unit)))
      (assert (= result 5))))

  (test-case "Union Some - has correct selector"
    (let ((result (ssz-tests:ssz-tests-union-some-selector coalton:Unit)))
      (assert (= result 5))))

  (test-case "Union Some - is-none returns false"
    (let ((result (ssz-tests:ssz-tests-union-some-is-none coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Union Some - roundtrip preserves selector"
    (let ((result (ssz-tests:ssz-tests-union-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Union None - roundtrip preserves None"
    (let ((result (ssz-tests:ssz-tests-union-none-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Union Some - hash tree root is 32 bytes"
    (let ((result (ssz-tests:ssz-tests-union-hash-tree-root-length coalton:Unit)))
      (assert (= result 32))))

  (test-case "Union None - hash tree root is 32 bytes"
    (let ((result (ssz-tests:ssz-tests-union-none-hash-tree-root-length coalton:Unit)))
      (assert (= result 32))))

  (test-case "Union - selector > 127 is rejected"
    (let ((result (ssz-tests:ssz-tests-union-selector-validation coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Union - selector 0 for Some is rejected"
    (let ((result (ssz-tests:ssz-tests-union-selector-0-validation coalton:Unit)))
      (assert (eq result coalton:True)))))

;;; =========================================================================
;;; Comprehensive Roundtrip Tests
;;; =========================================================================

(defun run-roundtrip-tests ()
  (format t "~%=== Comprehensive Roundtrip Tests ===~%")

  (test-case "U8 roundtrip - value preserved"
    (let ((result (ssz-tests:ssz-tests-u8-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "U16 roundtrip - value preserved"
    (let ((result (ssz-tests:ssz-tests-u16-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "U32 roundtrip - value preserved"
    (let ((result (ssz-tests:ssz-tests-u32-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "U64 roundtrip - value preserved"
    (let ((result (ssz-tests:ssz-tests-u64-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "U128 roundtrip - value preserved"
    (let ((result (ssz-tests:ssz-tests-u128-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "U256 roundtrip - value preserved"
    (let ((result (ssz-tests:ssz-tests-u256-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Boolean True roundtrip - value preserved"
    (let ((result (ssz-tests:ssz-tests-bool-true-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Boolean False roundtrip - value preserved"
    (let ((result (ssz-tests:ssz-tests-bool-false-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bitvector 8-bit roundtrip"
    (let ((result (ssz-tests:ssz-tests-bitvector-8-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bitvector 64-bit roundtrip"
    (let ((result (ssz-tests:ssz-tests-bitvector-64-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bitvector 256-bit roundtrip"
    (let ((result (ssz-tests:ssz-tests-bitvector-256-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bitvector with set bits roundtrip"
    (let ((result (ssz-tests:ssz-tests-bitvector-with-bits-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bitlist empty roundtrip"
    (let ((result (ssz-tests:ssz-tests-bitlist-empty-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bitlist with bits roundtrip"
    (let ((result (ssz-tests:ssz-tests-bitlist-with-bits-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Vector[U8] roundtrip"
    (let ((result (ssz-tests:ssz-tests-vector-u8-full-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Vector[U16] roundtrip"
    (let ((result (ssz-tests:ssz-tests-vector-u16-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Vector[U64] roundtrip"
    (let ((result (ssz-tests:ssz-tests-vector-u64-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Vector[Boolean] roundtrip"
    (let ((result (ssz-tests:ssz-tests-vector-bool-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "List[U8] roundtrip"
    (let ((result (ssz-tests:ssz-tests-list-u8-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "List[U16] roundtrip"
    (let ((result (ssz-tests:ssz-tests-list-u16-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "List[U64] roundtrip"
    (let ((result (ssz-tests:ssz-tests-list-u64-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "List[Boolean] roundtrip"
    (let ((result (ssz-tests:ssz-tests-list-bool-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "U8 max value roundtrip"
    (let ((result (ssz-tests:ssz-tests-u8-max-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "U16 max value roundtrip"
    (let ((result (ssz-tests:ssz-tests-u16-max-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "U32 max value roundtrip"
    (let ((result (ssz-tests:ssz-tests-u32-max-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "U64 max value roundtrip"
    (let ((result (ssz-tests:ssz-tests-u64-max-roundtrip coalton:Unit)))
      (assert (eq result coalton:True)))))

;;; =========================================================================
;;; Edge Case Tests
;;; =========================================================================

(defun run-edge-case-tests ()
  (format t "~%=== Edge Case Tests ===~%")

  (test-case "Vector single element roundtrip"
    (let ((result (ssz-tests:ssz-tests-vector-single-element coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "List single element roundtrip"
    (let ((result (ssz-tests:ssz-tests-list-single-element coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bitvector 255-bit roundtrip (chunk boundary -1)"
    (let ((result (ssz-tests:ssz-tests-bitvector-255-bits coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bitvector 257-bit roundtrip (chunk boundary +1)"
    (let ((result (ssz-tests:ssz-tests-bitvector-257-bits coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bitvector single bit roundtrip"
    (let ((result (ssz-tests:ssz-tests-bitvector-single-bit coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bitlist single bit roundtrip"
    (let ((result (ssz-tests:ssz-tests-bitlist-single-bit coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bitlist 7 bits roundtrip (byte boundary -1)"
    (let ((result (ssz-tests:ssz-tests-bitlist-7-bits coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bitlist 8 bits roundtrip (byte boundary)"
    (let ((result (ssz-tests:ssz-tests-bitlist-8-bits coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Pack 31 bytes produces 1 chunk"
    (let ((result (ssz-tests:ssz-tests-pack-31-bytes coalton:Unit)))
      (assert (= result 1))))

  (test-case "Pack 32 bytes produces 1 chunk"
    (let ((result (ssz-tests:ssz-tests-pack-32-bytes coalton:Unit)))
      (assert (= result 1))))

  (test-case "Pack 33 bytes produces 2 chunks"
    (let ((result (ssz-tests:ssz-tests-pack-33-bytes coalton:Unit)))
      (assert (= result 2))))

  (test-case "Pack 64 bytes produces 2 chunks"
    (let ((result (ssz-tests:ssz-tests-pack-64-bytes coalton:Unit)))
      (assert (= result 2))))

  (test-case "Deserialize U32 with truncated data returns error"
    (let ((result (ssz-tests:ssz-tests-deserialize-u32-truncated coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Deserialize U64 with truncated data returns error"
    (let ((result (ssz-tests:ssz-tests-deserialize-u64-truncated coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Deserialize bool value 2 returns error"
    (let ((result (ssz-tests:ssz-tests-deserialize-bool-invalid-2 coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Deserialize bool value 255 returns error"
    (let ((result (ssz-tests:ssz-tests-deserialize-bool-invalid-255 coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bytes slice full array"
    (let ((result (ssz-tests:ssz-tests-bytes-slice-full coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bytes slice empty (zero length)"
    (let ((result (ssz-tests:ssz-tests-bytes-slice-empty coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Zero hash depth 0 is 32 zero bytes"
    (let ((result (ssz-tests:ssz-tests-zero-hash-depth-0 coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Zero hash depth 1 is hash(zero, zero)"
    (let ((result (ssz-tests:ssz-tests-zero-hash-depth-1 coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Merkleize single chunk"
    (let ((result (ssz-tests:ssz-tests-merkleize-single-chunk coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Merkleize two zero chunks equals zero-hash(1)"
    (let ((result (ssz-tests:ssz-tests-merkleize-two-chunks coalton:Unit)))
      (assert (eq result coalton:True)))))

;;; =========================================================================
;;; Additional Coverage Tests
;;; =========================================================================

(defun run-additional-coverage-tests ()
  (format t "~%=== Additional Coverage Tests ===~%")

  ;; U128/U256 max value roundtrips
  (test-case "U128 max value roundtrip (all 0xFF)"
    (let ((result (ssz-tests:ssz-tests-u128-max-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "U256 max value roundtrip (all 0xFF)"
    (let ((result (ssz-tests:ssz-tests-u256-max-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  ;; Validation rejection tests
  (test-case "SSZList from-list rejects over-max-length"
    (let ((result (ssz-tests:ssz-tests-list-from-list-reject coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Bitlist rejects bits exceeding max-length"
    (let ((result (ssz-tests:ssz-tests-bitlist-max-reject coalton:Unit)))
      (assert (eq result coalton:True))))

  ;; Mix-in-selector
  (test-case "Mix-in-selector produces 32 bytes and changes root"
    (let ((result (ssz-tests:ssz-tests-mix-in-selector coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Mix-in-selector different selectors differ"
    (let ((result (ssz-tests:ssz-tests-mix-in-selector-differs coalton:Unit)))
      (assert (eq result coalton:True))))

  ;; Merkleize-with-limit
  (test-case "Merkleize-with-limit pads to limit"
    (let ((result (ssz-tests:ssz-tests-merkleize-with-limit coalton:Unit)))
      (assert (eq result coalton:True))))

  ;; Bytes-set!
  (test-case "Bytes-set! writes correct values"
    (let ((result (ssz-tests:ssz-tests-bytes-set coalton:Unit)))
      (assert (eq result coalton:True))))

  ;; Union value extraction
  (test-case "Union roundtrip preserves inner value"
    (let ((result (ssz-tests:ssz-tests-union-value-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  ;; Large collections
  (test-case "Vector 128 U8 elements roundtrip"
    (let ((result (ssz-tests:ssz-tests-large-vector-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "List 200 U16 elements roundtrip"
    (let ((result (ssz-tests:ssz-tests-large-list-roundtrip coalton:Unit)))
      (assert (eq result coalton:True))))

  ;; Container hash-tree-root
  (test-case "Container hash-tree-root with U32/U64 fields"
    (let ((result (ssz-tests:ssz-tests-container-htr-u32 coalton:Unit)))
      (assert (eq result coalton:True))))

  (test-case "Container hash-tree-root differs for different values"
    (let ((result (ssz-tests:ssz-tests-container-htr-differs coalton:Unit)))
      (assert (eq result coalton:True))))

  ;; Container deserialization value verification (pure CL)
  ;; Coalton lists are CL lists, Result/Ok wraps in slot _0
  (test-case "Container deserialization - field values correct"
    (let* ((f1 (ssz:fixed-field 4 (ssz:serialize-u32 12345)))
           (f2 (ssz:fixed-field 2 (ssz:serialize-u16 678)))
           (serialized (ssz:serialize-container (list f1 f2)))
           (field-types (list (ssz:FixedField 4) (ssz:FixedField 2)))
           (result (ssz:deserialize-container field-types serialized)))
      (assert (search "OK" (symbol-name (type-of result))))
      ;; Coalton Ok wraps value in _0, and DeserializedFields is a CL list
      (let* ((fields (slot-value result 'coalton-library/classes::|_0|))
             (field0 (first fields))
             (field1 (second fields)))
        (let ((val0 (ssz:deserialize-u32 field0))
              (val1 (ssz:deserialize-u16 field1)))
          (assert (= (slot-value val0 'coalton-library/classes::|_0|) 12345)
                  nil "Expected 12345, got ~A" (slot-value val0 'coalton-library/classes::|_0|))
          (assert (= (slot-value val1 'coalton-library/classes::|_0|) 678)
                  nil "Expected 678, got ~A" (slot-value val1 'coalton-library/classes::|_0|))))))

  ;; Container with variable field - value verification
  (test-case "Container variable field deserialization - values correct"
    (let* ((var-data (make-array 5 :initial-contents '(10 20 30 40 50)
                                 :fill-pointer 5 :adjustable t))
           (f1 (ssz:fixed-field 4 (ssz:serialize-u32 999)))
           (f2 (ssz:variable-field var-data))
           (serialized (ssz:serialize-container (list f1 f2)))
           (field-types (list (ssz:FixedField 4) ssz:VariableField))
           (result (ssz:deserialize-container field-types serialized)))
      (assert (search "OK" (symbol-name (type-of result))))
      (let* ((fields (slot-value result 'coalton-library/classes::|_0|))
             (field0 (first fields))
             (var-bytes (second fields)))
        (let ((val0 (ssz:deserialize-u32 field0)))
          (assert (= (slot-value val0 'coalton-library/classes::|_0|) 999))
          (assert (= (length var-bytes) 5))
          (assert (= (aref var-bytes 0) 10))
          (assert (= (aref var-bytes 4) 50))))))

  ;; Validate-container-offsets
  (test-case "Validate-container-offsets accepts valid offsets"
    (let ((result (coalton:coalton
                   (ssz:validate-container-offsets
                    (coalton:Cons 8 (coalton:Cons 12 coalton:Nil))
                    8 16))))
      (assert (search "OK" (symbol-name (type-of result))))))

  (test-case "Validate-container-offsets rejects wrong first offset"
    (let ((result (coalton:coalton
                   (ssz:validate-container-offsets
                    (coalton:Cons 10 coalton:Nil)
                    8 16))))
      (assert (search "ERR" (symbol-name (type-of result))))))

  (test-case "Validate-container-offsets rejects out-of-order offsets"
    (let ((result (coalton:coalton
                   (ssz:validate-container-offsets
                    (coalton:Cons 8 (coalton:Cons 6 coalton:Nil))
                    8 16))))
      (assert (search "ERR" (symbol-name (type-of result))))))

  (test-case "Validate-container-offsets rejects offset exceeding data"
    (let ((result (coalton:coalton
                   (ssz:validate-container-offsets
                    (coalton:Cons 8 coalton:Nil)
                    8 4))))
      (assert (search "ERR" (symbol-name (type-of result))))))

  (test-case "Validate-container-offsets accepts empty offsets"
    (let ((result (coalton:coalton
                   (ssz:validate-container-offsets
                    coalton:Nil 0 0))))
      (assert (search "OK" (symbol-name (type-of result)))))))
