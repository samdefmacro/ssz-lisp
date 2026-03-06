;;; Coalton test helpers - defined in SSZ-TESTS package
;;; These helpers test the SSZ library functionality

(cl:in-package #:ssz-tests)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare ssz-tests-vector-serialize-length (Unit -> UFix))
  (define (ssz-tests-vector-serialize-length _)
    "Test Vector[U32] serialize length"
    (let ((vec (the (ssz:SSZVector U32) (ssz:SSZVector (Cons 100 (Cons 200 (Cons 300 Nil)))))))
      (ssz:bytes-length (ssz:ssz-serialize vec))))

  (declare ssz-tests-vector-fixed-size-p (Unit -> Boolean))
  (define (ssz-tests-vector-fixed-size-p _)
    "Test Vector[U32] fixed-size?"
    (let ((vec (the (ssz:SSZVector U32) (ssz:SSZVector (Cons 1 Nil)))))
      (ssz:ssz-fixed-size? vec)))

  (declare ssz-tests-vector-fixed-size (Unit -> UFix))
  (define (ssz-tests-vector-fixed-size _)
    "Test Vector[U32] fixed-size"
    (let ((vec (the (ssz:SSZVector U32) (ssz:SSZVector (Cons 1 (Cons 2 (Cons 3 Nil)))))))
      (ssz:ssz-fixed-size vec)))

  (declare ssz-tests-vector-hash-tree-root (Unit -> ssz:Chunk))
  (define (ssz-tests-vector-hash-tree-root _)
    "Test Vector hash tree root"
    (let ((vec (the (ssz:SSZVector U32) (ssz:SSZVector (Cons 1 (Cons 2 Nil))))))
      (ssz:hash-tree-root vec)))

  (declare ssz-tests-list-serialize-length (Unit -> UFix))
  (define (ssz-tests-list-serialize-length _)
    "Test List[U32] serialize length"
    (let ((lst (the (ssz:SSZList U32) (ssz:SSZList 100 (Cons 77 (Cons 88 Nil))))))
      (ssz:bytes-length (ssz:ssz-serialize lst))))

  (declare ssz-tests-list-fixed-size-p (Unit -> Boolean))
  (define (ssz-tests-list-fixed-size-p _)
    "Test List[U32] fixed-size?"
    (let ((lst (the (ssz:SSZList U32) (ssz:SSZList 100 (Cons 1 Nil)))))
      (ssz:ssz-fixed-size? lst)))

  (declare ssz-tests-list-hash-tree-root (Unit -> ssz:Chunk))
  (define (ssz-tests-list-hash-tree-root _)
    "Test List hash tree root"
    (let ((lst (the (ssz:SSZList U32) (ssz:SSZList 100 (Cons 42 Nil)))))
      (ssz:hash-tree-root lst)))

  (declare ssz-tests-empty-vector-serialize (Unit -> UFix))
  (define (ssz-tests-empty-vector-serialize _)
    "Test empty Vector serialize"
    (let ((vec (the (ssz:SSZVector U32) (ssz:SSZVector Nil))))
      (ssz:bytes-length (ssz:ssz-serialize vec))))

  (declare ssz-tests-empty-list-serialize (Unit -> UFix))
  (define (ssz-tests-empty-list-serialize _)
    "Test empty List serialize"
    (let ((lst (the (ssz:SSZList U32) (ssz:SSZList 100 Nil))))
      (ssz:bytes-length (ssz:ssz-serialize lst))))

  (declare ssz-tests-vector-element-count (Unit -> UFix))
  (define (ssz-tests-vector-element-count _)
    "Test Vector element count"
    (let ((vec (the (ssz:SSZVector U32) (ssz:SSZVector (Cons 100 (Cons 200 (Cons 300 Nil)))))))
      (ssz:ssz-vector-length vec)))

  ;;; =========================================================================
  ;;; Deserialization test helpers
  ;;; =========================================================================

  (declare ssz-tests-vector-roundtrip (Unit -> Boolean))
  (define (ssz-tests-vector-roundtrip _)
    "Test Vector serialize then deserialize roundtrip"
    (let ((vec (the (ssz:SSZVector U32) (ssz:SSZVector (Cons 100 (Cons 200 (Cons 300 Nil)))))))
      (let ((serialized (ssz:ssz-serialize vec)))
        (match (the (ssz:SSZResult (ssz:SSZVector U32)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok (ssz:SSZVector elements))
           ;; Check we got 3 elements back
           (== (lisp UFix (elements) (cl:length elements)) 3))))))

  (declare ssz-tests-vector-roundtrip-values (Unit -> Boolean))
  (define (ssz-tests-vector-roundtrip-values _)
    "Test Vector roundtrip preserves values"
    (let ((vec (the (ssz:SSZVector U32) (ssz:SSZVector (Cons 100 (Cons 200 (Cons 300 Nil)))))))
      (let ((serialized (ssz:ssz-serialize vec)))
        (match (the (ssz:SSZResult (ssz:SSZVector U32)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok (ssz:SSZVector elements))
           ;; Check values using lisp to extract
           (lisp Boolean (elements)
             (cl:and (cl:= (cl:length elements) 3)
                     (cl:= (cl:first elements) 100)
                     (cl:= (cl:second elements) 200)
                     (cl:= (cl:third elements) 300))))))))

  (declare ssz-tests-list-roundtrip (Unit -> Boolean))
  (define (ssz-tests-list-roundtrip _)
    "Test List serialize then deserialize roundtrip"
    (let ((lst (the (ssz:SSZList U32) (ssz:SSZList 100 (Cons 42 (Cons 99 Nil))))))
      (let ((serialized (ssz:ssz-serialize lst)))
        (match (the (ssz:SSZResult (ssz:SSZList U32)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok (ssz:SSZList _ elements))
           ;; Check we got 2 elements back
           (== (lisp UFix (elements) (cl:length elements)) 2))))))

  (declare ssz-tests-list-roundtrip-values (Unit -> Boolean))
  (define (ssz-tests-list-roundtrip-values _)
    "Test List roundtrip preserves values"
    (let ((lst (the (ssz:SSZList U32) (ssz:SSZList 100 (Cons 42 (Cons 99 Nil))))))
      (let ((serialized (ssz:ssz-serialize lst)))
        (match (the (ssz:SSZResult (ssz:SSZList U32)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok (ssz:SSZList _ elements))
           ;; Check values using lisp to extract
           (lisp Boolean (elements)
             (cl:and (cl:= (cl:length elements) 2)
                     (cl:= (cl:first elements) 42)
                     (cl:= (cl:second elements) 99))))))))

  (declare ssz-tests-empty-vector-roundtrip (Unit -> Boolean))
  (define (ssz-tests-empty-vector-roundtrip _)
    "Test empty Vector roundtrip"
    (let ((vec (the (ssz:SSZVector U32) (ssz:SSZVector Nil))))
      (let ((serialized (ssz:ssz-serialize vec)))
        (match (the (ssz:SSZResult (ssz:SSZVector U32)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok (ssz:SSZVector elements))
           (lisp Boolean (elements) (cl:null elements)))))))

  (declare ssz-tests-empty-list-roundtrip (Unit -> Boolean))
  (define (ssz-tests-empty-list-roundtrip _)
    "Test empty List roundtrip"
    (let ((lst (the (ssz:SSZList U32) (ssz:SSZList 100 Nil))))
      (let ((serialized (ssz:ssz-serialize lst)))
        (match (the (ssz:SSZResult (ssz:SSZList U32)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok (ssz:SSZList _ elements))
           (lisp Boolean (elements) (cl:null elements)))))))

  (declare ssz-tests-vector-u8-roundtrip (Unit -> Boolean))
  (define (ssz-tests-vector-u8-roundtrip _)
    "Test Vector of U8 roundtrip"
    (let ((vec (the (ssz:SSZVector U8) (ssz:SSZVector (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))))))
      (let ((serialized (ssz:ssz-serialize vec)))
        (match (the (ssz:SSZResult (ssz:SSZVector U8)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok (ssz:SSZVector elements))
           (== (lisp UFix (elements) (cl:length elements)) 4))))))

  ;;; SSZUnion test helpers

  (declare ssz-tests-union-none-serialize (Unit -> UFix))
  (define (ssz-tests-union-none-serialize _)
    "Test None union serialization - should produce 1 byte (selector only)"
    (ssz:bytes-length (ssz:ssz-serialize (ssz:ssz-union-none Unit))))

  (declare ssz-tests-union-none-selector (Unit -> U8))
  (define (ssz-tests-union-none-selector _)
    "Test None union has selector 0"
    (ssz:ssz-union-selector (ssz:ssz-union-none Unit)))

  (declare ssz-tests-union-none-is-none (Unit -> Boolean))
  (define (ssz-tests-union-none-is-none _)
    "Test ssz-union-is-none returns True for None union"
    (ssz:ssz-union-is-none (ssz:ssz-union-none Unit)))

  (declare ssz-tests-union-some-serialize-length (Unit -> UFix))
  (define (ssz-tests-union-some-serialize-length _)
    "Test Some union serialization length - selector (1) + value (4 bytes for U32)"
    (match (ssz:ssz-union-from-value 1 (the U32 42))
      ((Err _) 0)
      ((Ok union) (ssz:bytes-length (ssz:ssz-serialize union)))))

  (declare ssz-tests-union-some-selector (Unit -> U8))
  (define (ssz-tests-union-some-selector _)
    "Test Some union has correct selector"
    (match (ssz:ssz-union-from-value 5 (the U32 42))
      ((Err _) 0)
      ((Ok union) (ssz:ssz-union-selector union))))

  (declare ssz-tests-union-some-is-none (Unit -> Boolean))
  (define (ssz-tests-union-some-is-none _)
    "Test ssz-union-is-none returns False for Some union"
    (match (ssz:ssz-union-from-value 1 (the U32 42))
      ((Err _) True)  ;; if error, fail the test
      ((Ok union) (not (ssz:ssz-union-is-none union)))))

  (declare ssz-tests-union-roundtrip (Unit -> Boolean))
  (define (ssz-tests-union-roundtrip _)
    "Test union roundtrip (serialize then deserialize)"
    (match (ssz:ssz-union-from-value 3 (the U32 12345))
      ((Err _) False)
      ((Ok union)
       (let ((serialized (ssz:ssz-serialize union)))
         (match (ssz:ssz-deserialize serialized)
           ((Err _) False)
           ((Ok deserialized)
            (and (== (ssz:ssz-union-selector deserialized) 3)
                 (not (ssz:ssz-union-is-none deserialized)))))))))

  (declare ssz-tests-union-none-roundtrip (Unit -> Boolean))
  (define (ssz-tests-union-none-roundtrip _)
    "Test None union roundtrip"
    (let ((none-union (ssz:ssz-union-none Unit)))
      (let ((serialized (ssz:ssz-serialize none-union)))
        (match (ssz:ssz-deserialize serialized)
          ((Err _) False)
          ((Ok deserialized)
           (and (== (ssz:ssz-union-selector deserialized) 0)
                (ssz:ssz-union-is-none deserialized)))))))

  (declare ssz-tests-union-hash-tree-root-length (Unit -> UFix))
  (define (ssz-tests-union-hash-tree-root-length _)
    "Test union hash tree root produces 32 bytes"
    (match (ssz:ssz-union-from-value 1 (the U32 42))
      ((Err _) 0)
      ((Ok union) (ssz:bytes-length (ssz:hash-tree-root union)))))

  (declare ssz-tests-union-none-hash-tree-root-length (Unit -> UFix))
  (define (ssz-tests-union-none-hash-tree-root-length _)
    "Test None union hash tree root produces 32 bytes"
    (ssz:bytes-length (ssz:hash-tree-root (ssz:ssz-union-none Unit))))

  (declare ssz-tests-union-selector-validation (Unit -> Boolean))
  (define (ssz-tests-union-selector-validation _)
    "Test that selectors > 127 are rejected"
    (match (ssz:ssz-union-some 128 (ssz:make-bytes 4))
      ((Err _) True)   ;; Expected: error for selector > 127
      ((Ok _) False)))

  (declare ssz-tests-union-selector-0-validation (Unit -> Boolean))
  (define (ssz-tests-union-selector-0-validation _)
    "Test that selector 0 is rejected for ssz-union-some"
    (match (ssz:ssz-union-some 0 (ssz:make-bytes 4))
      ((Err _) True)   ;; Expected: error for selector 0
      ((Ok _) False)))

  ;;; =========================================================================
  ;;; Comprehensive Roundtrip Test Helpers
  ;;; =========================================================================

  ;; Basic type roundtrips with value verification
  (declare ssz-tests-u8-roundtrip (Unit -> Boolean))
  (define (ssz-tests-u8-roundtrip _)
    (let ((val (the U8 42)))
      (match (ssz:deserialize-u8 (ssz:serialize-u8 val))
        ((Err _) False)
        ((Ok result) (== result val)))))

  (declare ssz-tests-u16-roundtrip (Unit -> Boolean))
  (define (ssz-tests-u16-roundtrip _)
    (let ((val (the U16 12345)))
      (match (ssz:deserialize-u16 (ssz:serialize-u16 val))
        ((Err _) False)
        ((Ok result) (== result val)))))

  (declare ssz-tests-u32-roundtrip (Unit -> Boolean))
  (define (ssz-tests-u32-roundtrip _)
    (let ((val (the U32 123456789)))
      (match (ssz:deserialize-u32 (ssz:serialize-u32 val))
        ((Err _) False)
        ((Ok result) (== result val)))))

  (declare ssz-tests-u64-roundtrip (Unit -> Boolean))
  (define (ssz-tests-u64-roundtrip _)
    (let ((val (the U64 9876543210)))
      (match (ssz:deserialize-u64 (ssz:serialize-u64 val))
        ((Err _) False)
        ((Ok result) (== result val)))))

  (declare ssz-tests-u128-roundtrip (Unit -> Boolean))
  (define (ssz-tests-u128-roundtrip _)
    (let ((val (ssz:u128-make 12345678 87654321)))
      (match (ssz:deserialize-u128 (ssz:serialize-u128 val))
        ((Err _) False)
        ((Ok result)
         (and (== (ssz:u128-word 0 result) (ssz:u128-word 0 val))
              (== (ssz:u128-word 1 result) (ssz:u128-word 1 val)))))))

  (declare ssz-tests-u256-roundtrip (Unit -> Boolean))
  (define (ssz-tests-u256-roundtrip _)
    (let ((val (ssz:u256-make 111 222 333 444)))
      (match (ssz:deserialize-u256 (ssz:serialize-u256 val))
        ((Err _) False)
        ((Ok result)
         (and (== (ssz:u256-word 0 result) (ssz:u256-word 0 val))
              (== (ssz:u256-word 1 result) (ssz:u256-word 1 val))
              (== (ssz:u256-word 2 result) (ssz:u256-word 2 val))
              (== (ssz:u256-word 3 result) (ssz:u256-word 3 val)))))))

  (declare ssz-tests-bool-true-roundtrip (Unit -> Boolean))
  (define (ssz-tests-bool-true-roundtrip _)
    (match (ssz:deserialize-bool (ssz:serialize-bool True))
      ((Err _) False)
      ((Ok result) result)))

  (declare ssz-tests-bool-false-roundtrip (Unit -> Boolean))
  (define (ssz-tests-bool-false-roundtrip _)
    (match (ssz:deserialize-bool (ssz:serialize-bool False))
      ((Err _) False)
      ((Ok result) (not result))))

  ;; Max value roundtrips
  (declare ssz-tests-u8-max-roundtrip (Unit -> Boolean))
  (define (ssz-tests-u8-max-roundtrip _)
    (let ((val (the U8 255)))
      (match (ssz:deserialize-u8 (ssz:serialize-u8 val))
        ((Err _) False)
        ((Ok result) (== result val)))))

  (declare ssz-tests-u16-max-roundtrip (Unit -> Boolean))
  (define (ssz-tests-u16-max-roundtrip _)
    (let ((val (the U16 65535)))
      (match (ssz:deserialize-u16 (ssz:serialize-u16 val))
        ((Err _) False)
        ((Ok result) (== result val)))))

  (declare ssz-tests-u32-max-roundtrip (Unit -> Boolean))
  (define (ssz-tests-u32-max-roundtrip _)
    (let ((val (the U32 4294967295)))
      (match (ssz:deserialize-u32 (ssz:serialize-u32 val))
        ((Err _) False)
        ((Ok result) (== result val)))))

  (declare ssz-tests-u64-max-roundtrip (Unit -> Boolean))
  (define (ssz-tests-u64-max-roundtrip _)
    (let ((val (the U64 18446744073709551615)))
      (match (ssz:deserialize-u64 (ssz:serialize-u64 val))
        ((Err _) False)
        ((Ok result) (== result val)))))

  ;; Bitvector roundtrips
  (declare ssz-tests-bitvector-8-roundtrip (Unit -> Boolean))
  (define (ssz-tests-bitvector-8-roundtrip _)
    (let ((bv (ssz:ssz-bitvector-zeros 8)))
      (match (ssz:deserialize-bitvector 8 (ssz:serialize-bitvector bv))
        ((Err _) False)
        ((Ok result) (== (ssz:ssz-bitvector-length result) 8)))))

  (declare ssz-tests-bitvector-64-roundtrip (Unit -> Boolean))
  (define (ssz-tests-bitvector-64-roundtrip _)
    (let ((bv (ssz:ssz-bitvector-zeros 64)))
      (match (ssz:deserialize-bitvector 64 (ssz:serialize-bitvector bv))
        ((Err _) False)
        ((Ok result) (== (ssz:ssz-bitvector-length result) 64)))))

  (declare ssz-tests-bitvector-256-roundtrip (Unit -> Boolean))
  (define (ssz-tests-bitvector-256-roundtrip _)
    (let ((bv (ssz:ssz-bitvector-zeros 256)))
      (match (ssz:deserialize-bitvector 256 (ssz:serialize-bitvector bv))
        ((Err _) False)
        ((Ok result) (== (ssz:ssz-bitvector-length result) 256)))))

  (declare ssz-tests-bitvector-with-bits-roundtrip (Unit -> Boolean))
  (define (ssz-tests-bitvector-with-bits-roundtrip _)
    ;; Create bitvector with some bits set: [T, F, T, F, T, T, F, F]
    (match (ssz:ssz-bitvector-from-list 8
             (Cons True (Cons False (Cons True (Cons False
              (Cons True (Cons True (Cons False (Cons False Nil)))))))))
      ((Err _) False)
      ((Ok bv)
       (match (ssz:deserialize-bitvector 8 (ssz:serialize-bitvector bv))
         ((Err _) False)
         ((Ok result)
          (and (== (ssz:ssz-bitvector-length result) 8)
               (== (ssz:ssz-bitvector-ref 0 result) (Some True))
               (== (ssz:ssz-bitvector-ref 1 result) (Some False))
               (== (ssz:ssz-bitvector-ref 2 result) (Some True))
               (== (ssz:ssz-bitvector-ref 4 result) (Some True))))))))

  ;; Bitlist roundtrips
  (declare ssz-tests-bitlist-empty-roundtrip (Unit -> Boolean))
  (define (ssz-tests-bitlist-empty-roundtrip _)
    (let ((bl (ssz:ssz-bitlist-empty 100)))
      (match (ssz:deserialize-bitlist 100 (ssz:serialize-bitlist bl))
        ((Err _) False)
        ((Ok result) (== (ssz:ssz-bitlist-length result) 0)))))

  (declare ssz-tests-bitlist-with-bits-roundtrip (Unit -> Boolean))
  (define (ssz-tests-bitlist-with-bits-roundtrip _)
    ;; Create bitlist with bits: [T, T, F, T]
    (match (ssz:ssz-bitlist-from-list 100
             (Cons True (Cons True (Cons False (Cons True Nil)))))
      ((Err _) False)
      ((Ok bl)
       (match (ssz:deserialize-bitlist 100 (ssz:serialize-bitlist bl))
         ((Err _) False)
         ((Ok result)
          (and (== (ssz:ssz-bitlist-length result) 4)
               (== (ssz:ssz-bitlist-ref 0 result) (Some True))
               (== (ssz:ssz-bitlist-ref 1 result) (Some True))
               (== (ssz:ssz-bitlist-ref 2 result) (Some False))
               (== (ssz:ssz-bitlist-ref 3 result) (Some True))))))))

  ;; Vector roundtrips with different element types
  (declare ssz-tests-vector-u8-full-roundtrip (Unit -> Boolean))
  (define (ssz-tests-vector-u8-full-roundtrip _)
    (let ((vec (the (ssz:SSZVector U8) (ssz:SSZVector (Cons 10 (Cons 20 (Cons 30 (Cons 40 Nil))))))))
      (let ((serialized (ssz:ssz-serialize vec)))
        (match (the (ssz:SSZResult (ssz:SSZVector U8)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok (ssz:SSZVector elements))
           (lisp Boolean (elements)
             (cl:and (cl:= (cl:length elements) 4)
                     (cl:= (cl:first elements) 10)
                     (cl:= (cl:fourth elements) 40))))))))

  (declare ssz-tests-vector-u16-roundtrip (Unit -> Boolean))
  (define (ssz-tests-vector-u16-roundtrip _)
    (let ((vec (the (ssz:SSZVector U16) (ssz:SSZVector (Cons 1000 (Cons 2000 (Cons 3000 Nil)))))))
      (let ((serialized (ssz:ssz-serialize vec)))
        (match (the (ssz:SSZResult (ssz:SSZVector U16)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok (ssz:SSZVector elements))
           (lisp Boolean (elements)
             (cl:and (cl:= (cl:length elements) 3)
                     (cl:= (cl:first elements) 1000)
                     (cl:= (cl:third elements) 3000))))))))

  (declare ssz-tests-vector-u64-roundtrip (Unit -> Boolean))
  (define (ssz-tests-vector-u64-roundtrip _)
    (let ((vec (the (ssz:SSZVector U64) (ssz:SSZVector (Cons 9999999999 (Cons 8888888888 Nil))))))
      (let ((serialized (ssz:ssz-serialize vec)))
        (match (the (ssz:SSZResult (ssz:SSZVector U64)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok (ssz:SSZVector elements))
           (lisp Boolean (elements)
             (cl:and (cl:= (cl:length elements) 2)
                     (cl:= (cl:first elements) 9999999999))))))))

  (declare ssz-tests-vector-bool-roundtrip (Unit -> Boolean))
  (define (ssz-tests-vector-bool-roundtrip _)
    (let ((vec (the (ssz:SSZVector Boolean) (ssz:SSZVector (Cons True (Cons False (Cons True Nil)))))))
      (let ((serialized (ssz:ssz-serialize vec)))
        ;; Just check that deserialization succeeds and returns correct count
        (match (the (ssz:SSZResult (ssz:SSZVector Boolean)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok result) (== (ssz:ssz-vector-length result) 3))))))

  ;; List roundtrips with different element types
  (declare ssz-tests-list-u8-roundtrip (Unit -> Boolean))
  (define (ssz-tests-list-u8-roundtrip _)
    (let ((lst (the (ssz:SSZList U8) (ssz:SSZList 100 (Cons 5 (Cons 10 (Cons 15 Nil)))))))
      (let ((serialized (ssz:ssz-serialize lst)))
        ;; Just check that deserialization succeeds and returns correct count
        (match (the (ssz:SSZResult (ssz:SSZList U8)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok result) (== (ssz:ssz-list-length result) 3))))))

  (declare ssz-tests-list-u16-roundtrip (Unit -> Boolean))
  (define (ssz-tests-list-u16-roundtrip _)
    (let ((lst (the (ssz:SSZList U16) (ssz:SSZList 100 (Cons 500 (Cons 600 Nil))))))
      (let ((serialized (ssz:ssz-serialize lst)))
        (match (the (ssz:SSZResult (ssz:SSZList U16)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok (ssz:SSZList _ elements))
           (lisp Boolean (elements)
             (cl:and (cl:= (cl:length elements) 2)
                     (cl:= (cl:first elements) 500)
                     (cl:= (cl:second elements) 600))))))))

  (declare ssz-tests-list-u64-roundtrip (Unit -> Boolean))
  (define (ssz-tests-list-u64-roundtrip _)
    (let ((lst (the (ssz:SSZList U64) (ssz:SSZList 100 (Cons 1234567890123 Nil)))))
      (let ((serialized (ssz:ssz-serialize lst)))
        (match (the (ssz:SSZResult (ssz:SSZList U64)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok (ssz:SSZList _ elements))
           (lisp Boolean (elements)
             (cl:and (cl:= (cl:length elements) 1)
                     (cl:= (cl:first elements) 1234567890123))))))))

  (declare ssz-tests-list-bool-roundtrip (Unit -> Boolean))
  (define (ssz-tests-list-bool-roundtrip _)
    (let ((lst (the (ssz:SSZList Boolean) (ssz:SSZList 100 (Cons False (Cons True (Cons False Nil)))))))
      (let ((serialized (ssz:ssz-serialize lst)))
        ;; Just check that deserialization succeeds and returns correct count
        (match (the (ssz:SSZResult (ssz:SSZList Boolean)) (ssz:ssz-deserialize serialized))
          ((Err _) False)
          ((Ok result) (== (ssz:ssz-list-length result) 3))))))

  ;;; =========================================================================
  ;;; Edge Case Test Helpers
  ;;; =========================================================================

  ;; Single element tests
  (declare ssz-tests-vector-single-element (Unit -> Boolean))
  (define (ssz-tests-vector-single-element _)
    (let ((vec (the (ssz:SSZVector U32) (ssz:SSZVector (Cons 42 Nil)))))
      (match (the (ssz:SSZResult (ssz:SSZVector U32)) (ssz:ssz-deserialize (ssz:ssz-serialize vec)))
        ((Err _) False)
        ((Ok result) (== (ssz:ssz-vector-length result) 1)))))

  (declare ssz-tests-list-single-element (Unit -> Boolean))
  (define (ssz-tests-list-single-element _)
    (let ((lst (the (ssz:SSZList U64) (ssz:SSZList 100 (Cons 999999 Nil)))))
      (match (the (ssz:SSZResult (ssz:SSZList U64)) (ssz:ssz-deserialize (ssz:ssz-serialize lst)))
        ((Err _) False)
        ((Ok result) (== (ssz:ssz-list-length result) 1)))))

  ;; Bitvector boundary tests (chunk = 256 bits)
  (declare ssz-tests-bitvector-255-bits (Unit -> Boolean))
  (define (ssz-tests-bitvector-255-bits _)
    (let ((bv (ssz:ssz-bitvector-zeros 255)))
      (match (ssz:deserialize-bitvector 255 (ssz:serialize-bitvector bv))
        ((Err _) False)
        ((Ok result) (== (ssz:ssz-bitvector-length result) 255)))))

  (declare ssz-tests-bitvector-257-bits (Unit -> Boolean))
  (define (ssz-tests-bitvector-257-bits _)
    (let ((bv (ssz:ssz-bitvector-zeros 257)))
      (match (ssz:deserialize-bitvector 257 (ssz:serialize-bitvector bv))
        ((Err _) False)
        ((Ok result) (== (ssz:ssz-bitvector-length result) 257)))))

  (declare ssz-tests-bitvector-single-bit (Unit -> Boolean))
  (define (ssz-tests-bitvector-single-bit _)
    (match (ssz:ssz-bitvector-from-list 1 (Cons True Nil))
      ((Err _) False)
      ((Ok bv)
       (match (ssz:deserialize-bitvector 1 (ssz:serialize-bitvector bv))
         ((Err _) False)
         ((Ok result)
          (and (== (ssz:ssz-bitvector-length result) 1)
               (== (ssz:ssz-bitvector-ref 0 result) (Some True))))))))

  ;; Bitlist boundary tests
  (declare ssz-tests-bitlist-single-bit (Unit -> Boolean))
  (define (ssz-tests-bitlist-single-bit _)
    (match (ssz:ssz-bitlist-from-list 100 (Cons True Nil))
      ((Err _) False)
      ((Ok bl)
       (match (ssz:deserialize-bitlist 100 (ssz:serialize-bitlist bl))
         ((Err _) False)
         ((Ok result)
          (and (== (ssz:ssz-bitlist-length result) 1)
               (== (ssz:ssz-bitlist-ref 0 result) (Some True))))))))

  (declare ssz-tests-bitlist-7-bits (Unit -> Boolean))
  (define (ssz-tests-bitlist-7-bits _)
    ;; 7 bits + marker bit = 8 bits = 1 byte
    (match (ssz:ssz-bitlist-from-list 100
             (Cons True (Cons False (Cons True (Cons True
              (Cons False (Cons True (Cons False Nil))))))))
      ((Err _) False)
      ((Ok bl)
       (match (ssz:deserialize-bitlist 100 (ssz:serialize-bitlist bl))
         ((Err _) False)
         ((Ok result) (== (ssz:ssz-bitlist-length result) 7))))))

  (declare ssz-tests-bitlist-8-bits (Unit -> Boolean))
  (define (ssz-tests-bitlist-8-bits _)
    ;; 8 bits + marker bit = 9 bits = 2 bytes
    (match (ssz:ssz-bitlist-from-list 100
             (Cons True (Cons True (Cons True (Cons True
              (Cons True (Cons True (Cons True (Cons True Nil)))))))))
      ((Err _) False)
      ((Ok bl)
       (match (ssz:deserialize-bitlist 100 (ssz:serialize-bitlist bl))
         ((Err _) False)
         ((Ok result) (== (ssz:ssz-bitlist-length result) 8))))))

  ;; Pack boundary tests
  (declare ssz-tests-pack-31-bytes (Unit -> UFix))
  (define (ssz-tests-pack-31-bytes _)
    ;; 31 bytes should produce 1 chunk
    (list:length (ssz:pack (ssz:make-bytes 31))))

  (declare ssz-tests-pack-32-bytes (Unit -> UFix))
  (define (ssz-tests-pack-32-bytes _)
    ;; 32 bytes should produce 1 chunk
    (list:length (ssz:pack (ssz:make-bytes 32))))

  (declare ssz-tests-pack-33-bytes (Unit -> UFix))
  (define (ssz-tests-pack-33-bytes _)
    ;; 33 bytes should produce 2 chunks
    (list:length (ssz:pack (ssz:make-bytes 33))))

  (declare ssz-tests-pack-64-bytes (Unit -> UFix))
  (define (ssz-tests-pack-64-bytes _)
    ;; 64 bytes should produce 2 chunks
    (list:length (ssz:pack (ssz:make-bytes 64))))

  ;; Invalid deserialization tests
  (declare ssz-tests-deserialize-u32-truncated (Unit -> Boolean))
  (define (ssz-tests-deserialize-u32-truncated _)
    ;; 3 bytes is not enough for U32 (needs 4)
    (match (ssz:deserialize-u32 (ssz:make-bytes 3))
      ((Err _) True)   ;; Expected: error
      ((Ok _) False)))

  (declare ssz-tests-deserialize-u64-truncated (Unit -> Boolean))
  (define (ssz-tests-deserialize-u64-truncated _)
    ;; 7 bytes is not enough for U64 (needs 8)
    (match (ssz:deserialize-u64 (ssz:make-bytes 7))
      ((Err _) True)   ;; Expected: error
      ((Ok _) False)))

  (declare ssz-tests-deserialize-bool-invalid-2 (Unit -> Boolean))
  (define (ssz-tests-deserialize-bool-invalid-2 _)
    ;; Value 2 is invalid for boolean
    (let ((invalid-bytes (ssz:bytes-from-list (Cons 2 Nil))))
      (match (ssz:deserialize-bool invalid-bytes)
        ((Err _) True)   ;; Expected: error
        ((Ok _) False))))

  (declare ssz-tests-deserialize-bool-invalid-255 (Unit -> Boolean))
  (define (ssz-tests-deserialize-bool-invalid-255 _)
    ;; Value 255 is invalid for boolean
    (let ((invalid-bytes (ssz:bytes-from-list (Cons 255 Nil))))
      (match (ssz:deserialize-bool invalid-bytes)
        ((Err _) True)   ;; Expected: error
        ((Ok _) False))))

  ;; Bytes slice edge cases
  (declare ssz-tests-bytes-slice-full (Unit -> Boolean))
  (define (ssz-tests-bytes-slice-full _)
    ;; Slice entire array
    (let ((data (ssz:bytes-from-list (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))))
      (== (ssz:bytes-length (ssz:bytes-slice 0 4 data)) 4)))

  (declare ssz-tests-bytes-slice-empty (Unit -> Boolean))
  (define (ssz-tests-bytes-slice-empty _)
    ;; Zero-length slice
    (let ((data (ssz:bytes-from-list (Cons 1 (Cons 2 Nil)))))
      (== (ssz:bytes-length (ssz:bytes-slice 0 0 data)) 0)))

  ;; Zero hash edge cases
  (declare ssz-tests-zero-hash-depth-0 (Unit -> Boolean))
  (define (ssz-tests-zero-hash-depth-0 _)
    ;; Depth 0 should be 32 zero bytes
    (let ((zh (ssz:zero-hash 0)))
      (and (== (ssz:bytes-length zh) 32)
           (== (ssz:bytes-ref 0 zh) (Some 0))
           (== (ssz:bytes-ref 31 zh) (Some 0)))))

  (declare ssz-tests-zero-hash-depth-1 (Unit -> Boolean))
  (define (ssz-tests-zero-hash-depth-1 _)
    ;; Depth 1 should be hash(zero, zero) - not all zeros
    (let ((zh (ssz:zero-hash 1)))
      (and (== (ssz:bytes-length zh) 32)
           ;; First byte of sha256(zeros64) is 0xf5
           (== (ssz:bytes-ref 0 zh) (Some #xf5)))))

  ;; Merkleize edge cases
  (declare ssz-tests-merkleize-single-chunk (Unit -> Boolean))
  (define (ssz-tests-merkleize-single-chunk _)
    ;; Single chunk should return itself
    (let ((chunk (ssz:zero-chunk)))
      (let ((root (ssz:merkleize (Cons chunk Nil))))
        (== (ssz:bytes-length root) 32))))

  (declare ssz-tests-merkleize-two-chunks (Unit -> Boolean))
  (define (ssz-tests-merkleize-two-chunks _)
    ;; Two chunks -> hash them together
    (let ((c1 (ssz:zero-chunk))
          (c2 (ssz:zero-chunk)))
      (let ((root (ssz:merkleize (Cons c1 (Cons c2 Nil)))))
        ;; Root should equal hash(c1, c2) = zero-hash(1)
        (== (ssz:bytes-ref 0 root) (Some #xf5)))))

  ;;; =========================================================================
  ;;; Additional Coverage Test Helpers
  ;;; =========================================================================

  ;; U128 max value roundtrip (all bytes 0xFF)
  (declare ssz-tests-u128-max-roundtrip (Unit -> Boolean))
  (define (ssz-tests-u128-max-roundtrip _)
    (let ((val (ssz:u128-make 18446744073709551615 18446744073709551615)))
      (match (ssz:deserialize-u128 (ssz:serialize-u128 val))
        ((Err _) False)
        ((Ok result)
         (and (== (ssz:u128-word 0 result) 18446744073709551615)
              (== (ssz:u128-word 1 result) 18446744073709551615))))))

  ;; U256 max value roundtrip (all bytes 0xFF)
  (declare ssz-tests-u256-max-roundtrip (Unit -> Boolean))
  (define (ssz-tests-u256-max-roundtrip _)
    (let ((val (ssz:u256-make 18446744073709551615 18446744073709551615
                              18446744073709551615 18446744073709551615)))
      (match (ssz:deserialize-u256 (ssz:serialize-u256 val))
        ((Err _) False)
        ((Ok result)
         (and (== (ssz:u256-word 0 result) 18446744073709551615)
              (== (ssz:u256-word 1 result) 18446744073709551615)
              (== (ssz:u256-word 2 result) 18446744073709551615)
              (== (ssz:u256-word 3 result) 18446744073709551615))))))

  ;; SSZList from-list rejects over-max-length
  (declare ssz-tests-list-from-list-reject (Unit -> Boolean))
  (define (ssz-tests-list-from-list-reject _)
    (match (ssz:ssz-list-from-list 2 (make-list (the U32 1) 2 3))
      ((Err _) True)
      ((Ok _) False)))

  ;; Bitlist rejects bits > max-length
  (declare ssz-tests-bitlist-max-reject (Unit -> Boolean))
  (define (ssz-tests-bitlist-max-reject _)
    (match (ssz:ssz-bitlist-from-list 3
             (make-list True False True True))
      ((Err _) True)
      ((Ok _) False)))

  ;; Mix-in-selector direct test
  (declare ssz-tests-mix-in-selector (Unit -> Boolean))
  (define (ssz-tests-mix-in-selector _)
    (let ((root (ssz:zero-chunk))
          (mixed (ssz:mix-in-selector (ssz:zero-chunk) 5)))
      ;; Should produce 32 bytes and differ from the original
      (and (== (ssz:bytes-length mixed) 32)
           (not (== (ssz:bytes-ref 0 root) (ssz:bytes-ref 0 mixed))))))

  ;; Mix-in-selector different selectors produce different results
  (declare ssz-tests-mix-in-selector-differs (Unit -> Boolean))
  (define (ssz-tests-mix-in-selector-differs _)
    (let ((mixed1 (ssz:mix-in-selector (ssz:zero-chunk) 1))
          (mixed2 (ssz:mix-in-selector (ssz:zero-chunk) 2)))
      (not (== (ssz:bytes-ref 0 mixed1) (ssz:bytes-ref 0 mixed2)))))

  ;; Merkleize-with-limit direct test
  (declare ssz-tests-merkleize-with-limit (Unit -> Boolean))
  (define (ssz-tests-merkleize-with-limit _)
    ;; One chunk with limit 4 should produce a valid root
    (let ((chunk (ssz:zero-chunk))
          (root (ssz:merkleize-with-limit
                  (Cons (ssz:zero-chunk) Nil)
                  (Some 4))))
      ;; With limit=4, it pads to 4 leaves before merkleizing
      ;; Result should differ from just merkleizing one chunk (limit=1)
      (let ((root-no-limit (ssz:merkleize (Cons chunk Nil))))
        (and (== (ssz:bytes-length root) 32)
             (not (== (ssz:bytes-ref 0 root)
                      (ssz:bytes-ref 0 root-no-limit)))))))

  ;; Bytes-set! direct test
  (declare ssz-tests-bytes-set (Unit -> Boolean))
  (define (ssz-tests-bytes-set _)
    (let ((data (ssz:make-bytes 4)))
      (ssz:bytes-set! 0 42 data)
      (ssz:bytes-set! 3 99 data)
      (and (== (ssz:bytes-ref 0 data) (Some 42))
           (== (ssz:bytes-ref 1 data) (Some 0))
           (== (ssz:bytes-ref 3 data) (Some 99)))))

  ;; Union value extraction after roundtrip
  (declare ssz-tests-union-value-roundtrip (Unit -> Boolean))
  (define (ssz-tests-union-value-roundtrip _)
    (match (ssz:ssz-union-from-value 1 (the U32 12345))
      ((Err _) False)
      ((Ok union)
       (let ((serialized (ssz:ssz-serialize union)))
         (match (ssz:ssz-deserialize serialized)
           ((Err _) False)
           ((Ok deserialized)
            ;; Extract the inner value and deserialize it
            (match (ssz:ssz-union-value deserialized)
              ((None) False)
              ((Some val-bytes)
               (match (ssz:deserialize-u32 val-bytes)
                 ((Err _) False)
                 ((Ok val) (== val 12345)))))))))))

  ;; Large vector roundtrip (128 elements)
  (declare ssz-tests-large-vector-roundtrip (Unit -> Boolean))
  (define (ssz-tests-large-vector-roundtrip _)
    (let ((elems (lisp (List U8) ()
                   (cl:loop :for i :from 0 :below 128 :collect i))))
      (let ((vec (the (ssz:SSZVector U8) (ssz:SSZVector elems))))
        (let ((serialized (ssz:ssz-serialize vec)))
          (and (== (ssz:bytes-length serialized) 128)
               (match (the (ssz:SSZResult (ssz:SSZVector U8))
                           (ssz:ssz-deserialize serialized))
                 ((Err _) False)
                 ((Ok result)
                  (== (ssz:ssz-vector-length result) 128))))))))

  ;; Large list roundtrip (200 elements)
  (declare ssz-tests-large-list-roundtrip (Unit -> Boolean))
  (define (ssz-tests-large-list-roundtrip _)
    (let ((elems (lisp (List U16) ()
                   (cl:loop :for i :from 0 :below 200 :collect i))))
      (let ((lst (the (ssz:SSZList U16) (ssz:SSZList 1000 elems))))
        (let ((serialized (ssz:ssz-serialize lst)))
          (and (== (ssz:bytes-length serialized) 400)
               (match (the (ssz:SSZResult (ssz:SSZList U16))
                           (ssz:ssz-deserialize serialized))
                 ((Err _) False)
                 ((Ok result)
                  (== (ssz:ssz-list-length result) 200))))))))

  ;; Container hash-tree-root with U32 fields
  (declare ssz-tests-container-htr-u32 (Unit -> Boolean))
  (define (ssz-tests-container-htr-u32 _)
    (let ((root (ssz:hash-tree-root-container
                  (make-list
                    (ssz:hash-tree-root (the U32 100))
                    (ssz:hash-tree-root (the U32 200))
                    (ssz:hash-tree-root (the U64 300))))))
      (== (ssz:bytes-length root) 32)))

  ;; Container hash-tree-root differs for different values
  (declare ssz-tests-container-htr-differs (Unit -> Boolean))
  (define (ssz-tests-container-htr-differs _)
    (let ((root1 (ssz:hash-tree-root-container
                   (make-list
                     (ssz:hash-tree-root (the U32 1))
                     (ssz:hash-tree-root (the U32 2)))))
          (root2 (ssz:hash-tree-root-container
                   (make-list
                     (ssz:hash-tree-root (the U32 3))
                     (ssz:hash-tree-root (the U32 4))))))
      (not (== (ssz:bytes-ref 0 root1) (ssz:bytes-ref 0 root2)))))

)

