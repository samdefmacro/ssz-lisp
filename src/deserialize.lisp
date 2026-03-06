(in-package #:ssz)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  ;;; Deserialization functions for basic SSZ types
  ;;; All integers are deserialized from little-endian format

  ;; Deserialize U8 (1 byte)
  (declare deserialize-u8 (Bytes -> (SSZResult U8)))
  (define (deserialize-u8 data)
    "Deserialize a U8 from a single byte"
    (if (/= (bytes-length data) 1)
        (Err (DeserializationError "Expected 1 byte for U8"))
        (Ok (bytes-ref-unsafe 0 data))))

  ;; Deserialize U16 (2 bytes, little-endian)
  (declare deserialize-u16 (Bytes -> (SSZResult U16)))
  (define (deserialize-u16 data)
    "Deserialize a U16 from 2 bytes (little-endian)"
    (if (/= (bytes-length data) 2)
        (Err (DeserializationError "Expected 2 bytes for U16"))
        (let ((b0 (bytes-ref-unsafe 0 data))
              (b1 (bytes-ref-unsafe 1 data)))
          (Ok (lisp U16 (b0 b1)
                (cl:logior b0 (cl:ash b1 8)))))))

  ;; Deserialize U32 (4 bytes, little-endian)
  (declare deserialize-u32 (Bytes -> (SSZResult U32)))
  (define (deserialize-u32 data)
    "Deserialize a U32 from 4 bytes (little-endian)"
    (if (/= (bytes-length data) 4)
        (Err (DeserializationError "Expected 4 bytes for U32"))
        (let ((b0 (bytes-ref-unsafe 0 data))
              (b1 (bytes-ref-unsafe 1 data))
              (b2 (bytes-ref-unsafe 2 data))
              (b3 (bytes-ref-unsafe 3 data)))
          (Ok (lisp U32 (b0 b1 b2 b3)
                (cl:logior b0
                           (cl:ash b1 8)
                           (cl:ash b2 16)
                           (cl:ash b3 24)))))))

  ;; Deserialize U64 (8 bytes, little-endian)
  (declare deserialize-u64 (Bytes -> (SSZResult U64)))
  (define (deserialize-u64 data)
    "Deserialize a U64 from 8 bytes (little-endian)"
    (if (/= (bytes-length data) 8)
        (Err (DeserializationError "Expected 8 bytes for U64"))
        (let ((b0 (bytes-ref-unsafe 0 data))
              (b1 (bytes-ref-unsafe 1 data))
              (b2 (bytes-ref-unsafe 2 data))
              (b3 (bytes-ref-unsafe 3 data))
              (b4 (bytes-ref-unsafe 4 data))
              (b5 (bytes-ref-unsafe 5 data))
              (b6 (bytes-ref-unsafe 6 data))
              (b7 (bytes-ref-unsafe 7 data)))
          (Ok (lisp U64 (b0 b1 b2 b3 b4 b5 b6 b7)
                (cl:logior b0
                           (cl:ash b1 8)
                           (cl:ash b2 16)
                           (cl:ash b3 24)
                           (cl:ash b4 32)
                           (cl:ash b5 40)
                           (cl:ash b6 48)
                           (cl:ash b7 56)))))))

  ;; Deserialize U128 (16 bytes, little-endian)
  (declare deserialize-u128 (Bytes -> (SSZResult U128)))
  (define (deserialize-u128 data)
    "Deserialize a U128 from 16 bytes (little-endian)"
    (if (/= (bytes-length data) 16)
        (Err (DeserializationError "Expected 16 bytes for U128"))
        (match (deserialize-u64 (bytes-slice 0 8 data))
          ((Err e) (Err e))
          ((Ok low)
           (match (deserialize-u64 (bytes-slice 8 8 data))
             ((Err e) (Err e))
             ((Ok high) (Ok (u128-make low high))))))))

  ;; Deserialize U256 (32 bytes, little-endian)
  (declare deserialize-u256 (Bytes -> (SSZResult U256)))
  (define (deserialize-u256 data)
    "Deserialize a U256 from 32 bytes (little-endian)"
    (if (/= (bytes-length data) 32)
        (Err (DeserializationError "Expected 32 bytes for U256"))
        (match (deserialize-u64 (bytes-slice 0 8 data))
          ((Err e) (Err e))
          ((Ok w0)
           (match (deserialize-u64 (bytes-slice 8 8 data))
             ((Err e) (Err e))
             ((Ok w1)
              (match (deserialize-u64 (bytes-slice 16 8 data))
                ((Err e) (Err e))
                ((Ok w2)
                 (match (deserialize-u64 (bytes-slice 24 8 data))
                   ((Err e) (Err e))
                   ((Ok w3) (Ok (u256-make w0 w1 w2 w3))))))))))))

  ;; Deserialize Boolean (1 byte: 0x00 for False, 0x01 for True)
  (declare deserialize-bool (Bytes -> (SSZResult Boolean)))
  (define (deserialize-bool data)
    "Deserialize a Boolean from a single byte"
    (if (/= (bytes-length data) 1)
        (Err (DeserializationError "Expected 1 byte for Boolean"))
        (let ((b (bytes-ref-unsafe 0 data)))
          (cond
            ((== b 0) (Ok False))
            ((== b 1) (Ok True))
            (True (Err (DeserializationError "Boolean must be 0x00 or 0x01")))))))

  ;; Deserialize offset (4 bytes, little-endian U32)
  (declare deserialize-offset (Bytes -> (SSZResult U32)))
  (define deserialize-offset deserialize-u32)

  ;; Helper: read U32 offset from bytes at position
  (declare read-offset-at (UFix -> Bytes -> (SSZResult U32)))
  (define (read-offset-at pos data)
    "Read a U32 offset from bytes at the given position"
    (deserialize-offset (bytes-slice pos 4 data))))
