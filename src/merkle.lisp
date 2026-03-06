(in-package #:ssz)

(named-readtables:in-readtable coalton:coalton)

;;; Zero hash cache - precomputed zero hashes for depths 0-32
;;; Initialized after coalton-toplevel defines sha256
(cl:defvar *zero-hash-cache* nil
  "Cache of precomputed zero hashes for depths 0-32")

(coalton-toplevel
  ;;; Merkleization for SSZ
  ;;; Uses SHA256 for hashing (via Ironclad)

  ;; Type alias for a 32-byte hash (chunk)
  (define-type-alias Chunk Bytes)

  ;; SHA256 hash of a byte vector
  (declare sha256 (Bytes -> Chunk))
  (define (sha256 data)
    "Compute SHA256 hash of byte data"
    (lisp Bytes (data)
      (cl:let* ((digest (ironclad:make-digest :sha256))
                (vec (cl:make-array (cl:length data)
                                    :element-type '(cl:unsigned-byte 8)
                                    :initial-contents data))
                (result-digest (ironclad:digest-sequence digest vec))
                (result (cl:make-array 32 :fill-pointer 32 :adjustable cl:t)))
        (cl:loop :for i :from 0 :below 32
                 :do (cl:setf (cl:aref result i) (cl:aref result-digest i)))
        result)))

  ;; Hash two 32-byte chunks together
  (declare hash-concat (Chunk -> Chunk -> Chunk))
  (define (hash-concat left right)
    "Hash two chunks together: SHA256(left || right)"
    (sha256 (bytes-append left right)))

  ;; Zero chunk (32 bytes of zeros)
  (declare zero-chunk (Unit -> Chunk))
  (define (zero-chunk)
    "Return a 32-byte zero chunk"
    (make-bytes 32))

  ;; Precomputed zero hashes for each level (cached)
  ;; zero_hashes[0] = zero chunk
  ;; zero_hashes[n+1] = hash(zero_hashes[n], zero_hashes[n])
  (declare zero-hash (UFix -> Chunk))
  (define (zero-hash depth)
    "Get the zero hash at the given depth (cached for depths 0-32)"
    (lisp Chunk (depth)
      (cl:aref *zero-hash-cache* (cl:min depth 32))))

  ;; Next power of 2 (with 0 -> 1)
  (declare next-pow-of-two (UFix -> UFix))
  (define (next-pow-of-two n)
    "Get the next power of 2 >= n (with 0 -> 1)"
    (if (== n 0)
        1
        (lisp UFix (n)
          (cl:ash 1 (cl:integer-length (cl:1- n))))))

  ;; Calculate log2 (ceiling)
  (declare log2-ceil (UFix -> UFix))
  (define (log2-ceil n)
    "Calculate ceiling of log2(n)"
    (if (<= n 1)
        0
        (lisp UFix (n)
          (cl:integer-length (cl:1- n)))))

  ;; Pack values into chunks (32 bytes each)
  ;; Serialized data is padded to multiple of 32 bytes
  (declare pack (Bytes -> (List Chunk)))
  (define (pack data)
    "Pack serialized data into 32-byte chunks"
    (lisp (List Bytes) (data)
      (cl:let* ((len (cl:length data))
                (num-chunks (cl:if (cl:= len 0)
                                   1
                                   (cl:ceiling len 32)))
                (result cl:nil))
        (cl:if (cl:= len 0)
               ;; Empty data -> single zero chunk
               (cl:setf result (cl:list (cl:make-array 32 :fill-pointer 32 :adjustable cl:t :initial-element 0)))
               ;; Pad and split into chunks
               (cl:loop :for i :from 0 :below num-chunks
                        :for chunk := (cl:make-array 32 :fill-pointer 32 :adjustable cl:t :initial-element 0)
                        :for start := (cl:* i 32)
                        :do (cl:loop :for j :from 0 :below 32
                                     :for src-idx := (cl:+ start j)
                                     :when (cl:< src-idx len)
                                     :do (cl:setf (cl:aref chunk j) (cl:aref data src-idx)))
                        :do (cl:push chunk result)))
        (cl:nreverse result))))

  ;; Helper: set bits in bytes
  (declare pack-bits ((List Boolean) -> (List Chunk)))
  (define (pack-bits bits)
    "Pack boolean bits into chunks (8 bits per byte, 32 bytes per chunk)"
    (lisp (List Bytes) (bits)
      (cl:let* ((num-bits (cl:length bits))
                (num-bytes (cl:if (cl:= num-bits 0)
                                  0
                                  (cl:ceiling num-bits 8))))
        (cl:if (cl:= num-bytes 0)
               (cl:list (cl:make-array 32 :fill-pointer 32 :adjustable cl:t :initial-element 0))
               (cl:let ((bytes-data (cl:make-array num-bytes :fill-pointer num-bytes :adjustable cl:t :initial-element 0)))
                 ;; Set bits in bytes
                 (cl:loop :for bit :in bits
                          :for i :from 0
                          :when bit
                          :do (cl:let ((byte-idx (cl:floor i 8))
                                       (bit-idx (cl:mod i 8)))
                                (cl:setf (cl:aref bytes-data byte-idx)
                                         (cl:logior (cl:aref bytes-data byte-idx)
                                                    (cl:ash 1 bit-idx)))))
                 ;; Pack bytes into chunks
                 (cl:let* ((num-chunks (cl:ceiling num-bytes 32))
                           (result cl:nil))
                   (cl:loop :for i :from 0 :below num-chunks
                            :for chunk := (cl:make-array 32 :fill-pointer 32 :adjustable cl:t :initial-element 0)
                            :for start := (cl:* i 32)
                            :do (cl:loop :for j :from 0 :below 32
                                         :for src-idx := (cl:+ start j)
                                         :when (cl:< src-idx num-bytes)
                                         :do (cl:setf (cl:aref chunk j) (cl:aref bytes-data src-idx)))
                            :do (cl:push chunk result))
                   (cl:nreverse result)))))))

  ;; Merkleize chunks (build merkle tree and return root)
  (declare merkleize ((List Chunk) -> Chunk))
  (define (merkleize chunks)
    "Merkleize chunks without limit"
    (merkleize-with-limit chunks None))

  ;; Merkleize chunks with optional limit
  (declare merkleize-with-limit ((List Chunk) -> (Optional UFix) -> Chunk))
  (define (merkleize-with-limit chunks limit)
    "Merkleize chunks with optional limit for padding"
    (let ((num-chunks (list:length chunks)))
      (if (== num-chunks 0)
          ;; Empty -> merkleize single zero chunk
          (merkleize-with-limit (Cons (zero-chunk) Nil) limit)
          ;; Calculate target number of leaves
          (let ((target-leaves
                 (match limit
                   ((Some lim) (next-pow-of-two lim))
                   ((None) (next-pow-of-two (lisp UFix (num-chunks) num-chunks))))))
            (if (> (lisp UFix (num-chunks) num-chunks) target-leaves)
                ;; Error: too many chunks for limit
                (zero-chunk)
                ;; Build merkle tree
                (merkleize-tree chunks target-leaves))))))

  ;; Build merkle tree from chunks
  (declare merkleize-tree ((List Chunk) -> UFix -> Chunk))
  (define (merkleize-tree chunks num-leaves)
    "Build merkle tree from chunks with num-leaves (power of 2)"
    (let ((num-chunks (list:length chunks)))
      (if (== num-leaves 1)
          ;; Base case: single leaf
          (if (> (lisp UFix (num-chunks) num-chunks) 0)
              (match chunks
                ((Cons first _) first)
                ((Nil) (zero-chunk)))
              (zero-chunk))
          ;; Recursive case: hash pairs
          (let ((half (lisp UFix (num-leaves) (cl:floor num-leaves 2))))
            (match (split-list half chunks)
              ((Tuple left-chunks right-chunks)
               ;; Recursively merkleize subtrees
               (let ((left-root (merkleize-tree left-chunks half))
                     (right-root (if (list:null? right-chunks)
                                     (zero-hash (log2-ceil half))
                                     (merkleize-tree right-chunks half))))
                 (hash-concat left-root right-root))))))))

  ;; Helper: split list at position
  (declare split-list (UFix -> (List :a) -> (Tuple (List :a) (List :a))))
  (define (split-list n lst)
    "Split a list at position n"
    (lisp (Tuple (List :a) (List :a)) (n lst)
      (cl:let ((left cl:nil)
               (right lst)
               (count 0))
        (cl:loop :while (cl:and (cl:< count n) right)
                 :do (cl:push (cl:car right) left)
                 :do (cl:setf right (cl:cdr right))
                 :do (cl:incf count))
        (coalton-prelude:Tuple (cl:nreverse left) right))))

  ;; Mix in length (for variable-size types)
  (declare mix-in-length (Chunk -> UFix -> Chunk))
  (define (mix-in-length root len)
    "Mix length into merkle root: hash(root || length_as_256bit_le)"
    (let ((len-bytes (make-bytes 32))
          (len64 (lisp U64 (len) len))
          (serialized (serialize-u64 len64)))
      ;; Copy first 8 bytes of length (rest are zeros)
      (lisp Unit (serialized len-bytes)
        (cl:loop :for i :from 0 :below (cl:min 8 (cl:length serialized))
                 :do (cl:setf (cl:aref len-bytes i) (cl:aref serialized i)))
        coalton:Unit)
      (hash-concat root len-bytes)))

  ;; Mix in selector (for union types)
  (declare mix-in-selector (Chunk -> U8 -> Chunk))
  (define (mix-in-selector root selector)
    "Mix selector into merkle root: hash(root || selector_padded_to_32bytes)"
    (let ((selector-bytes (make-bytes 32)))
      (bytes-set! 0 selector selector-bytes)
      (hash-concat root selector-bytes)))

  ;; Chunk count for different types
  (declare chunk-count-basic (UFix -> UFix))
  (define (chunk-count-basic _size-bytes)
    "Chunk count for basic types (always 1)"
    1)

  (declare chunk-count-bitvector (UFix -> UFix))
  (define (chunk-count-bitvector num-bits)
    "Chunk count for bitvector: (N + 255) / 256"
    (lisp UFix (num-bits) (cl:floor (cl:+ num-bits 255) 256)))

  (declare chunk-count-vector-basic (UFix -> UFix -> UFix))
  (define (chunk-count-vector-basic max-length element-size)
    "Chunk count for Vector[B, N] where B is basic: (N * size_of(B) + 31) / 32"
    (lisp UFix (max-length element-size) (cl:floor (cl:+ (cl:* max-length element-size) 31) 32)))

  (declare chunk-count-vector-composite (UFix -> UFix))
  (define (chunk-count-vector-composite max-length)
    "Chunk count for Vector[C, N] where C is composite: N"
    max-length))

;;; Cache initialization is in merkle-cache.lisp (loaded after this file)
