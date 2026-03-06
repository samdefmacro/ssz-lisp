;;; Zero hash cache initialization
;;; This file uses the standard readtable (no Coalton)

#.(cl:progn (named-readtables:in-readtable :standard) cl:nil)

(cl:in-package #:ssz)

(cl:defun initialize-zero-hash-cache ()
  "Precompute zero hashes for depths 0-32"
  (cl:let ((cache (cl:make-array 33)))
    ;; Depth 0: zero chunk (32 bytes of zeros)
    (cl:setf (cl:aref cache 0) (cl:make-array 32 :fill-pointer 32 :adjustable cl:t :initial-element 0))
    ;; Depths 1-32: hash(prev, prev)
    (cl:dotimes (d 32)
      (cl:let* ((depth (cl:1+ d))
                (prev (cl:aref cache (cl:1- depth)))
                (combined (cl:make-array 64 :fill-pointer 64 :adjustable cl:t)))
        ;; Copy prev twice into combined
        (cl:dotimes (i 32)
          (cl:setf (cl:aref combined i) (cl:aref prev i))
          (cl:setf (cl:aref combined (cl:+ i 32)) (cl:aref prev i)))
        ;; Hash combined
        (cl:let* ((digest (ironclad:make-digest :sha256))
                  (vec (cl:make-array 64 :element-type '(cl:unsigned-byte 8) :initial-contents combined))
                  (result-digest (ironclad:digest-sequence digest vec))
                  (result (cl:make-array 32 :fill-pointer 32 :adjustable cl:t)))
          (cl:dotimes (i 32)
            (cl:setf (cl:aref result i) (cl:aref result-digest i)))
          (cl:setf (cl:aref cache depth) result))))
    (cl:setf *zero-hash-cache* cache)))

;; Initialize cache at load time
(initialize-zero-hash-cache)
