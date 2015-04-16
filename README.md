# cl-sophia
Common Lisp binding for Sophia database

### Examples
##### set and get
```lisp
(with-database ()
  (setf (object "a") "foo"
        (object "b") "bar")
  (values (object "a")
          (object "b")
          (object "c")))
;; "x"
;; "y"
;; NIL
```
##### delete
```lisp
(with-database ()
  (setf (object "a") "x")
  (let ((a (object "a")))
    (setf (object "a") nil)
    (values a (object "a"))))
;; "x"
;; NIL
```
##### set and get with (unsigned-byte 32) keys
```lisp
(with-database (:cmp :u32)
  (let ((n 10))
    (dotimes (i n)
      (setf (object i) (format nil "~r" i)))
    (let (result)
      (dotimes (i n (nreverse result))
        (push (object i) result)))))
;; ("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")
```
