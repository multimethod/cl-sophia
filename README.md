# CL-SOPHIA
Common Lisp binding for Sophia database

### Examples
##### Set and get
```lisp
(with-database ("test")
  (setf ($ "x") "a"
        ($ "y") "b")
  (values ($ "x")
          ($ "y")
          ($ "z")))

;; "a"
;; "b"
;; NIL
```
##### Delete
```lisp
(with-database ("test")
  (setf ($ "x") "a")
  (let ((x ($ "x")))
    (setf ($ "x") nil)
    (values x ($ "x"))))

;; "a"
;; NIL
```
##### Transaction
```lisp
(with-database ("test" :cmp :u32)
  (with-transaction ()
    (setf ($ 0) "a"
          ($ 1) "b"
          ($ 2) "c"))
  (ignore-errors
    (with-transaction ()
      (setf ($ 1) nil)
      (error "Oops!")))
  (values ($ 0)
          ($ 1)
          ($ 2)))

;; "a"
;; "b"
;; "c"
```
###### Transaction over two databases
```lisp
(with-named-databases ((dbx "dbx" :cmp :string) ; by default
                       (dby "dby" :cmp :u32))
  (with-transaction ()
    (setf ($ "x" dbx) "a"
          ($ 100 dby) "b"))
  (ignore-errors
    (with-transaction ()
      (setf ($ "x" dbx) "foo"
            ($ 100 dby) "bar"
            ($ 101 dby) "baz")
      (error "Oops!")))
  (values ($ "x" dbx)
          ($ 100 dby)
          ($ 101 dby)))

;; "a"
;; "b"
;; NIL
```
###### Nested transaction
```lisp
(with-database ("test")
  (with-transaction ()
    (setf ($ "x") "foo"
          ($ "y") "bar")
    (handler-case
        (with-transaction ()            ; Signaled transaction-error with :lock state
          (setf ($ "y") "baz"))
      (transaction-error (c)
        (transaction-state c)           ; :lock
        )))

  (values ($ "x")
          ($ "y")))

;; "foo"
;; "bar"
```
##### Iterator
```lisp
(with-database ("test4" :cmp :u32)
  (dotimes (i 3)
    (let ((i (1+ i)))
      (setf ($ i) (format nil "~r" i))))

  (let ((*order* :>=))                  ; by default
    (with-database-iterator (fn)
      (list #1=(multiple-value-list (fn))
            #1#
            #1#
            #1#)))
  ;; ((T 1 "one") (T 2 "two") (T 3 "three") (NIL))

  (let ((*order* :<=))
    (collecting
      (map-object (lambda (key value)
                    (collect (cons key value))))))
  ;; ((3 . "three") (2 . "two") (1 . "one"))
  )
```
