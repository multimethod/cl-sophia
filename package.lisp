(in-package #:cl-user)

(defpackage #:cl-sophia
  (:nicknames #:sophia)
  (:use #:cl
        #:cffi)
  (:import-from #:alexandria
                #:with-unique-names
                #:once-only)
  (:export #:with-database
           #:with-named-database
           #:with-named-databases
           #:with-transaction
           #:with-database-iterator
           #:$
           #:map-object
           #:*path*
           #:*order*
           #:transaction-error
           #:transaction-state))
