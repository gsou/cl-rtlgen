(in-package :cl-rtlgen)

(defparameter rtl-counter-vargen 0
  "Counter for the variable name generator")


(defun rtl-get-dir (base dir &key (sep "_"))
  "Generate a direction suffix with the base direction BASE xor DIR"
  (case dir
    ('no "")
    (t (if (or (and (not base) (not dir)) (and base dir))
         (concatenate 'string sep "i")
         (concatenate 'string sep "o")))))

(defun rtl-iface-axi-full (&optional (dir 'no) (idw 4) (addrw 32) (dataw 32) (strbw 4))
  "Alist with the name of a full AXI4 interface with the signal width and direction as the value"
  ;; TODO Add parameters for user signals
  `(( "awid"    ,(rtl-get-dir t dir) . ,idw )
    ( "awaddr"  ,(rtl-get-dir t dir) . ,addrw )
    ( "awlen"   ,(rtl-get-dir t dir) . 8 )
    ( "awsize"  ,(rtl-get-dir t dir) . 3 )
    ( "awburst" ,(rtl-get-dir t dir) . 2 )
    ( "awlock"  ,(rtl-get-dir t dir) . 1 )
    ( "awcache" ,(rtl-get-dir t dir) . 4 )
    ( "awprot"  ,(rtl-get-dir t dir) . 3 )
    ( "awqos"   ,(rtl-get-dir t dir) . 4 )
    ( "awuser"  ,(rtl-get-dir t dir) . 1 )
    ( "awvalid" ,(rtl-get-dir t dir) . 1 )
    ( "awready" ,(rtl-get-dir nil dir) . 1 )
    ( "wdata"   ,(rtl-get-dir t dir) . ,dataw )
    ( "wstrb"   ,(rtl-get-dir t dir) . ,strbw )
    ( "wlast"   ,(rtl-get-dir t dir) . 1 )
    ( "wuser"   ,(rtl-get-dir t dir) . 1 )
    ( "wvalid"  ,(rtl-get-dir t dir) . 1 )
    ( "wready"  ,(rtl-get-dir nil dir) . 1 )
    ( "bid"     ,(rtl-get-dir nil dir) . ,idw )
    ( "bresp"   ,(rtl-get-dir nil dir) . 2 )
    ( "buser"   ,(rtl-get-dir nil dir) . 1 )
    ( "bvalid"  ,(rtl-get-dir nil dir) . 1 )
    ( "bready"  ,(rtl-get-dir t dir) . 1 )
    ( "arid"    ,(rtl-get-dir t dir) . ,idw )
    ( "araddr"  ,(rtl-get-dir t dir) . ,addrw )
    ( "arlen"   ,(rtl-get-dir t dir) . 8 )
    ( "arsize"  ,(rtl-get-dir t dir) . 3 )
    ( "arburst" ,(rtl-get-dir t dir) . 2 )
    ( "arlock"  ,(rtl-get-dir t dir) . 1 )
    ( "arcache" ,(rtl-get-dir t dir) . 4 )
    ( "arprot"  ,(rtl-get-dir t dir) . 3 )
    ( "arqos"   ,(rtl-get-dir t dir) . 4 )
    ( "aruser"  ,(rtl-get-dir t dir) . 1 )
    ( "arvalid" ,(rtl-get-dir t dir) . 1 )
    ( "arready" ,(rtl-get-dir nil dir) . 1 )
    ( "rid"     ,(rtl-get-dir nil dir) . ,idw )
    ( "rdata"   ,(rtl-get-dir nil dir) . ,dataw )
    ( "rresp"   ,(rtl-get-dir nil dir) . 2)
    ( "rlast"   ,(rtl-get-dir nil dir) . 1 )
    ( "ruser"   ,(rtl-get-dir t dir) . 1 )
    ( "rvalid"  ,(rtl-get-dir nil dir) . 1 )
    ( "rready"  ,(rtl-get-dir t dir) . 1 )))

(defun rtl-mult-iface-size (factor iface)
  "Multiplies the width of all ports of the interface."
  (mapcar (lambda (x) (setf (cddr x) (* factor (cddr x))) x) (copy-tree iface)))


(defun rtl-iface-axi (&optional (dir 'no) (idw 4) (addrw 32) (dataw 32) (strbw 4))
  "Alist with the name of a subset of the AXI4 interface with the signal width as the value."

  (remove-if-not (lambda (x) (find (car x)
                                   '("awid" "awaddr" "awlen" "awburst" "awvalid" "awready" "wdata" "wstrb" "wlast" "wvalid" "wready" "bid" "bresp" "bvalid" "bready" "arid" "araddr" "arlen" "arburst" "arvalid" "arready" "rid" "rdata" "rresp" "rlast" "rvalid" "rready") :test #'equal))
                 (rtl-iface-axi-full dir idw addrw dataw strbw)))


;; Low level access to the language

(defun rtl-assign (lhs rhs)
  "Assign the RHS to the LHS. assign LHS = RHS;"
  (format nil "~%assign ~A = ~A;" lhs rhs))

(defun rtl-concat (list)
  "Concat together the given params"
  (format nil "{~{~a~^, ~}}" list))

(defun rtl-define (name bits)
  "Define the signal/wire NAME of length BITS"
  (if (= 1 bits)
      (format nil "~%wire ~A;" name)
      (format nil "~%wire [~A-1:0] ~A;" bits name)))

(defun rtl-component (comp name params signals)
  "Instantiate a component"
  (format nil "~%~A #(~A) ~A (~A);" comp params name signals))

(defun rtl-newname () (incf rtl-counter-vargen) (format nil "tmpvar~A" rtl-counter-vargen))

(defun rtl-bindvar (var sig)
  "Bind the variable VAR to signal SIG (as in a component declaration)"
  (format nil "~%.~A(~A)," var sig))

;; Higher level constructs

(defun rtl-concat-to (dest &rest source)
  "Assign all the sources on the right to the variable on the left."
  (rtl-assign dest (rtl-concat source)))

(defun rtl-defineiface (name iface &key (sep "_"))
  "Define the signal/wires of the interface with the given prefix"
  (format nil "~{~A~}"
          (mapcar #'(lambda (x)
                    (rtl-define (concatenate 'string name sep (car x)) (cddr x))) iface)))

(defun rtl-bindiface-prefix (var sig iface &key override (sep "_"))
  "Assign a port from in interface"
  (let ((ov (assoc (car iface) override :test #'equal)))
    (if (or (not ov) (cdr ov))
        (rtl-bindvar (concatenate 'string var sep (car iface) (cadr iface))
                     (or (cdr ov) (concatenate 'string sig sep (car iface))))
        "")))

;; (rtl-bindiface-prefix "axi" "var" '("bid" "_i" . 3) :override '( ("bid" . "3'b0")))

(defun rtl-bindiface (var sig iface &key override)
  "Bind all the variables in the interface"
  (apply 'concatenate '(string) (mapcar (lambda (x) (rtl-bindiface-prefix var sig x :override override)) iface)))

(defun rtl-concat-iface (dest sources iface &key (sep "_"))
  "Concat multiple interfaces into a single wider interface signal."
  ;; TODO Split signal in opposite direction
  (apply 'concatenate '(string)
         (mapcar (lambda (x)
                   (apply #'rtl-concat-to (concatenate 'string dest sep (car x))
                          (mapcar #'(lambda (s) (concatenate 'string s sep (car x))) sources))) iface)))


;;; Examples usages
(defun rtl-run-string (text)
  (funcall (cl-template:compile-template text) nil))
(defun rtl-run-path (path)
  "Run the template engine on the given path."
  (with-open-file (tf path)
    (let ((text (make-string (file-length tf))))
      (read-sequence text tf)
      (rtl-run-string text))))
(defun rtl-run (name)
  "Run the template on the file NAME.tpl"
  (with-open-file (out name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "~A" (rtl-run-path (concatenate 'string name ".tpl")))))
