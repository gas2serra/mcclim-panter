;;; Navigator
;;; ============================================================================
;;; Entry point is to use CLIMACS-GUI::COM-NAVIGATOR via M-C-s. Click on symbol
;;; values and press the return button to return them.

(in-package :mcclim-panter)

(defparameter navigator-column-heading-text-style
  (make-text-style :serif :bold 10))
(defvar matching-packages nil)
(defvar matching-symbols nil)
(defvar render-fully-qualified-symbols? nil)
(defvar display-external-symbols-only? nil)
(defvar display-command-internals? nil)
(defvar navigator-bound-filter nil)
(defvar navigator-error-condition nil)

(defun command-internals-symbol? (symbol)
  "XXX, If this were implemented properly, it would scan for one of the
following strings: acceptor, partial or unparser. But since it is cheaper..

Perhaps hotpatch define-command to throw if one feeds it a command name with %?"
  (let* ((name (symbol-name symbol))
         (scanner (create-scanner "COM-" :case-insensitive-mode t)))
    (and (scan scanner name) (scan #\% name))))

(defun external-symbol? (symbol)
  (declare (ignore symbol))
  t)
(defun bound? (symbol) (or (fboundp symbol) (boundp symbol)))
(defun unbound? (symbol) (not (bound? symbol)))

(defun package-apropos-list (search-regex)
  (let* ((out)
         (scanner (create-scanner search-regex :case-insensitive-mode t)))
    (dolist (p (list-all-packages))
      (when (scan scanner (package-name p))
        (push p out)))
    out))

(defvar return-values nil)
(defvar *return-values* nil)

(DEFINE-APPLICATION-FRAME NAVIGATOR NIL NIL (:MENU-BAR NIL)
			  (:PANES (PACKAGE-APROPOS-TEXT-FIELD :TEXT-FIELD :VALUE-CHANGED-CALLBACK 'MAYBE-UPDATE-APROPOS-DISPLAY)
				  (APROPOS-TEXT-FIELD :TEXT-FIELD :VALUE-CHANGED-CALLBACK 'MAYBE-UPDATE-APROPOS-DISPLAY)
				  (APROPOS-DISPLAY :APPLICATION :INCREMENTAL-REDISPLAY T :DISPLAY-FUNCTION 'RENDER-APROPOS :SCROLL-BARS :VERTICAL :END-OF-PAGE-ACTION
						   :ALLOW)
				  (FULLY-QUALIFIED-SYMBOLS-RADIO
				   (WITH-RADIO-BOX (:ORIENTATION :HORIZONTAL :VALUE-CHANGED-CALLBACK NIL)
				     (RADIO-BOX-CURRENT-SELECTION "T")
				     "NIL"))
				  (INCLUDE-EXTERNAL-SYMBOLS-RADIO
				   (WITH-RADIO-BOX (:ORIENTATION :HORIZONTAL :VALUE-CHANGED-CALLBACK NIL)
				     "T"
				     (RADIO-BOX-CURRENT-SELECTION "NIL")))
				  (INCLUDE-COMMAND-INTERNALS-RADIO
				   (WITH-RADIO-BOX (:ORIENTATION :HORIZONTAL :VALUE-CHANGED-CALLBACK NIL)
				     "T"
				     (RADIO-BOX-CURRENT-SELECTION "NIL")))
				  (APROPOS-BOUND-FILTER-RADIO
				   (WITH-RADIO-BOX (:ORIENTATION :HORIZONTAL :VALUE-CHANGED-CALLBACK 'UPDATE-NAVIGATOR-BOUND-FILTER)
				     (RADIO-BOX-CURRENT-SELECTION "NIL")
				     "BOUNDP"
				     "FBOUNDP"))
				  (RETURN-BUTTON :PUSH-BUTTON :ACTIVATE-CALLBACK (LAMBDA (&REST _)
										   (declare (ignore _))
										   (COM-QUIT-AND-RETURN-VALUES-TO-POINT)) :LABEL
						 "quit and return selected values"))
			  (:LAYOUTS
			   (:DEFAULT
                               (VERTICALLY NIL
                                 APROPOS-DISPLAY
                                 (VERTICALLY NIL
                                   (HORIZONTALLY NIL
                                     RETURN-BUTTON
                                     (LABELLING (:LABEL "Print fully qualified symbols?")
                                       FULLY-QUALIFIED-SYMBOLS-RADIO)
                                     (LABELLING (:LABEL "Include external symbols?")
                                       INCLUDE-EXTERNAL-SYMBOLS-RADIO)
                                     (LABELLING (:LABEL "Include command internals?")
                                       INCLUDE-COMMAND-INTERNALS-RADIO)
                                     (LABELLING (:LABEL "Bound filter")
                                       APROPOS-BOUND-FILTER-RADIO))
                                   (HORIZONTALLY NIL
                                     (1/2
                                      (LABELLING (:LABEL "Symbol Apropos" :ALIGN-X :CENTER)
                                        APROPOS-TEXT-FIELD))
                                     (1/2
                                      (LABELLING (:LABEL "Package Apropos" :ALIGN-X :CENTER)
                                        PACKAGE-APROPOS-TEXT-FIELD))))))))

(DEFUN NAVIGATOR-FRAME ()
  "XXX, for some reason this does not work when evaluated in the 
editor, but does when evaluated in the listener"
  (FIND 'NAVIGATOR (SLOT-VALUE (FIND-FRAME-MANAGER) 'FRAMES) :KEY 'FRAME-NAME))

(DEFUN RUN-OR-FOCUS-NAVIGATOR ()
  ;;(ANAPHORA:AIF (STUMPWM::WINDOW-BY-NAME "NAVIGATOR") (STUMPWM:SELECT-WINDOW (STUMPWM::WINDOW-NAME ANAPHORA:IT))
  ;;              (BORDEAUX-THREADS:MAKE-THREAD 'RUN-NAVIGATOR :NAME "NAVIGATOR")))
  )


(DEFUN RUN-NAVIGATOR ()
  (setf *return-values* nil)
  (setf return-values nil)
  (let* ((frame (MAKE-APPLICATION-FRAME 'NAVIGATOR)))
    (setf (GETF (SLOT-VALUE frame 'CLIM-INTERNALS::PROPERTIES) 'CLIM-CLX::FOCUS)
          (find-pane-named frame 'package-apropos-text-field))
    (RUN-FRAME-TOP-LEVEL frame :NAME "NAVIGATOR"))
  *return-values*)




(defun update-navigator-bound-filter (this-gadget selected-gadget)
  (let* ((sym (read-from-string (gadget-label selected-gadget))))
    (setf navigator-bound-filter sym)))

(defun maybe-update-apropos-display (&rest _)
  (declare (ignore _))
  (anaphora:awhen (find-pane-named *application-frame* 'apropos-display)
    (redisplay-frame-pane *application-frame* anaphora:it :force-p t)))

(defun emptyp (o)
  (eq o nil))

(defun take (n l)
  (subseq l 0 (if (< (length l) n) (if (> n (1- (length l))) (length l) (1- (length l))) n)))

(defun render-apropos (frame pane)
  ;; TODO
  ;; - display multiple columns of matching symbols according, CLASSES, SLOTS etc.
  ;; - when the change is a single character, filter existing search, etc.
  ;; - index SBCL symbol table each intern..
  ;; - clim-listener::apropos-present-symbol
  ;; - (cl-ppcre::regex-apropos-aux ("regex" packages t))  
  (handler-bind ((error (lambda (condition)
                          (setf navigator-error-condition condition)
                          (with-drawing-options (pane :ink +red+ :text-size 16)
                            (let* ((*package* (find-package 'keyword)))
                              (format pane "Error encountered while trying to update apropos display~2%~S~2%Inspect ~S for more info" 
                                      navigator-error-condition
                                      'navigator-error-condition
                                      )))
                          (return-from render-apropos))))
    (let* ((pane (find-pane-named frame 'apropos-display))
             (package-apropos-regex-string 
              (gadget-value (find-pane-named *application-frame* 'package-apropos-text-field)))
             (apropos-regex-string 
              (gadget-value (find-pane-named *application-frame* 'apropos-text-field)))
             (ignorable-length-apropos (<= (length apropos-regex-string) 3))
             (symbols-x-offset 200))
        (setf matching-packages (unless (emptyp package-apropos-regex-string)
                                  (package-apropos-list package-apropos-regex-string))
              matching-symbols (unless (or (emptyp apropos-regex-string) ignorable-length-apropos)
                                 (regex-apropos-list apropos-regex-string matching-packages)))
        ;; if we previously had an error condition, clear it. If it throws again,
        ;; see the HANDLER-BIND
        (and navigator-error-condition (setf navigator-error-condition nil))
        #+nil(when matching-symbols
          (when navigator-bound-filter 
            (setf matching-symbols (filter navigator-bound-filter matching-symbols)))
          (when display-external-symbols-only?
            (setf matching-symbols (filter 'external-symbol? matching-symbols)))
          (unless display-command-internals?
            (setf matching-symbols (remove-if 'command-internals-symbol? matching-symbols))))

        (labels ((set-offset-start-position (&key (y 5)) 
                    (setf (stream-cursor-position pane) (values 10 y)))

                 (matching-packages-column ()
                    ; (length (list-all-packages)) => 397 so we'll just display them all..
                    (set-offset-start-position)
                    (surrounding-output-with-border
                      (pane :shape :underline :ink +black+)
                      (with-text-style (pane navigator-column-heading-text-style)
                        (princ "Packages" pane)))
                    (stream-increment-cursor-position pane 0 -8)                    
                    (dolist (package matching-packages)
                      (fresh-line pane)
                      (stream-increment-cursor-position pane 10 0)
                      (princ (package-name package) pane)))

                 (matching-symbols-column ()
                    (setf (stream-cursor-position pane) (values symbols-x-offset 5))
                    (surrounding-output-with-border
                      (pane :shape :underline :ink +black+)
                      (with-text-style (pane navigator-column-heading-text-style)
                        (princ "Symbols" pane)))
                    (if ignorable-length-apropos
                        (progn (fresh-line pane)
                               (stream-increment-cursor-position pane symbols-x-offset 0)
                               (princ "; (<= 3 search-string-length), not searching" pane))
                        (progn (stream-increment-cursor-position pane 0 -8)
                               (dolist (sym (take 800 matching-symbols))
                                 (fresh-line pane)
                                 (stream-increment-cursor-position pane symbols-x-offset 0)
                                 (present sym 'symbol :stream pane)
                                 ; (clim-listener::apropos-present-symbol sym pane t)
                                 )))))

          (if (and (null matching-packages) (null matching-symbols))
              (progn (set-offset-start-position)
                     (princ "; no results" pane))
              (progn (matching-packages-column)
                     (matching-symbols-column)))))))

(define-navigator-command (com-quit-and-return-values-to-point :menu t
                                                               :name t
                                                               :keystroke ((:meta #\q)))
    ()
  (setf *return-values* return-values)
  (frame-exit *application-frame*))

(define-navigator-command (com-select-symbol-for-return :name t)
  ((sym 'symbol :gesture :select))
  (push sym return-values))


;;;
;;; climacs
;;;
#|
(in-package climacs-gui)

(defvar return-point nil)

(define-command (com-navigator
                 :name t 
                 :command-table drei-lisp-syntax::lisp-table) ()
  (setf return-point (climacs-gui::point)
        climi::return-values nil)
  (flexichain:insert-sequence climacs-gui::return-point (write-to-string
							 (climi::run-navigator))))

(esa-io::set-key 'com-navigator 'drei-lisp-syntax::lisp-table '((#\s :meta :control)))
|#

;; listener
#|
(CLIM-LISTENER::DEFINE-LISTENER-COMMAND (COM-RUN-NAVIGATOR :NAME T)
    NIL
  (RUN-OR-FOCUS-NAVIGATOR))
|#
