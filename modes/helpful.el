;; overrides in apropos to try to use helpful-* instead.

(require 'apropos)

(define-button-type 'apropos-function
  'apropos-label "Function"
  'apropos-short-label "f"
  'face 'apropos-function-button
  'help-echo "mouse-2, RET: Display more help on this function"
  'follow-link t
  'action (lambda (button)
            (helpful-function (button-get button 'apropos-symbol))))

(define-button-type 'apropos-macro
  'apropos-label "Macro"
  'apropos-short-label "m"
  'face 'apropos-function-button
  'help-echo "mouse-2, RET: Display more help on this macro"
  'follow-link t
  'action (lambda (button)
            (helpful-function (button-get button 'apropos-symbol))))

(define-button-type 'apropos-command
  'apropos-label "Command"
  'apropos-short-label "c"
  'face 'apropos-function-button
  'help-echo "mouse-2, RET: Display more help on this command"
  'follow-link t
  'action (lambda (button)
            (helpful-function (button-get button 'apropos-symbol))))

(define-button-type 'apropos-variable
  'apropos-label "Variable"
  'apropos-short-label "v"
  'face 'apropos-variable-button
  'help-echo "mouse-2, RET: Display more help on this variable"
  'follow-link t
  'action (lambda (button)
            (helpful-variable (button-get button 'apropos-symbol))))

(define-button-type 'apropos-user-option
  'apropos-label "User option"
  'apropos-short-label "o"
  'face 'apropos-user-option-button
  'help-echo "mouse-2, RET: Display more help on this user option"
  'follow-link t
  'action (lambda (button)
            (helpful-variable (button-get button 'apropos-symbol))))

;; (define-button-type 'apropos-face
;;   'apropos-label "Face"
;;   'apropos-short-label "F"
;;   'face '(font-lock-variable-name-face button)
;;   'help-echo "mouse-2, RET: Display more help on this face"
;;   'follow-link t
;;   'action (lambda (button)
;;             (describe-face (button-get button 'apropos-symbol))))
;;
;; (define-button-type 'apropos-group
;;   'apropos-label "Group"
;;   'apropos-short-label "g"
;;   'face 'apropos-misc-button
;;   'help-echo "mouse-2, RET: Display more help on this group"
;;   'follow-link t
;;   'action (lambda (button)
;;             (customize-group-other-window
;;              (button-get button 'apropos-symbol))))
;;
;; (define-button-type 'apropos-widget
;;   'apropos-label "Widget"
;;   'apropos-short-label "w"
;;   'face 'apropos-misc-button
;;   'help-echo "mouse-2, RET: Display more help on this widget"
;;   'follow-link t
;;   'action (lambda (button)
;;             (widget-browse-other-window (button-get button 'apropos-symbol))))
;;
;; (define-button-type 'apropos-plist
;;   'apropos-label "Properties"
;;   'apropos-short-label "p"
;;   'face 'apropos-misc-button
;;   'help-echo "mouse-2, RET: Display more help on this plist"
;;   'follow-link t
;;   'action (lambda (button)
;;             (apropos-describe-plist (button-get button 'apropos-symbol))))
;;
;; (define-button-type 'apropos-library
;;   'help-echo "mouse-2, RET: Display more help on this library"
;;   'follow-link t
;;   'action (lambda (button)
;;             (apropos-library (button-get button 'apropos-symbol))))
