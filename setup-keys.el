;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys & Menus:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;bad habits broken by dvorak
;(global-unset-key "\C-x\C-c") ;; Keep from killing myself with bad habits.
;(global-unset-key "\C-z") ;; Annoying behavior

(global-set-key 'f1  'undo)                         ;; undo
(global-set-key 'f2  'x-kill-primary-selection)     ;; cut
(global-set-key 'f3  'x-copy-primary-selection)     ;; copy
(global-set-key 'f4  'x-yank-clipboard-selection)   ;; paste
(global-set-key 'f5  'kill-buffer-and-close-window) ;; close
;; f6 unassigned
(global-set-key 'f7  'swap-buffers) ; located in setup-aliases.el
(global-set-key 'f8  'toggle-split) ; located in setup-aliases.el
(global-set-key 'f9  'start-kbd-macro)
(global-set-key 'f10 'end-kbd-macro)
(global-set-key 'f11 'call-last-kbd-macro)
(global-set-key 'f12 'query-replace-regexp)

;; Add Big Brother Database keys
;;(global-set-key "\C-x\M-b" 'bbdb)
;;(global-set-key "\C-x\M-m" 'vm-bugread)

(global-set-key "\C-\M-n" 'insert-buffer-name)

(global-unset-key 'insert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus

(add-menu-button '("Tools") ["----" nil nil])
(add-submenu   '("Tools") '("Server"
			    [ "Start Edit Server" gnuserv-start t ]
			    [ "Stop Edit Server" (gnuserv-start t) t ]
			    [ "Finish Edit" server-edit t ]))
;(add-submenu   '("Tools") '("VM Mail"
;			    [ "Read Bugmail" vm-bugread t ]
;			    [ "Read HelpRequest" vm-hrread t ]))
;;(add-submenu   '("Tools") '("BBDB"
;;			    [ "Grep" bbdb t]
;;			    [ "New" bbdb-create t]))
(add-submenu   '("Tools") '("Misc"
			    [ "Repeat Macro To End" macro-to-end t]
			    [ "Inline Shell on Region" shell-on-region t]
			    [ "Insert Buffer name" insert-buffer-name t]))
