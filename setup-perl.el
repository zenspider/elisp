
;; Perl variables:
;;(setq cperl-hairy t)
;;(autoload 'perl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(setq perl-indent-level 2)
(setq perl-continued-statement-offset 2)
(setq perl-continued-brace-offset 0)
(setq perl-brace-offset 0)
(setq perl-brace-imaginary-offset 0)
(setq perl-label-offset -2)

(defalias 'insert-perl-subroutine (read-kbd-macro
"C-k =item SPC $result SPC = SPC C-y (); 2*RET Description. 2*RET =cut 2*RET sub SPC C-y SPC { 2*RET TAB my SPC $self SPC = SPC shift; 2*RET TAB return SPC 0; RET } TAB 2*RET"))
(global-set-key "\C-cs" 'insert-perl-subroutine)
