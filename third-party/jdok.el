;; jdok.el --- Javadoc template generator

;; Copyright (C) 1998, 2000 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 8 Oct 1998
;; Version: 1.11
;; Keywords: tools
;; VC: $Id: //depot/main/user/ryand/Bin/elisp/third-party/jdok.el#1 $

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This library helps to document Java classes and methods. The
;; command `jdok-generate-javadoc-template' automatically inserts a
;; javadoc block comment template above the method, class or interface
;; declaration at point (it assumes that the declaration syntax is
;; valid).
;;
;; The command `jdok-customize' (or customize-group jdok) allows you
;; to customize each category of javadoc comment line (description,
;; @version, @return, ...).

;; Installation
;;
;; Put this file on your Emacs load-path and add (require 'jdok) into
;; your Emacs startup file.

;; Support
;;
;; This program is available at <http://www.dponce.com/>. Any
;; comments, suggestions, bug reports or upgrade requests are welcome.
;; Please send them to David Ponce at <david@dponce.com>

;;; Change Log:

;; $Log: jdok.el,v $
;; Revision 1.1  2000/12/21 02:18:06  ryand
;; Added.
;;
;; Revision 1.15  2000/06/09 07:26:26  david_ponce
;; Version 1.11 released.
;;
;; FIXED: When methods/constructors contains a "Class" object jdok
;; mistaked them for classes. This problem (may be others?) arose when
;; `case-fold-search' value is non-nil (searches and matches should
;; ignore case). So, now `jdok-generate-javadoc-template' temporarily
;; forces `case-fold-search' to nil.
;;
;; Thank you very much to Eric D. Friedman <eric@hfriedman.rdsl.lmi.net>
;; who have reported this nasty bug.
;;
;; Revision 1.14  2000/05/12 09:48:52  david_ponce
;; New version 1.11 (beta).
;;
;; This is a major rewrite of jdok.el to use fully customizable "tempo"
;; templates for each category of javadoc line. So now jdok require the
;; tempo package to work.
;;
;; Starting with this version of jdok, customized templates made in
;; previous versions of jdok are no more used. So jdok templates must be
;; customized again.
;;
;; Revision 1.13  2000/04/26 15:24:42  david_ponce
;; New improved `jdok-get-declaration' function. It delegates to a new
;; light Java source scanner `jdok-get-normalized-source' which read the
;; current buffer and return normalized Java source code (extra Java
;; whitespace characters and comments are removed).
;;
;; Revision 1.12  2000/04/21 10:34:22  david_ponce
;; Updated to follow standard Emacs conventions for comments.
;; Version 1.9 released.
;;
;; Revision 1.11  2000/04/13 11:40:36  david_ponce
;; XEmacs compatibility changes:
;;   Specialized functions now handle insertion of @param,
;;   @exception and @see tag and does nothing if a tag value is ""
;;   (split-string different behaviour between Emacs and XEmacs).
;;
;; "final" keyword is now skipped in method/constructor arglist.
;;
;; Code cleanup:
;;  Specialized functions now handle insertion of @return tag and
;;  method/constructor description.
;;  New `jdok-extract-method' function consistent with `jdok-extract-class'
;;  and `jdok-extract-interface'.
;;
;; Thanks to "Malcolm Purvis" <malcolm.purvis@alcatel.com.au> for his
;; help.
;;
;; Revision 1.10  2000/04/11 10:33:17  david_ponce
;; FIXED: `jdok-class-regexp' which failed:
;;   - with 'implements' clause without 'extends' clause
;;   - when no 'extends'/'implements' clause and no space
;;     between class name and '{'.
;;
;; FIXED: `jdok-version-tag' which has an incorrect default
;; value "1.0" replaced by "* @version 1.0".
;;
;; Revision 1.9  2000/04/10 11:55:53  david_ponce
;; Can now generate javadoc template for classes/interfaces.
;; No more require cl.
;; Code cleanup and documentation rewrite.
;;
;; Revision 1.8  1999/05/07 08:08:28  ebat311
;; Added (require 'cl) to avoid problem using `mapcar'
;; and `mapc' from `cl-extra'.
;;
;; Revision 1.7  1999-04-22 23:34:53+02  ebat311
;; Added `autoload' cookies.
;;
;; Revision 1.6  1998/11/05 07:58:05  ebat311
;; `jdok-get-method-signature'  and `jdok-method-throws-regexp'
;; updated to handle abstract method definitions.
;; Thanks to Jerry Boetje <jboetje@healthquality.com> for his
;; contribution.
;;
;; Revision 1.5  1998/11/04 11:12:27  ebat311
;; `jdok-get-method-signature' updated to skip CR characters.
;;
;; Revision 1.4  1998/10/21 11:42:11  ebat311
;; Fixed a problem with TAB characters in method signature. Thanks to
;; Christian Halstrick <Christian.Halstrick@dredsner-bank.com> for his help.
;;
;; Revision 1.3  1998/10/20 10:02:55  ebat311
;; Have run `untabify' on the whole source (follows a remark from ricky@siemensdc.com).
;;
;; Revision 1.2  1998/10/08 13:46:05  ebat311
;; Updated usage comments.
;;
;; Revision 1.1  1998/10/08 13:27:44  ebat311
;; Initial revision
;;

;;; Code:
(require 'tempo)

(defconst jdok-version "1.11 $Date: 2000/12/20 $"
  "jdok version information.")

(defconst jdok-method-type-and-name-regexp
  "\\s-*\\(\\(public\\|protected\\|private\\|const\\|abstract\\|synchronized\\|final\\|static\\|threadsafe\\|transient\\|native\\|volatile\\)\\s-+\\)*\\(.*\\)("
  "Regexp used to find the type and name of a method.")

(defconst jdok-method-type-and-name-match 3
  "Match string index in `jdok-method-type-and-name-regexp'.")

(defconst jdok-method-arglist-regexp
  "([ ]*\\(.*\\)[ ]*)"
  "Regexp used to find method arguments.")

(defconst jdok-method-arglist-match 1
  "Match string index in `jdok-method-arglist-regexp'.")

(defconst jdok-method-throws-regexp
  "throws[ ]+\\(.*\\)[ ]*[{;]"
  "Regexp used to find method exceptions.")

(defconst jdok-method-throws-match 1
  "Match string index in `jdok-method-throws-regexp'.")

(defconst jdok-identifier-regexp "[A-Za-z_$][A-Za-z0-9_$]*"
  "Regexp matching a java identifier.")

(defconst jdok-class-name-regexp
  (concat "\\(\\(\\w+[.]\\)*" jdok-identifier-regexp "\\)")
  "Regexp matching a Java class name.")

(defconst jdok-class-regexp
  (concat "\\<class\\>\\s-+\\(" jdok-identifier-regexp "\\)\\s-*\\(\\<extends\\>\\s-+"
          jdok-class-name-regexp
          "[ {]+\\)?\\(\\<implements\\>\\s-+\\(\\("
          jdok-class-name-regexp
          "[ ,{]+\\)+\\)\\)?")
  "Regexp used to find a class signature.")

(defconst jdok-class-match 1
  "Match string index of the class name in `jdok-class-regexp'.")

(defconst jdok-extends-match 3
  "Match string index of the 'extends' name in `jdok-class-regexp'.")

(defconst jdok-implements-match 6
  "Match string index of 'implements' list in `jdok-class-regexp'.")

(defconst jdok-interface-regexp
  (concat "\\<interface\\>\\s-+\\(" jdok-identifier-regexp
          "\\)\\(\\s-+\\<extends\\>\\s-+\\(\\("
          jdok-class-name-regexp
          "[ ,{]+\\)+\\)\\)?")
  "Regexp used to find an interface signature.")

(defconst jdok-interface-match 1
  "Match string index of the interface name in `jdok-interface-regexp'.")

(defconst jdok-iextends-match 3
  "Match string index of 'extends' list in `jdok-interface-regexp'.")

(defgroup jdok nil
  "Javadoc template generator"
  :group 'tools
  :prefix "jdok-")

;; IMPORTANT: This function must be defined before the following defcustoms
;; because it is used in their :set clause.
(defun jdok-define-template (sym val)
  "Define a template (see `tempo-define-template'). The template name
is the `symbol-name' of SYM from which the '-template' suffix has been
removed, prefixed by 'tempo-template-'. VAL is the template value."
  (let* ((name (symbol-name sym))
         (template-name
          (if (string-match "\\(.*\\)-template$" name)
              (match-string 1 name)
            (error "Invalid template variable name: %S" name))))
    (tempo-define-template template-name val nil name)
    (set-default sym val)))

(defcustom jdok-describe-class-template
  '("* Describe class " (jdok-code name) " here.")
  "*Line template used to describe a class.
If nil the line is not inserted.
The variable 'name' is set to the class name.
See `jdok-generate-javadoc-template' for usage.
Define the template `tempo-template-jdok-describe-class'."
  :group 'jdok
  :type '(repeat sexp)
  :set 'jdok-define-template)

(defcustom jdok-describe-interface-template
  '("* Describe interface " (jdok-code name) " here.")
  "*Line template used to describe an interface.
If nil the line is not inserted.
The variable 'name' is set to the interface name.
See `jdok-generate-javadoc-template' for usage.
Define the template `tempo-template-jdok-describe-interface'."
  :group 'jdok
  :type '(repeat sexp)
  :set 'jdok-define-template)

(defcustom jdok-describe-constructor-template
  '("* Creates a new " (jdok-code name) " instance.")
  "*Line template used to describe a constructor.
If nil the line is not inserted.
The variable 'name' is set to the constructor name (that is the class name).
See `jdok-generate-javadoc-template' for usage.
Define the template `tempo-template-jdok-describe-constructor'."
  :group 'jdok
  :type '(repeat sexp)
  :set 'jdok-define-template)

(defcustom jdok-describe-method-template
  '("* Describe " (jdok-code name) " method here.")
  "*Line template used to describe a method.
If nil the line is not inserted.
The variable 'name' is set to the method name.
See `jdok-generate-javadoc-template' for usage.
Define the template `tempo-template-jdok-describe-method'."
  :group 'jdok
  :type '(repeat sexp)
  :set 'jdok-define-template)

(defcustom jdok-param-tag-template
  '("* @param " name " " (jdok-a type) " " (jdok-code type) " value")
  "*Line template used to describe a parameter.
If nil the line is not inserted.
The variable 'name' is set to the parameter name.
The variable 'type' is set to the parameter type.
A line is inserted for each parameter.
See `jdok-generate-javadoc-template' for usage.
Define the template `tempo-template-jdok-param-tag'."
  :group 'jdok
  :type '(repeat sexp)
  :set 'jdok-define-template)

(defcustom jdok-return-tag-template
  '("* @return " (jdok-a type) " " (jdok-code type) " value")
  "*Line template used to describe a returned value.
If nil the line is not inserted.
The variable 'type' is set to the returned type.
See `jdok-generate-javadoc-template' for usage.
Define the template `tempo-template-jdok-return-tag'."
  :group 'jdok
  :type '(repeat sexp)
  :set 'jdok-define-template)

(defcustom jdok-exception-tag-template
  '("* @exception " type " if an error occurs")
  "*Line template used to describe an exception.
If nil the line is not inserted.
The variable 'type' is set to the exception type.
A line is inserted for each exception in the 'throws' clause.
See `jdok-generate-javadoc-template' for usage.
Define the template `tempo-template-jdok-exception-tag'."
  :group 'jdok
  :type '(repeat sexp)
  :set 'jdok-define-template)

(defcustom jdok-author-tag-template
  '("* @author \"" user-full-name "\" <" user-mail-address ">")
  "*Line template used to give an author.
If nil the line is not inserted.
See `jdok-generate-javadoc-template' for usage.
Define the template `tempo-template-jdok-author-tag'."
  :group 'jdok
  :type '(repeat sexp)
  :set 'jdok-define-template)

(defcustom jdok-version-tag-template
  '("* @version 1.0")
  "*Line template used to give a version.
If nil the line is not inserted.
See `jdok-generate-javadoc-template' for usage.
Define the template `tempo-template-jdok-version-tag'."
  :group 'jdok
  :type '(repeat sexp)
  :set 'jdok-define-template)

(defcustom jdok-see-tag-template
  '("* @see " ref)
  "*Line template used to give a reference.
If nil the line is not inserted.
The variable 'ref' is set to the class or interface name.
A line is inserted for each name in the 'extends' then 'implements' clauses.
See `jdok-generate-javadoc-template' for usage.
Define the template `tempo-template-jdok-see-tag'."
  :group 'jdok
  :type '(repeat sexp)
  :set 'jdok-define-template)

(defcustom jdok-since-tag-template
  '("* @since 1.0")
  "*Line template used to give a since reference.
If nil the line is not inserted.
See `jdok-generate-javadoc-template' for usage.
Define the template `tempo-template-jdok-since-tag'."
  :group 'jdok
  :type '(repeat sexp)
  :set 'jdok-define-template)

(defcustom jdok-load-hook '(jdok-default-load-hook)
   "*Hook run when package has been loaded.
See also `jdok-default-load-hook'."
  :group 'jdok
  :type 'hook)

(defconst jdok-java-whitespaces '(?\ ?\t ?\n ?\r ?\f)
  "Java whitespace characters.
That is space, tab, newline, carriage return and formfeed.")

(defconst jdok-java-quotes '(?\" ?\')
  "List of Java quote characters.
That is delimiters of Java string and character tokens.")

(defconst jdok-java-escape ?\\
  "Escape character in Java string or character tokens.")

(defconst jdok-java-stickies '(?\[ ?\])
  "Remove spaces before these characters.")

(defconst jdok-java-comments '(("/*" . "*/")
                               ("//" . "\n"))
  "List of Java comment delimiters.
Each element must be a pair of strings (STARTER . ENDER)")

(defun jdok-skip-comment ()
  "Maybe skip a Java comment starting just before the point.
Return non-nil if the point has moved just after the comment."
  (let ((delims jdok-java-comments)
        starter ender ender-re found)
    (save-excursion
      (backward-char)
      (while (and delims (not found))
        (setq starter (caar delims))
        (setq ender   (cdar delims))
        (setq found   (looking-at (regexp-quote starter)))
        (setq delims  (cdr  delims))
        ))
    (when found
      (setq found nil)
      (setq ender-re (regexp-quote ender))
      (while (and (not (eobp)) (not found))
        (forward-char)
        (setq found (looking-at ender-re)))
      (and found (forward-char (length ender))))
    found))
       
(defun jdok-get-normalized-source (&optional end-delimiters)
  "Return a normalized source string from the current buffer.

Read the buffer from point location to (and including) next character
found in END-DELIMITERS. When END-DELIMITERS is nil (default) stop
reading at end of buffer. Leave the point location unchanged.

Normalization remove extra whitespace characters and comments."
  (save-excursion
    (let (out                                ; output stack
          c c2)                              ; working characters
      (while (not (eobp))
        (setq c (following-char))
        (forward-char)
        (cond
         ;; Java whitespace
         ((memq c jdok-java-whitespaces)
          (if (not (and (consp out) (eq (car out) ?\ )))
              (setq out (cons ?\  out)))     ; emit a space if necessary
          )
         ;; string or character token
         ((memq c jdok-java-quotes)
          (setq out (cons c out))
          (setq c2 c)
          (setq c nil)
          (while (and (not (eobp)) (not (eq c c2)))
            (setq c (following-char))
            (setq out (cons c out))
            (forward-char)
            (and (eq c jdok-java-escape)     ; emit & skip an escaped character
                 (not (eobp))
                 (setq out (cons (following-char) out))
                 (forward-char))))
         ;; comment
         ((jdok-skip-comment)
          (if (not (and (consp out) (eq (car out) ?\ )))
              (setq out (cons ?\  out)))     ; emit a space if necessary
          )
         ;; other character
         (t
          (if (and (memq c jdok-java-stickies)
                   (consp out) (eq (car out) ?\ ))
              (setcar out c)
            (setq out (cons c out)))
          (if (memq c end-delimiters)        ; if an end delimiter is reached
              (goto-char (point-max)))       ; stop scanning
          )
         ))
      ;; Return the normalized string
      (mapconcat 'char-to-string (nreverse out) ""))))

(defconst jdok-java-declare-ends '(?\; ?\{)
  "List of characters delimiters of declaration end.")

(defun jdok-get-declaration ()
  "Return normalized declaration string at point.

Read the declaration statement in the current buffer from the
beginning of the current line to (and including) the next '{' or ';'
found (see `jdok-get-normalized-source'). The point is moved to the
beginning of the current line.

In this example, point is located after the word 'public'.

  public-|-
  void   myMethod( int  x/*,  int y */)
    throws Exception
  {
    ...

The function returns the string:

  \"public void myMethod( int x ) throws Exception {\""
  (beginning-of-line)
  (jdok-get-normalized-source jdok-java-declare-ends))

(defun jdok-extract-type-and-name (declaration)
  "Return the TYPE-AND-NAME of the method specified by DECLARATION or
nil if not found.

- - TYPE-AND-NAME is a pair (METHOD-TYPE . METHOD-NAME). For a
    constructor METHOD-TYPE is nil."
  (let ((result (and (string-match jdok-method-type-and-name-regexp declaration)
                     (match-string jdok-method-type-and-name-match  declaration))))
    (when result
      (setq result (split-string result "[ ]+"))
      (if (stringp (cadr result))
          (cons (car result) (cadr result))
        (cons nil (car result))))))

(defun jdok-extract-arglist (declaration)
  "Return the ARGUMENT-LIST of the method specified by DECLARATION or
nil if not found.

- - ARGUMENT-LIST is a list
    ((ARG1-TYPE . ARG1-NAME) ... (ARGN-TYPE . ARGN-NAME))."
  (let ((result (and (string-match jdok-method-arglist-regexp declaration)
                     (match-string jdok-method-arglist-match  declaration))))
    (when result
      (setq result (split-string result "[ ]*,[ ]*"))
      (mapcar '(lambda (x)
                 (let ((arg (split-string x "[ ]+")))
                   (if (string= (car arg) "final")
                       (setq arg (cdr arg)))
                   (rplacd arg (cadr arg))
                   arg))
              result))))

(defun jdok-extract-throws (declaration)
  "Return the EXCEPTION-LIST of the method specified by DECLARATION or
nil if not found.

- - EXCEPTION-LIST is a list (EXCEPTION1 ... EXCEPTIONN)."
  (let ((result (and (string-match jdok-method-throws-regexp declaration)
                     (match-string jdok-method-throws-match  declaration))))
    (if result
        (split-string result "\\([ ]*,[ ]*\\|[ ]+\\)"))))

(defun jdok-extract-class (declaration)
  "Return the CLASS-DEFINITION in DECLARATION or nil if not found.

- - CLASS-DEFINITION is a vector
    [CLASS-NAME SUPER-CLASS-NAME INTERFACE-NAME-LIST]
- - CLASS-NAME is the class name.
- - SUPER-CLASS-NAME is the super-class name or nil if missing.
- - INTERFACE-NAME-LIST is the list of interface names or nil if missing.

Examples:
  public class MyClass
    -> [\"MyClass\" nil nil]

  class MyClass implements I, J
    -> [\"MyClass\" nil (\"I\",\"J\")]

  abstract class MyClass extends S implements I
    -> [\"MyClass\" \"S\" (\"I\")]

  public class MyClass extends S
    -> [\"MyClass\" \"S\" nil]"
  (if (string-match jdok-class-regexp declaration)
      (vector (match-string jdok-class-match declaration)
              (match-string jdok-extends-match declaration)
              (split-string (or (match-string jdok-implements-match declaration)
                                "")
                            "[ ,{]+"))))

(defun jdok-extract-interface (declaration)
  "Return the INTERFACE-DEFINITION found in DECLARATION or nil if not found.

- - INTERFACE-DEFINITION is a vector [INTERFACE-NAME SUPER-NAME-LIST]
- - INTERFACE-NAME is the interface name.
- - SUPER-NAME-LIST is the list of super-interface names or nil if missing.

Examples:
  public interface MyInterface
    -> [\"MyInterface\" nil]

  interface MyInterface extends I, J
    -> [\"MyInterface\" (\"I\",\"J\")]

  public interface MyInterface extends I
    -> [\"MyInterface\" (\"I\")]"
  (if (string-match jdok-interface-regexp declaration)
      (vector (match-string jdok-interface-match declaration)
              (split-string (or (match-string jdok-iextends-match declaration)
                                "")
                            "[ ,{]+"))))

(defun jdok-extract-method (declaration)
  "Return the METHOD-DEFINITION found in DECLARATION or nil if not found.

- - METHOD-DEFINITION is a vector [TYPE NAME ARGUMENT-LIST EXCEPTION-LIST]
- - TYPE (see `jdok-extract-type-and-name').
- - NAME (see `jdok-extract-type-and-name').
- - ARGUMENT-LIST (see `jdok-extract-arglist').
- - EXCEPTION-LIST (see `jdok-extract-throws').

Examples:
  public T m()
    -> [\"T\" \"m\" nil nil]

  private static T m(A a)
    -> [\"T\" \"m\" ((\"A\" . \"a\")) nil]

  void m() throws E, F
    -> [\"void\" \"m\" nil (\"E\" \"F\")]

  T m(A a, final B b) throws E
    -> [\"T\" \"m\" ((\"A\" . \"a\") (\"B\" . \"b\")) (\"E\")]"
  (let* ((type-and-name (jdok-extract-type-and-name declaration))
         (arglist       (jdok-extract-arglist       declaration))
         (throws        (jdok-extract-throws        declaration))
         (type          (car type-and-name))
         (name          (cdr type-and-name)))
    (if (or type name arglist throws)
        (vector type name arglist throws))))

(defun jdok-a (word)
  "Return \"an\" if WORD begin with a vowel or \"a\" otherwise.
Useful to generate description like \"an int value\" or \"a long value\"."
  (if (string-match "^[aeiouyAEIOUY]" word)
      "an" "a"))

(defun jdok-code (text)
  "Return \"<code>TEXT</code>\".
Useful to generate HTML code style."
  (concat "<code>" text "</code>"))

(defun jdok-insert-start-block ()
  "Insert a javadoc start comment block '/**'."
  (delete-region (point) (progn (skip-chars-backward " \t") (point)))
  (indent-according-to-mode)
  (insert "/**")
  (reindent-then-newline-and-indent))
  
(defun jdok-insert-empty-line ()
  "Insert an empty javadoc line '*'."
  (insert "*")
  (reindent-then-newline-and-indent))
  
(defun jdok-insert-end-block ()
  "Insert a javadoc end comment block '*/'."
  (insert "*/")
  (reindent-then-newline-and-indent))
  
(defun jdok-insert-template (template-name)
  "Insert the `tempo' line template TEMPLATE-NAME. Does nothing if
TEMPLATE is nil."
  (when (symbol-value template-name)
    (tempo-insert-template template-name nil)
    (reindent-then-newline-and-indent)))

(defun jdok-insert-see-tag (ref)
  "Insert a javadoc @see tag referencing REF."
  (and ref (not (string= ref ""))
       (jdok-insert-template 'tempo-template-jdok-see-tag)))

(defun jdok-insert-param-tag (param)
  "Insert a javadoc @param tag for PARAM value.

- - PARAM is a pair (TYPE . NAME)."
  (and param
       (let ((type (car param))
             (name (cdr param)))
         (and type (not (string= type ""))
              name (not (string= name ""))
              (jdok-insert-template 'tempo-template-jdok-param-tag)))))

(defun jdok-insert-exception-tag (type)
  "Insert a javadoc @exception tag for the exception TYPE."
  (and type (not (string= type ""))
       (jdok-insert-template 'tempo-template-jdok-exception-tag)))

(defun jdok-insert-return-tag (type)
  "Insert a javadoc @return tag of type TYPE."
  (and type (not (string= type "void"))
       (jdok-insert-template 'tempo-template-jdok-return-tag)))

(defun jdok-insert-method-desc (type name)
  "Insert a description for the method NAME of type TYPE.
If TYPE is nil insert a constructor description."
  (and name (not (string= name ""))
       (jdok-insert-template
        (if (and type (not (string= type "")))
            'tempo-template-jdok-describe-method
          'tempo-template-jdok-describe-constructor))))

(defun jdok-insert-class-desc (name)
  "Insert a description for the class NAME."
  (jdok-insert-template 'tempo-template-jdok-describe-class))

(defun jdok-insert-interface-desc (name)
  "Insert a description for the interface NAME."
  (jdok-insert-template 'tempo-template-jdok-describe-interface))

;;;###autoload
(defun jdok-customize ()
  "Show the jdok customization options panel."
  (interactive)
  (customize-group "jdok"))

;;;###autoload
(defun jdok-generate-javadoc-template ()
  "Insert a javadoc block comment template above the class or method
declaration at point (see `jdok-get-declaration' for details on how
the declaration is found). The command assumes that the declaration
syntax is valid.

BEFORE EXECUTING THE COMMAND, THE POINT MUST BE LOCATED AT THE FIRST
LINE OF THE CLASS OR METHOD DECLARATION. IF NOT RESULT IS UNCERTAIN.

In the following examples, point is located at the beginning of the
line, before the word 'public' (but it could be anywhere on this
line):

1- Class example:
   -------------

-|-  public class MyClass
       extends MySuperClass implements Runnable, java.io.Serializable
     {
       ...

\\[jdok-generate-javadoc-template] inserts:

+    /**
+     * Describe class <code>MyClass</code> here.
+     *
+     * @author \"David Ponce\" <david.ponce@wanadoo.fr>
+     * @version 1.0
+     * @since 1.0
+     * @see MySuperClass
+     * @see Runnable
+     * @see java.io.Serializable
+     */
     public class MyClass
       extends MySuperClass implements Runnable, java.io.Serializable
     {
       ...

2- Method example:
   --------------

-|-  public
     void   myMethod( int  x,  int y )
       throws Exception
     {
       ...

\\[jdok-generate-javadoc-template] inserts:

+    /**
+     * Describe <code>myMethod</code> method here.
+     *
+     * @param x an <code>int</code> value
+     * @param y a <code>long</code> value
+     * @exception Exception if an error occurs
+     */
     public
     void   myMethod( int  x,  long y )
       throws Exception
     {
       ...

`tempo' templates are used for each category of javadoc line. The
following templates are currently defined and fully customizable (see
`tempo-define-template' for the different items that can be used in a
tempo template):

- - `jdok-author-tag-template'
- - `jdok-describe-class-template'
- - `jdok-describe-constructor-template'
- - `jdok-describe-interface-template'
- - `jdok-describe-method-template'
- - `jdok-exception-tag-template'
- - `jdok-param-tag-template'
- - `jdok-return-tag-template'
- - `jdok-see-tag-template'
- - `jdok-since-tag-template'
- - `jdok-version-tag-template'

For example if you customize `jdok-describe-class-template' with the
following value:

'(\"* \" (P \"Class description: \"))

you will be asked to enter the class description in the
minibuffer. See also the `jdok-a' and `jdok-code' helper functions."
  (interactive)
  (or (eq major-mode 'jde-mode)
      (eq major-mode 'java-mode)
      (error "Invalid major mode found. Must be 'java-mode' or 'jde-mode'."))
  (let (case-fold-search                ; Ensure that searching is case sensitive
        (declaration (jdok-get-declaration)) def)
    (when declaration
      (jdok-insert-start-block)
      (cond
       ;; Class signature [CLASS-NAME SUPER-CLASS-NAME INTERFACE-NAME-LIST]
       ((setq def (jdok-extract-class declaration))
        (message "class signature: %S" declaration)
        (jdok-insert-class-desc (aref def 0))
        (jdok-insert-empty-line)
        (jdok-insert-template 'tempo-template-jdok-author-tag)
        (jdok-insert-template 'tempo-template-jdok-version-tag)
        (jdok-insert-template 'tempo-template-jdok-since-tag)
        (jdok-insert-see-tag (aref def 1))
        (mapcar 'jdok-insert-see-tag (aref def 2)))
       ;; Interface signature [INTERFACE-NAME SUPER-NAME-LIST]
       ((setq def (jdok-extract-interface declaration))
        (message "interface signature: %S" declaration)
        (jdok-insert-interface-desc (aref def 0))
        (jdok-insert-empty-line)
        (jdok-insert-template 'tempo-template-jdok-author-tag)
        (jdok-insert-template 'tempo-template-jdok-version-tag)
        (jdok-insert-template 'tempo-template-jdok-since-tag)
        (mapcar 'jdok-insert-see-tag (aref def 1)))
       ;; Method signature [TYPE NAME ARGUMENT-LIST EXCEPTION-LIST]
       ((setq def (jdok-extract-method declaration))
        (message "method signature: %S" declaration)
        (jdok-insert-method-desc (aref def 0) (aref def 1))
        (jdok-insert-empty-line)
        (mapcar 'jdok-insert-param-tag (aref def 2))
        (jdok-insert-return-tag (aref def 0))
        (mapcar 'jdok-insert-exception-tag (aref def 3)))
       (t
        (message "unknown declaration: %S" declaration)
        (jdok-insert-empty-line))
       )
      (jdok-insert-end-block))))

(defun jdok-default-load-hook ()
  "Default hook run when package has been loaded. Map the `java-mode'
key `C-cj' to the `jdok-generate-javadoc-template' command."
  (if (not (boundp 'java-mode-map))
      (add-hook 'c-initialization-hook
                '(lambda ()
                   (define-key (symbol-value 'java-mode-map)
                     "\C-cj"  'jdok-generate-javadoc-template))) 
    (define-key (symbol-value 'java-mode-map)
      "\C-cj"  'jdok-generate-javadoc-template)))

(provide 'jdok)
(run-hooks 'jdok-load-hook)

;;; jdok.el ends here
