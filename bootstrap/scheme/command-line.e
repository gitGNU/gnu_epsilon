;;;;; This is -*- epsilon -*-
;;;;; Command line handling.

;;;;; Copyright (C) 2016 Luca Saiu

;;;;; This file is part of GNU epsilon.

;;;;; GNU epsilon is free software: you can redistribute it and/or modify
;;;;; it under the terms of the GNU General Public License as published by
;;;;; the Free Software Foundation, either version 3 of the License, or
;;;;; (at your option) any later version.

;;;;; GNU epsilon is distributed in the hope that it will be useful,
;;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;; GNU General Public License for more details.

;;;;; You should have received a copy of the GNU General Public License
;;;;; along with GNU epsilon.  If not, see <http://www.gnu.org/licenses/>.


;;;;; Simple access to argv without option interpretation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following global boxes are set by calling command-line:process-args.

;;; A string vector box
(e1:define command-line:argv-box
  (box:make 0))

;;; A string vector box.  Kept synchronized with command-line:non-option-list .
(e1:define command-line:non-option-vector
  (box:make 0))
;;; A string list box.  Kept synchronized with command-line:non-option-vector .
(e1:define command-line:non-option-list
  (box:make 0))


;;;;; Command line interface definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A command line option is defined by a list of names and a type.  All the
;;; names of the same command-line option are regarded as synonymous.

;;; A type, unless void, specifies how to interpret a mandatory argument for an
;;; option.  The argument may be either supplied in the command line right after
;;; the option, or separated by an "=" sign.  Void options accept no explicit
;;; arguments but they can be regarded as boolean switches, false when not
;;; supplied.
;;;
;;; Option arguments have the same lexicon accepted by the epsilon1 reader,
;;; except for the string type, which accepts any character sequence -- no
;;; quoting is allowed.  Of course the user is free to parse arguments of type
;;; string.
;;;
;;; When an option is specified multiple times on the command lines the last
;;; instance takes precedence [FIXME: should there be an option to fail
;;; instead?]

;;; [FIXME: move the relevant part here]


;;;;; Internal implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The possible argument types.
(e1:define-sum command-line:type
  (void)
  (fixnum)
  (fixedpoint)
  (string))

;;; Given a type name as a short ssymbol convenient to use in macros, return the
;;; corresponding full identitifer.
(e1:define (command-line:complete-stype stype)
  (e1:case (sexpression:eject-symbol stype)
    ((void) 'command-line:type-void)
    ((fixnum) 'command-line:type-fixnum)
    ((fixedpoint fixed-point) 'command-line:type-fixedpoint)
    ((string) 'command-line:type-string)
    (else
     (e1:error "type completion: invalid type"))))

;;; Return the default value, as an sexpression, associated to the given type
;;; (as returned by command-line:complete-stype).
(e1:define (command-line:stype->sdefault complete-stype)
  (e1:case (sexpression:eject-symbol complete-stype)
    ((command-line:type-void) '#f)
    ((command-line:type-fixnum) '0)
    ((command-line:type-fixedpoint) '0.0)
    ((command-line:type-string) '"")
    (else
     (e1:error "default value: invalid type"))))

;;; Given an argument type return a string containing "=" or space followed by a
;;; reasonably named metavariable, or an empty string for the void type.  The
;;; result is meant to be used as part of a message to be printed by --help,
;;; as a parameter to be supplied to an option.
(e1:define (command-line:type->demo-arg type)
  (e1:match type
    ((command-line:type-void)
     "")
    ((command-line:type-fixnum)
     " N")
    ((command-line:type-fixedpoint)
     " F")
    ((command-line:type-string)
     " X")
    (else
     (e1:assert #f))))

;;; In a command line option record the names are a string list and the type is
;;; a command-line:option-argument-type object.  The description string should
;;; contain a short message to be shown on --help.  The default field contains
;;; the default value to assign to the argument in case the user didn't supply
;;; the option (by default 0 for fixnum, (0.0 for fixedpoint and an empty string
;;; for string).  Of course the record also contains the actual supplied value
;;; of the appropriate shape, and a boolean field to indicate whether the option
;;; was actually passed.
(e1:define-record command-line:option
  names type default description value supplied)

;;; The currently defined options.
(e1:define command-line:options
  (box:make list:nil)) ;; a string list box.
(e1:define command-line:invocation-name
  (box:make 0)) ;; a string box.

;; A string hash table mapping each recognized option name into the
;; corresponding command-line:option data structure.
(e1:define command-line:option-hash
  (string-hash:make))

;;; Undefine every command-line option.
(e1:define (command-line:unset-options!)
  (box:set! command-line:options list:nil))

(e1:define (command-line:add-option-procedure names type default description)
  (e1:let ((new-option (command-line:option names
                                            type
                                            default
                                            description
                                            default
                                            #f)))
    ;; FIXME: this is correct but doesn't support prefixes.
    ;; FIXME: look at this: https://en.wikipedia.org/wiki/Longest_common_substring_problem
    (e1:dolist (name names)
      (string-hash:set! command-line:option-hash name new-option))
    (box:set! command-line:options (list:cons new-option
                                              (box:get command-line:options)))))

;;; Each descriptor is a list which can take any of the shapes of the arguments
;;; of command-line:add-option, as shown in its comment.
(e1:define-macro (command-line:add-options . descriptors)
  (e1:if (sexpression:null? descriptors)
    '()
    `(e1:begin
       (command-line:add-option ,@(sexpression:car descriptors))
       (command-line:add-options ,@(sexpression:cdr descriptors)))))

;;; Accepted syntaxes:
;;; (command-line:add-option names)
;;; (command-line:add-option names type)
;;; (command-line:add-option names description)
;;; (command-line:add-option names type description)
;;; (command-line:add-option names type default description)
;;; A remarkably ugly definition, but the syntax it allows is convenient.
(e1:define-macro (command-line:add-option names . rest)
  (e1:require (sexpression:list? names))
  (e1:require (list:for-all? (e1:lambda (name) (sexpression:string? name))
                             (sexpression:eject-list names)))
  (e1:let ((rest-length (sexpression:length rest)))
    (e1:require (fixnum:<= rest-length 3))
    (e1:let (((stype sdefault sdescription)
              (e1:case rest-length
                ((0) ;; nothing
                 (e1:bundle 'command-line:type-void
                            '#f
                            '""))
                ((1) ;; type only
                 (e1:let ((first (sexpression:car rest)))
                   (e1:cond ((sexpression:symbol? first)
                             (e1:let* ((stype (command-line:complete-stype first))
                                       (sdefault (command-line:stype->sdefault stype)))
                               (e1:bundle stype
                                          sdefault
                                          '"")))
                            ((sexpression:string? first)
                             (e1:bundle 'command-line:type-void
                                        '#f
                                        first))
                            (else
                             (e1:error "invalid option syntax (A) " (se rest))))))
                ((2) ;; type, description
                 (e1:let ((first (sexpression:car rest))
                          (second (sexpression:cadr rest)))
                   (e1:cond ((e1:and (sexpression:symbol? first)
                                     (sexpression:string? second))
                             (e1:let* ((stype (command-line:complete-stype first))
                                       (sdefault (command-line:stype->sdefault stype)))
                               (e1:bundle stype
                                          sdefault
                                          second)))
                            (else
                             (e1:error "invalid option syntax (B) " (se rest))))))
                ((3) ;; type, default, description
                 (e1:let ((first (sexpression:car rest))
                          (second (sexpression:cadr rest))
                          (third (sexpression:caddr rest)))
                   (e1:cond ((e1:and (sexpression:symbol? first)
                                     (sexpression:string? third))
                             (e1:bundle (command-line:complete-stype first)
                                        second
                                        third))
                            (else
                             (e1:error "invalid option syntax"))))))))
      `(command-line:add-option-procedure (e1:value-list ,@names)
                                          (,stype)
                                          ,sdefault
                                          ,sdescription))))

;;; Add zero or more options: the parameters are an s-list of s-lists, each of
;;; which with the same format of the command-line:add-option arguments.
(e1:define-macro (command-line:add-options . options)
  (e1:if (sexpression:null? options)
    '(e1:bundle)
    `(e1:begin
       (command-line:add-option ,@(sexpression:car options))
       (command-line:add-options ,@(sexpression:cdr options)))))

;;; Undefine current options, then add the specified ones.  Same syntax of
;;; command-line:add-options.
(e1:define-macro (command-line:set-options . options)
  `(e1:begin
     (command-line:unset-options!)
     (command-line:add-options ,@options)))


;;;;; Process the actual command line options we received
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (command-line:get-opt-arg-parameter option option-parameter-option)
  (e1:match option-parameter-option
    ((option:option-none)
     (command-line:option-get-default option))
    ((option:option-some option-parameter-as-string)
     (e1:match (command-line:option-get-type option)
       ((command-line:type-void)
        (e1:when (fixnum:> (string:length option-parameter-as-string) 0)
          (fio:write "Warning: passing " (St option-parameter-as-string)
                     " as a void option parameter\n"))
        0)
       ((command-line:type-fixnum)
        (reader:string->fixnum option-parameter-as-string))
       ((command-line:type-fixedpoint)
        (reader:string-in-simple-dot-notation->fixed-point option-parameter-as-string))
       ((command-line:type-string)
        option-parameter-as-string)
       (else
        (e1:assert #f))))))

;;; The arg should not contain #\= : this has to be split by the caller.
(e1:define (command-line:process-opt-arg option-arg option-parameter-option)
  (e1:require (e1:not (string:has? option-arg #\=)))
  ;;(e1:require (string-hash:has? command-line:option-hash option-arg))
  (e1:unless (string-hash:has? command-line:option-hash option-arg)
    (e1:error "option " (St option-arg) " unknown"))
  #;(fio:write "* Process the opt arg " (St option-arg)
             ;;" with option parameter " (St option-parameter)
             "\n")
  (e1:let* ((option (string-hash:get command-line:option-hash option-arg))
            (option-parameter (command-line:get-opt-arg-parameter option
                                                                  option-parameter-option)))
    (command-line:option-set-supplied! option #t)
    (command-line:option-set-value! option option-parameter)))

;;; Set the argv box to the actual value of command-line arguments.
(e1:define (command-line:set-argv!)
  (box:set! command-line:argv-box (e1:primitive command-line:argv)))

;;; Clear the option state for every possible option.  This is useful before
;;; reprocessing options at exec time.
(e1:define (command-line:clear-options!)
  (e1:dolist (option (box:get command-line:options))
    (e1:let ((default (command-line:option-get-default option)))
      (command-line:option-set-supplied! option #f)
      (command-line:option-set-value! option default))))

;;; Process every command line argument, setting global boxes.  Also process
;;; common options such as --help and --version.
(e1:define (command-line:process-args)
  (e1:let* ((argv (command-line:get-argv)) ;; this calls command-line:set-argv! .
            (argc (vector:length argv)))
    (e1:assert (fixnum:>= argc 1))
    (box:set! command-line:invocation-name
              (vector:get argv 0))
    (e1:let loop ((i 1)
                  (process-options #t))
      (e1:when (fixnum:< i argc)
        (e1:let ((arg (vector:get argv i)))
          (e1:cond ((e1:and process-options
                            (string:equal? arg "--")
                            (string-hash:has? command-line:option-hash arg))
                    #;(fio:write "* Stopping option processing at this point: " (St arg) "\n")
                    (loop (fixnum:1+ i)
                          #f))
                   ((e1:and process-options
                            (fixnum:>= (string:length arg) 1)
                            (whatever:eq? (string:get arg 0) #\-)
                            (string:has? arg #\=))
                    (e1:let (((opt-arg optarg-arg) (string:split arg #\=)))
                      (command-line:process-opt-arg opt-arg (option:option-some optarg-arg)))
                    (loop (fixnum:1+ i)
                          process-options))
                   ((e1:and process-options
                            (fixnum:>= (string:length arg) 1)
                            (whatever:eq? (string:get arg 0) #\-))
                    ;; An option arg with no #\= sign.  If an option parameter
                    ;; is needed, get it from the next arg.
                    (e1:unless (string-hash:has? command-line:option-hash arg)
                      (e1:error "option " (St arg) " unknown"))
                    (e1:let ((option (string-hash:get command-line:option-hash arg)))
                      (e1:match (command-line:option-get-type option)
                        ((command-line:type-void)
                         (command-line:process-opt-arg arg (option:option-none))
                         (loop (fixnum:1+ i)
                               process-options))
                        (else ;; an option parameter is needed
                         (e1:when (fixnum:= (fixnum:1+ i) argc)
                           (e1:error "option parameter missing"))
                         (e1:let ((next-arg (vector:get argv (fixnum:1+ i))))
                           (command-line:process-opt-arg arg
                                                         (option:option-some next-arg)))
                         (loop (fixnum:+ i 2)
                               process-options)))))
                   (else
                    (box:set! command-line:non-option-list
                              (list:cons arg
                                         (box:get command-line:non-option-list)))
                    #;(fio:write "* Non-option arg " (St arg) "\n")
                    (loop (fixnum:1+ i)
                          process-options)))))))
  ;;; Reverse the non-option list, that we build backwards.  Also set the
  ;;; vector version, which may be more convenient sometimes.
  (e1:let* ((reversed-non-options (box:get command-line:non-option-list))
            (non-options (list:reverse reversed-non-options)))
    (box:set! command-line:non-option-list
              non-options)
    (box:set! command-line:non-option-vector
              (vector:list->vector non-options)))
  ;; Handle --help, --version and such.
  (command-line:process-common-options))


;;;;; Access option values after command line processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Access argv as a vector (after command line processing).  The returned vector
;;; contains every supplied string as provided in the input, with no distinction
;;; between options and non-options.
(e1:define (command-line:get-argv)
  (command-line:set-argv!) ;; Make sure that the argv box is up to date.
  (box:get command-line:argv-box))

;;; Return non-option parameters in the order they were supplied, as a list.
(e1:define (command-line:non-option-list)
  (box:get command-line:non-option-list))

;;; Return non-option parameters in the order they were supplied, as a vector.
(e1:define (command-line:non-option-vector)
  (box:get command-line:non-option-vector))

;;; Given an option name return the corresponding option data structure.
(e1:define (command-line:get-option option-name)
  (e1:require (string-hash:has? command-line:option-hash option-name))
  (string-hash:get command-line:option-hash option-name))

;;; Return #t iff an option was supplied having the given name among its names.
;;; Notice that boo
(e1:define (command-line:option-supplied? option-name)
  (e1:let ((option (command-line:get-option option-name)))
    (command-line:option-get-supplied option)))

;;; Return #t iff an option was supplied having the given name among its names.
(e1:define (command-line:option? option-name) ;; Just an alias.
  (command-line:option-supplied? option-name))

(e1:define (command-line:option-value option-name)
  (e1:let ((option (command-line:get-option option-name)))
    (command-line:option-get-value option)))


;;;;; Process common options (--help, --version)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (command-line:process-common-options)
  ;; GNU standards don't explicitly specify which option takes priority between
  ;; --help and --version.  I find it more useful to show help when the user
  ;; asks such a confusing question.
  (e1:when (command-line:option-supplied? "--help")
    (command-line:print-help)
    (unix:exit 0))
  (e1:when (command-line:option-supplied? "--version")
    (command-line:print-version)
    (unix:exit 0)))


;;;;; Program-specific information to be shown on --help or --version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following global boxes can be set with a single call of
;;; command-line:set-info! , using keyword arguments.  Defaults are specified in
;;; the definition of command-line:program-name .

;;; A box containing either 0 or the program name.
(e1:define command-line:program-name
  (box:make 0))

;;; A box containing either 0 or a version string.
(e1:define command-line:program-version
  (box:make 0))

;;; A box containing either 0 or a copyright notice.
(e1:define command-line:copyright
  (box:make 0))

;;; A box containing either 0 or a string, holding an email address to which
;;; the user is invited to report bugs.
(e1:define command-line:bug-email
  (box:make 0))

;;; A box containing either 0 or a string, holding the main authors' names.
(e1:define command-line:authors
  (box:make 0))

;;; A box containing the minimum version of the GNU GPL applying to this
;;; program, as a string.
(e1:define command-line:gpl-minimum-version
  (box:make 0))

;;; A box containing either 0 or a string, holding synopsis information to be
;;; printed right after "Usage: " in the first line of output on --help.
(e1:define command-line:usage
  (box:make 0))

;;; A box containing either 0 or a string, holding a short text to be printed as
;;; a very quick introduction at the beginning on the output of --help.
(e1:define command-line:introduction
  (box:make 0))

;;; A box containing either 0 or a string, holding a short text to be printed as
;;; some additional text at the end of the output of --help.
(e1:define command-line:closing
  (box:make 0))

;;; Set every info box.  Missing values are reset to their default.
(e1:define-with-keywords (command-line:set-info! (program-name 0)
                                                 (program-version 0)
                                                 (copyright 0)
                                                 (bug-email 0)
                                                 (authors 0)
                                                 (gpl-minimum-version "3")
                                                 (usage 0)
                                                 (introduction 0)
                                                 (closing 0))
  (box:set! command-line:program-name program-name)
  (box:set! command-line:program-version program-version)
  (box:set! command-line:copyright copyright)
  (box:set! command-line:bug-email bug-email)
  (box:set! command-line:authors authors)
  (box:set! command-line:gpl-minimum-version gpl-minimum-version)
  (box:set! command-line:usage usage)
  (box:set! command-line:introduction introduction)
  (box:set! command-line:closing closing))

;;; Reset all the info boxes above.
(e1:define (command-line:unset-info!)
  (command-line:set-info!))

;;; Reset info, in order to initialize to default values.
(command-line:unset-info!)


;;;;; Version information printing, for --version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Reset both info boxes and options.
(e1:define (command-line:clear!)
  (command-line:unset-info!)
  (command-line:unset-options!))


;;;;; Version information printing, for --version
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (command-line:print-version)
  (e1:let ((program-name (box:get command-line:program-name))
           (program-version (box:get command-line:program-version)))
    (fio:write (st (e1:if program-name
                     program-name
                     "[unknown program name]"))
               " "
               (st (e1:if program-version
                     program-version
                     "[unknown version]"))
               "\n"))
  (e1:let ((copyright (box:get command-line:copyright)))
    (e1:when copyright
      (fio:write (st copyright) "\n")))
  (e1:let ((gpl-minimum-version (box:get command-line:gpl-minimum-version)))
    (fio:write "This is free software: you are free to change and redistribute it under the\n")
    (fio:write "terms of the GNU GPL version " (st gpl-minimum-version)
               ", or any later version published by the Free\n"
               "Software Foundation: see <http://gnu.org/licenses/gpl.html>.\n")
    (fio:write "There is NO WARRANTY, to the extent permitted by law.\n"))
  (e1:let ((authors (box:get command-line:authors)))
    (e1:when authors
      (fio:write "\nWritten by " (st authors) ".\n"))))


;;;;; Usage help, for --help or --usage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (command-line:print-help)
  (e1:let ((usage (box:get command-line:usage)))
    (e1:if usage
      (fio:write "Usage: " (st usage) "\n")
      (fio:write "Usage: " (st (vector:get (command-line:get-argv) 0))
                 " [OPTION-OR-FILENAME]...\n")))
  (e1:let ((introduction (box:get command-line:introduction)))
    (e1:when introduction
      (fio:write (st introduction) "\n")))
  (fio:write "Options:\n")
  (e1:let ((max-width (box:make 0)))
    (e1:dolist (option (box:get command-line:options))
      (e1:dolist (option-name (command-line:option-get-names option))
        (e1:let* ((type (command-line:option-get-type option))
                  (demo-arg (command-line:type->demo-arg type))
                  (demo-arg-size (string:length demo-arg)))
          (box:set! max-width
                    (fixnum:max (box:get max-width)
                                (fixnum:+ (string:length option-name)
                                          demo-arg-size))))))
    (e1:dolist (option (list:reverse (box:get command-line:options)))
      (e1:let loop ((names (command-line:option-get-names option)))
        (e1:let* ((first-name (list:head names))
                  (type (command-line:option-get-type option))
                  (demo-arg (command-line:type->demo-arg type))
                  (demo-arg-size (string:length demo-arg))
                  (more-names (list:tail names))
                  (first-name-length (string:length first-name))
                  (space-no (fixnum:- (box:get max-width)
                                      first-name-length
                                      demo-arg-size
                                      -1)))
          (fio:write "  " ;; The indentation looks nice and helps help2man.
                     (st first-name) (st demo-arg))
          (e1:if (list:null? more-names)
            (e1:begin
              (e1:dotimes (i space-no)
                (fio:write (c #\space)))
              (fio:write " " (st (command-line:option-get-description option))
                         "\n"))
            (e1:begin
              (fio:write ",\n")
              (loop more-names))))))
    (e1:let ((closing (box:get command-line:closing))
             (bug-email (box:get command-line:bug-email)))
      (e1:when closing
        (fio:write "\n" (st closing) "\n"))
      (e1:when bug-email
        (fio:write "\nReport bugs to <" (st bug-email) ">.\n")))))


;;;;; Common defined options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following options are required by the GNU standards.
(e1:define (command-line:add-common-options)
  (command-line:add-options (("--") "stop option processing")
                            (("--help" #;"--usage" #;"-h") "display this help and exit")
                            (("--version" #;"-v") "show version info and exit")))

;;; Unset every previously defined option, then set common ones.
(e1:define (command-line:set-common-options)
  (command-line:unset-options!)
  (command-line:add-common-options))

;;; Automatically define common GNU options, which should be supported by every program.
(command-line:set-common-options)
