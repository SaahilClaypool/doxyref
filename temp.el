;; saahil claypool
;; "Wed May 11 11:17:56 2016"

;; VERSION: 1.1 : multiple files


;; TODO: 
;;      go back to string suffix ?
;;      save location for comapact mode

;; FIXES:
;;      no more macros
;;      highlight by default (doesnt quite work)
;;      wont miss functions
;;            before would skip the next line after each comment
;;      looks at only c cpp h hpp java files




;; (setq referenceDir "~/.emacs.d/reference")
;; (load "~/.emacs.d/doxyref/doxyref.el")
;; (global-set-key (kbd "C-c d d") (lambda ()(interactive)(doxyRef-lookup-docs)))
;; (global-set-key (kbd "C-c d s") (lambda ()(interactive)(doxyRef-setup-project)))




;; SETUP

;; misc
(setq max-lisp-eval-depth 100000);; overkill for large files
(setq max-specpdl-size 100000)
(setq max-functions 1000)
(setq max-files 500)
(setq funcSymbol "_func_") ;; any unique symbol works
(setq configuration (current-window-configuration))
;; location
(if (not (file-exists-p referenceDir)) ;; setup directory if it is not setup 
    (make-directory referenceDir))
;; comment characters
(setq blockCommentStart (list "/\\*\\*")) ;; list of possbile block comment starters
(setq blockCommentEnd (list "\\*/")) ;; list of possible block comment enders
(setq singleComments (list "///")) ;; list of possible single comments


;; last query (starts nil)
;; store last query for re printing
(setq compact nil) ;; print expanded by default
(setq last-function "**")
(setq last-class "**")
(setq last-project "**")

(defun add-single-comment (str)
  "Note: must exit regex symbols. /** become /\\*\\*"
  (setq singleComments (append singleComments (list str))
        ) 
  )
(defun add-block-comment-start (str)
  "Note: must exit regex symbols. /** become /\\*\\*"
  (setq blockCommentStart (append blockCommentStart (list str))
        ) 
  )
(defun add-block-comment-end (str)
  (setq blockCommentEnd (append blockCommentEnd (list str))
        )
  )

(defun string-match-any (str los)
  (if los
      (or (string-match (car los)
                        str
                        )
          (string-match-any str (cdr los)))
    nil))


;;
    ;; (let* (
    ;;        (project (read-string "Project (default ALL):  "))
    ;;        (class (read-string "Class (default ALL): "))
    ;;        (function (read-string "Function (default ALL): "))

    ;;        (filterProj (if (not (= (length project) 0))
    ;;                         (filter-project project aLos)
    ;;                       aLos))
    ;;        (filterClass (if (not (= (length class) 0))
    ;;                         (filter-class class filterProj)
    ;;                       filterProj))
    ;;        (filterFunction (if (not (= (length function) 0))
    ;;                       (filter-function function filterClass)
    ;;                         filterClass))

    ;;        )

;; takes string file name
(defun doxyRef-lookup-docs ()
  (let* (
         (proj-string (read-string (format "Project (** for all  , default %s):   " last-project)))
         (class-string (read-string (format"Class (** for all  , default %s):   " last-class)))
         (func-string (read-string (format "Function (** for all  , default %s):   " last-function)))

         (project (if (= (length proj-string)0)
                      last-project
                    proj-string
                    )
                  )
         (class (if (= (length class-string) 0)
                    last-class
                  class-string)
                )
         (function (if (= (length func-string) 0)
                       last-function
                     func-string)
                   )

         
         (projects (directory-files referenceDir));; all files in the reference directory 
         (filterProj (my-filter (lambda (proj) (string-match project proj))
                                projects)
                     );; list of projects matching 
         (filterClass (my-filter (lambda (a-class) (string-match class a-class))
                                 (files-in filterProj)
                                 )
                      ) ;; list of classes matching
         (filterFunction  (get-matching-functions filterClass 0 function 0)) ;; list of formatted functions
         ;; probably: get-matching-functions (list of file  filter-class)
         )
    
    
    (setq last-function function)
    (setq last-project project)
    (setq last-class class)
    (setq last-query nil)
    (new-window-print filterFunction);; need to get the list of string to print 
    
      )
  )





(defun files-in (lodir)
  (if lodir
      (if (not (or
                (string= ".."
                         (car lodir))
                (string= "."
                         (car lodir))))
          
          (append (mapcar
                   (lambda (file)
                     (concat (car lodir) "/" file)
                     )
                   (directory-files (concat referenceDir "/"(car lodir)))
                   )
                  (files-in (cdr lodir)))
        
        (files-in (cdr lodir))
        )
    nil
    )
  )



(defun get-matching-functions (lof numMatches str numFiles)
  (if (and (< numMatches max-functions )
           (< numFiles max-files))
      (if lof
          (let*
              (
               (cur (car lof))
               )
            (if (not (or (str-suffix= "." cur)
                         (str-suffix= ".." cur)))
                (let* (
                       (functions (filter-function str (read-functions-from-file (concat referenceDir "/" cur))))
                       (numberFunctions (length functions))
                       )
                   (append functions
                        (get-matching-functions (cdr lof) (+ numberFunctions numMatches) str (+ 1 numFiles)))
                  )
              (get-matching-functions (cdr lof) numMatches str (+ 1 numFiles))
              )
            )
        nil)
    nil)
  )

(defun str-suffix= (suf str)
  (string=
   (substring str
              (- 0 (length suf)))
   suf)
      
  )




(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))
(defun read-functions-from-file (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) funcSymbol t)))

(defun read-lines (filePath)
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n")))



;; returns a list of strings that contain this matching string. 
(defun filter-strings (q strs)
  (if strs
      (if

          (string-match q (car strs))
          (cons (car strs);; if true, append this line to the rest
             (filter-strings q (cdr strs)))
        (filter-strings q (cdr strs));; else just return the rest
       
       )
    nil)
  )

(defun filter-class (q strs)
    (if strs
      (if
          (string-match q
                        (nth 2 (split-string (car strs) "\n"))
                        )
          (cons (car strs);; if true, append this line to the rest
                (filter-class q (cdr strs)))
        (filter-class q (cdr strs));; else just return the rest
       
       )
      nil)
  )

(defun filter-function (q strs)
    (if strs
      (if
          (string-match q
                        (nth 3 (split-string (car strs) "\n"))
                        )
          (cons (car strs);; if true, append this line to the rest
                (filter-function q (cdr strs)))
        (filter-function q (cdr strs));; else just return the rest
       
       )
      nil)
    )
(defun filter-project (q strs)
    (if strs
      (if
          (string-match q
                        (nth 1 (split-string (car strs) "\n"))
                        )
          (cons (car strs);; if true, append this line to the rest
                (filter-project q (cdr strs)))
        (filter-project q (cdr strs));; else just return the rest
       
       )
      nil)
    )

(defun join-list-string (los)
  (if los
      (concat (car los) "\n"
              (join-list-string (cdr los)))
    ""
    )
  )

;; first line class
;; second line function
;; rest description
(defun pretty-print (aList)
  (if aList
      (progn
        (if (not (= 0 (length (car aList))))
            (let* (
                   (width (- (window-body-width) 1))
                   (line (split-string (car aList) "\n"))
                   )
              (pop line)
              (insert-char 45 width )
              (insert "\nProject:         ")
              (insert (pop line))
              (insert "\nFile:            ")              
              (insert (pop line))
              (insert "\n\nFunction:        ")
	      (setf line (nthcdr (insert-function line nil 0) line))
              (insert "\n\n")
              (insert (join-list-string line))
              
              )
          nil
          )
        (pretty-print (cdr aList)))
    )
  nil
  )

(defun insert-function (loline open count) 
  (if loline
      (let* (
	     (cur (pop loline))
	     )
	(if (and (or (string-match "(" cur)
		     open)
		 (not (string-match ")"
				    cur)))
	    (progn
	      (insert cur)
	      (insert "\n")
	      (insert-function loline t (+ 1 count))
	      )
	  (progn
	    (insert cur)
	    (+ 1 count))
	)
	)
    count
    )
  )
(defun pretty-print-compact (aList)
  (if aList
      (progn
        (if (not (= 0 (length (car aList))))
            (let* (
                   (width (- (window-body-width) 1))
                   (line (split-string (car aList) "\n"))
                   )
              (pop line)
              (insert-char 45 width )
              (pop line) ;; remove project
              (pop line) ;; remove file
              (insert "\nFunction:        ")
              (setf line (nthcdr (insert-function line nil 0) line))
              (insert "\n")
              ;;(insert (join-list-string line)) remove comment

              )
          nil
          )
        (pretty-print-compact (cdr aList)))
    )
  nil
  )
  


;; takes a list of strings, puts them in the other window
(defun new-window-print (aLos)
  (progn
    (setq configuration (current-window-configuration))
    (setq last-query aLos)
    (switch-to-buffer-other-window "doxyBuffer")
    (erase-buffer)
    (if compact
        (pretty-print-compact aLos)
      (pretty-print aLos)
      )
    (insert-char 45 (- (window-body-width) 1) )
    (beginning-of-buffer)
    (font-lock-mode 1)
    (doxyRef-mode 1)
    (highlight-function)
    )
  )


(defun re-print()
  (erase-buffer)
  (if compact
      (pretty-print-compact last-query)
    (pretty-print last-query)
    )
  (insert-char 45 (- (window-body-width)1) )
  (beginning-of-buffer)
  (highlight-function)
  )

  

(defun next-function ()
  ;;(concat (concat "\C-s return " (make-string (window-body-width) ?-) "\C-s\C-a") )
  [?\C-n ?\C-s return ?- ?- ?- ?- return ?\C-a]
      )

   
(defun next-function ()
  (next-line)
  (search-forward "----")
  (move-beginning-of-line nil)
  )
(defun prev-function ()
  ;;(concat (concat "\C-r return " (make-string (window-body-width)  ?-) "\C-a") )
   [?\C-p ?\C-r return ?- ?- ?- ?- return ?\C-a]
   )
(defun prev-function ()
  (previous-line)
  (search-backward "----")
  (move-beginning-of-line nil)
  )
;; (defun quit-function ()
;;    "\C-x1\C-xk\C-m")
 (defun quit-function()
    (kill-buffer "doxyBuffer")
    (other-window 1)
    (set-window-configuration configuration)
    )

(defun paste-function ()
  (search-forward "Function")
  (search-forward ")")
  (let* (
         (close (point))
         )
    (backward-sexp)
    (backward-sexp)
    (kill-ring-save (point) close)
    (kill-buffer "doxyBuffer")
    (other-window 1)
    (set-window-configuration configuration)
    (yank)
    )
  )

(defun highlight-function ()
  "highlight all FUNCTION: "

  (hi-lock-line-face-buffer "Function:" "warning" )
  
  )


(defun compact-function-toggle ()
  (setq compact (not compact))
  (search-forward "Function:")
  (let*(
        (start (point))
        )
    (move-end-of-line 1)
    (kill-ring-save start (point))
    )
  (re-print)
  (search-forward (car kill-ring))
  (search-backward "----")
  (move-beginning-of-line 1)
  (recenter (nth 1 recenter-positions))
  (highlight-function)
  )







(define-minor-mode doxyRef-mode
  "navigate the doc buffer"
  :lighter " doxyRef"
  :keymap (let (
                (map (make-keymap))
                )
            (define-key map (kbd "n") (lambda () (interactive) (next-function)))
            (define-key map (kbd "p") (lambda () (interactive) (prev-function)))
            (define-key map (kbd "i") (lambda () (interactive)(paste-function)))
            (define-key map (kbd "q") (lambda () (interactive) (quit-function)))
           ;; (define-key map (kbd "h") (lambda () (interactive) (highlight-function)))
            (define-key map (kbd "c") (lambda () (interactive)(compact-function-toggle)))
            map
            ) 
  )
   

;;(filter-strings "flag 1" lines)
;;(filter-strings "test" (list "this is a test" "this is another test"))
;;(filter-class "test" (list "this is a test" "this is another test"))
;;(pretty-print lines)

;; PARSING SECTION

;; Function: // change? 
;; file
;; function
;; description
(defun parse-single-file (fileName outputFile projName)
  
  "given file name and output file puts the reference information in that file"
  (progn
    (with-temp-buffer
;;      (insert "parse-SINGLE-files\n")
      (insert fileName)
      (append-to-buffer "output" nil nil)
      )

  (let*
      (
       (lines (read-lines fileName))
       (functions  (parse-strings lines nil))
       )
    ;; for each pair, write :_func_ ret fileName ret function ret description

    (with-temp-buffer
      (output-functions fileName functions projName)
      (write-region nil nil outputFile t)
      
      )
    ;;    functions
    nil
    )
  ))
;; takes in list of (function . desc)
(defun output-functions (fileName functions projName)
  "takes in file name and list of functions. Outputs them as symbol separated list"
  (if functions
      (progn
        (insert funcSymbol)
        (insert "\n")
        (insert projName)
        (insert "\n")
        (insert fileName)
        (insert "\n")
        (insert (nth 0 (car functions)))
        (insert "\n")
        (insert (join-list-string (nth 1 (car functions))))
        (output-functions fileName (cdr functions) projName)
        )
    )
  nil
  )


;; return list of all function desc combos (function . desc)
(defun parse-strings (los listPair)
  ;; if doxy comment, return list of (function description))
  (if los
      (if (string-match-any (car los)
                            singleComments
                            )
                        
          (let* (
                 (pair (parse-comment los nil 0))
                 (count (nth 2 pair))
                 (function (nth 0 pair))
                 (desc (nth 1 pair))
                 )
            (parse-strings (nthcdr (- count 1) los)
                           (cons (list function desc) listPair))
            )
        (if (string-match-any (car los)
                              blockCommentStart
                              )
            (let* (
                   (pair (parse-block-comment los nil 0))
                   (count (nth 2 pair))
                   (function (nth 0 pair))
                   (desc (nth 1 pair))
                   )
              (progn
                (with-temp-buffer
;;                  (insert (format "after the entire parse %s\n"
;;                                  desc))
                  (append-to-buffer "output" nil nil))
                (parse-strings (nthcdr (- count 1) los)
                               (cons (list function desc) listPair)))

              )
          (parse-strings (cdr los) listPair)
          )
        )
    listPair

    )
  )
;; parse block comment
(defun parse-block-comment (los listDesc lineCount)
  "parse block doxy comment and get the next function it refers to"
  (progn
    (with-temp-buffer
;;      (insert "parse block comment\n")
;;      (insert (format"desciption: %s\n" listDesc))
;;      (insert (format"current: %s\n" (if los (append   listDesc (list (car los))) "nil")))

      (append-to-buffer "output" nil nil)
      )
  (if los
      (let* (
             (curLine (car los))
             )
        (if (not (string-match-any  curLine
                                    blockCommentEnd
                                    )
                 )
            (progn
              (with-temp-buffer
;;                (insert (format "NOT THE END current line: %s current description: %s\n\n\n" curLine listDesc ))
                (append-to-buffer "output" nil nil))
              (parse-block-comment (cdr los)
                           (append  listDesc (list curLine))
                           (+ 1 lineCount)))
          ;; else, return the next nonEmpty line as a list with the description
          (let*(
                (non-empt-ret (next-non-empty (cdr los) 0))
                (empty-count (nth 1 non-empt-ret))
                (non-empty-str (nth 0 non-empt-ret))
                )
            (progn
              (with-temp-buffer
;;                (insert (format "THE END \n non empt %s count %d \n\n\n\n" non-empty-str empty-count ))
                (append-to-buffer "output" nil nil))
              (list non-empty-str (append listDesc (list curLine)) (+ 2 empty-count lineCount)))

            )
          
          
          )
        )
    (list "" listDesc lineCount)
    )
  ))

;; keep adding to list of Desc, when last line is not ///, go until non empty line
;; return cons (function description lineCount) 
(defun parse-comment (los listDesc lineCount)
  (if los
      (let* (
             (curLine (car los))
             )
        (if (string-match-any curLine
                              singleComments)
            (parse-comment (cdr los)
                           (append   listDesc (list curLine))
                           (+ 1 lineCount))
          ;; else, return the next nonEmpty line as a list with the description
          (let*(
                (non-empt-ret (next-non-empty los 0))
                (empty-count (nth 1 non-empt-ret))
                (non-empty-str (nth 0 non-empt-ret))
              )
            (list non-empty-str listDesc (+ 1 empty-count lineCount))
            )
          
          
          )
        )
    (list "" listDesc lineCount)
    )
  )

;; return string . count
(defun next-non-empty (los count)
    (if los
        (if (= (length (car los)) 0);; if str len is 0
            (next-non-empty (cdr los) (+ 1 count)) ;; return the next non empty
          (get-multi-line-function los
				   count
				   nil
				   (string-match "(" (car los)));; else reutrn this
          )
      (list "" count)
      )
    )
(defun get-multi-line-function (los count curFunction openPren) 
  "return (list function count"
  (if los 
      (if (and openPren
	       (not (string-match ")" (car los))))
	  (get-multi-line-function (cdr los)
				   (+ count 1)
				   (append curFunction (list (car los)))
				   openPren)
	(list (join-list-string (append curFunction (list (car los)))) (+ 1 count))
	)
    (list curFunction count)
    )
  )

;; each output file changed to project / output file
(defun doxyRef-setup-project()
  (let*(
        (root (read-string "Enter Path of Project Root (default current directory): "))
        (name (read-string "Enter Project Name: "))
        )
    (if (= 0 (length name))
        (message "Must Enter Project Name")
      (if (not (= (length root) 0 ))
          (setup-helper root (concat referenceDir "/" name) name)
        (setup-helper default-directory (concat referenceDir "/" name) name)
        )
      )
    )
  )


;; need to go and make a for every file in directory function
;; for each .h file

(defun setup-helper (dir outputDirectory projName)
  (if (not (file-exists-p outputDirectory)) ;; setup directory if it is not setup 
    (make-directory outputDirectory))
  (let* (
         (fileNames (directory-files  dir))
         )
    (parse-list-file fileNames outputDirectory dir projName)
    (parse-dirs fileNames outputDirectory dir projName)
    )
  )

;; my filter
(defun my-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
;;(my-filter (lambda (x) (string= x "a"))(list "a" "b" "c"))


(defun parse-dirs (lof outputDirectory dir projName)
  (progn
    (with-temp-buffer
;;      (insert "parse-directory-files\n")
;;      (if lof
;;          (insert (car lof)))
;;      (append-to-buffer "output" nil nil)
      )

  (if lof
      (let* (
             (firstName (car lof))
             )
        (if (and
             (not(string= (substring firstName 0 1) "."))
             (not (or (string= firstName ".") (string= firstName "..")))
             (nth 0 (file-attributes (concat dir "/" firstName))))
            (let* (
                   (subDir (concat dir "/" firstName))
                   (subLof (my-filter
                            (lambda (x) (not (string= "." (substring x 0 1))))
                            (directory-files subDir)))
                   )
              
                
              (parse-list-file subLof outputDirectory subDir projName)
              (parse-dirs subLof outputDirectory subDir projName)
              (parse-dirs  (cdr lof) outputDirectory dir projName)
              )
          (parse-dirs (cdr lof) outputDirectory dir projName)
          )
        )
    nil
    )
  )
  )


(defun parse-list-file (lof outputDirectory dir projName)
  (progn
    (with-temp-buffer
;;      (insert "parse-list-files\n")
;;      (if lof
;;          (insert (car lof)))
;;      (append-to-buffer "output" nil nil)
      )

    
  (if lof
      (let* (
             (curFileShort (car lof))
             (curFile (concat dir "/" curFileShort))
             )
        (if (and
             (not
              (nth 0 (file-attributes curFile)))
             (or (string-match ".cpp" curFile )
                 (string-match ".c" curFile )
                 (string-match ".hpp" curFile )
                 (string-match ".h" curFile )
                 )
             )
            
            (progn
              (parse-single-file curFile
                                 (concat outputDirectory "/" curFileShort)
                                 projName)
              (parse-list-file (cdr lof) outputDirectory dir projName)
              )
          (parse-list-file (cdr lof) outputDirectory dir projName)
          )
        )
    nil
    )
  )
  )
