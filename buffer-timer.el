;
; user setable variables
;
(defvar buffer-timer-idle-limit 300
  "the amount of time to wait for user input before switching to the 
buffer-timer-idle-buffer buffer")

(defvar buffer-timer-output-file    "/home/hardaker/.buffer-timer"
  "the location to store buffer-timer data.  Will get formated using
format-time-string, so date specifications (like file-%Y-%m-%d) will
work.")

(defvar buffer-timer-clear-data-on-filename-change t
  "If t, clears data from the buffer-timer on a filename change.
If a date format string is included in the buffer-timer-output-file variable
then the variable name will not be constant.  When it changes, this
variable indicates if the timers data should be cleared for the new time 
period.")

(defvar buffer-timer-idle-buffer "*idle*"
  "The buffer to switch to when no events have been detected.
Swiched to after buffer-timer-idle-limit seconds.")

(defvar buffer-timer-load-previous  t
  "If t, load the previous existing data file if present")

(defvar buffer-timer-summarize-sort-by 'time
  "Sort reports either by 'time or by 'name'")

(defvar buffer-timer-save-when-idle t
  "Whether we should save buffer-timer data every so often automatically.")

(defvar buffer-timer-small-idle-time 5
  "minimum idle time to wait before saving data")

(defvar buffer-timer-save-every-x-idletimes 5
  "save data only every buffer-timer-save-every-x-idletimes number of idle times.")

(defvar buffer-timer-rename-always  nil
  "if t, sort/classify the buffer names as they are processed.")

(defvar buffer-timer-regexp-merge-list
    '(("^\\*Group\\*"   	      . "news")  ; or maybe mail!
    ("^\\*Summary\\*" 	      . "news")
    ("drafts/[0-9]+$" 	      . "news-post")
    ("^\\*idle\\*"            . "idle")
    ("^\\*cvs.*\\*"           . "cvs")
    ("^\\*compilation\\*"     . "compiling")
    ("^\\*"         	      . "emacs-internal")
    ("^ "         	      . "emacs-really-internal"))
  "A list of (regexp . summary) pairs to make condensed reports from.")

(defvar buffer-timer-regexp-master-list
  '(("news" .  (("group" . "^\\*Group\\*")
		("summary" . "^\\*Summary\\*")
		("out" . "drafts/[0-9]+$")))
    ("idle" .			  "^\\*idle\\*")
    ("cvs" .			  "^\\*cvs.*\\*")           
    ("compiling" .		  "^\\*compilation\\*")
    ("emacs" . (("emacs-internal" .	  "^\\*")
		("emacs-really-internal" . "^ ")))
    )
  "A list of (name . regexp) or (name . ((subname . regexp)...)) type things..."
  )

(defvar buffer-timer-munge-dont-show-zeros t
  "if t, dont display munge results for zero time matches")

(defvar buffer-timer-munge-visible-depth 100
  "Maximum hierarchial depth to show as visible by default.")

(defvar buffer-timer-mouse-face 'highlight
  "*Face used for mouse highlighting in the summary buffer.")

(defvar buffer-timer-display-status-in-modeline t
  "Should the buffer-timer status be displayed in the modeline.")

;
; internal variables
;
(defvar buffer-timer-do-warnings    	  nil)
(defvar buffer-timer-locked         	  nil)
(defvar buffer-timer-debug          	  'file)
(defvar buffer-timer-debug-file   	  "/home/hardaker/.buffer-timer-log")
(defvar buffer-timer-debug-buffer   	  "*buffer-timer-log*")
(defvar buffer-timer-debug-buf            nil)
(defvar buffer-timer-last-file-name 	  nil)
(defvar buffer-timer-last-outputfile-name nil)
(defvar buffer-timer-data           	  nil)
(defvar buffer-timer-start-time     	  (current-time))
(defvar buffer-timer-switch-time    	  nil)
(defvar buffer-timer-switch-idle-time     nil)
(defvar buffer-timer-status               "")
(defvar buffer-timer-locked-xpm "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"13 13 118 2\",
\"   c #19191a\",
\".  c #1b1b1a\",
\"X  c #1e1e1e\",
\"o  c #1e1e22\",
\"O  c #1e1e25\",
\"+  c #1e1e28\",
\"@  c #232320\",
\"#  c Gray17\",
\"$  c Gray18\",
\"%  c #222232\",
\"&  c #333338\",
\"*  c #3c3c3a\",
\"=  c #3c3c3c\",
\"-  c #3f3f3f\",
\";  c #3e402b\",
\":  c #44442d\",
\">  c #444434\",
\",  c #434338\",
\"<  c #655904\",
\"1  c #615c15\",
\"2  c #686800\",
\"3  c #6d6f0e\",
\"4  c #606038\",
\"5  c #7a793d\",
\"6  c #404045\",
\"7  c #464648\",
\"8  c #4b4b4b\",
\"9  c #4e4e4e\",
\"0  c #5f5f41\",
\"q  c #58594c\",
\"w  c #5c5c49\",
\"e  c #505051\",
\"r  c #515151\",
\"t  c #555555\",
\"y  c #5b5b5b\",
\"u  c #65675e\",
\"i  c #727243\",
\"p  c #606060\",
\"a  c #636366\",
\"s  c #656564\",
\"d  c #656565\",
\"f  c #666664\",
\"g  c #66666b\",
\"h  c #707075\",
\"j  c #747474\",
\"k  c Gray46\",
\"l  c #767676\",
\"z  c #7c7c7c\",
\"x  c #7b7c84\",
\"c  c #827806\",
\"v  c #828200\",
\"b  c #858700\",
\"n  c #8d8d00\",
\"m  c #96911f\",
\"M  c #808034\",
\"N  c #92923a\",
\"B  c #999c3d\",
\"V  c #a09822\",
\"C  c #a5a408\",
\"Z  c #a9a904\",
\"A  c #84847f\",
\"S  c #b7b741\",
\"D  c #adad63\",
\"F  c #aaac6f\",
\"G  c #c3c500\",
\"H  c #c2c43e\",
\"J  c #d7cc39\",
\"K  c #d5ca6a\",
\"L  c #838383\",
\"P  c #898989\",
\"I  c #8b8b8b\",
\"U  c #8d8d8d\",
\"Y  c #96968e\",
\"T  c #939393\",
\"R  c Gray62\",
\"E  c #aaad99\",
\"W  c #a1a1a6\",
\"Q  c #a5a5a6\",
\"!  c #aaaaa7\",
\"~  c #a7a7b0\",
\"^  c #a8a8b6\",
\"/  c #b1b1bb\",
\"(  c #b7b7ba\",
\")  c #bebebe\",
\"_  c #bebec1\",
\"`  c #c5a989\",
\"'  c #c0bfa0\",
\"]  c #cccdba\",
\"[  c #d3d4b0\",
\"{  c Gray76\",
\"}  c #c8c7cb\",
\"|  c #cacaca\",
\" . c #cbcbcb\",
\".. c #c1c1d4\",
\"X. c #c9c9dd\",
\"o. c Gray82\",
\"O. c #d5d5d5\",
\"+. c #d6d6d5\",
\"@. c #d6d6d9\",
\"#. c #d6d6df\",
\"$. c #d8d8d7\",
\"%. c #d8d8d8\",
\"&. c #dadbe9\",
\"*. c #e1e1de\",
\"=. c #e9e5d0\",
\"-. c #ffeece\",
\";. c #e9e9ea\",
\":. c #f3f4f3\",
\">. c #f5f7f7\",
\",. c #f9f9f9\",
\"<. c #fffbfc\",
\"1. c Gray99\",
\"2. c #fcfcfd\",
\"3. c #fdfdfc\",
\"4. c #fdfdfd\",
\"5. c #fffdff\",
\"6. c #fefeff\",
\"7. c Gray100\",
/* pixels */
\"7.7.7.,.7.L = = P 7.,.7.7.\",
\"7.7.7.7.| R T P j { 7.7.7.\",
\"7.7.7.O.z U 9 r j s %.7.7.\",
\"7.7.7.| j t o.*.s 8 { 7.7.\",
\"7.7.7.O.s $ $.;.h * ) 7.7.\",
\"7.7.7.x &   7 i : % y 7.7.\",
\"7.7.A 2 5 ~ Y ! D n > 6 7.\",
\"1.7.q m -.J G C Z v 0 O 7.\",
\"1.7., F <.K H S M N w o 7.\",
\"7.7.r #.:.&.X.../ ^ a   7.\",
\"1.7.# ( >.@.} _ W Q p X 7.\",
\"1.7.@ ] 7.=.[ ' E B 4 + 7.\",
\"1.7.u 1 ` V b 3 c < ; g 7.\"
};")
(defvar buffer-timer-locked-gl (make-glyph (vector 'xpm :data buffer-timer-locked-xpm)))
;
; functions
;
(defun bt-warn (arg &rest)
  (if buffer-timer-do-warnings
      (warn arg)))
  
(defun buffer-timer-get-current-buffer-string ()
  (or (buffer-file-name) (buffer-name)))

;
; check against a regexp list for groupings
;
(defun buffer-timer-maybe-rename (name)
  (let ((list buffer-timer-regexp-merge-list)
	(ret nil))
    (while (and list (not ret))
      (if (string-match (caar list) name)
	  (setq ret (cdar list)))
      (setq list (cdr list)))
    (if (not ret)
	(setq ret name))
    ret))

;
; (current-time) is a joke
;
(defun buffer-timer-convert-time (ourtime)
  "converts a high/low integer pair to a real time value"
  (+ (* (first ourtime) 65536) (second ourtime)))

(defun buffer-timer-current-time ()
  "return a real 32 bit time value"
  (buffer-timer-convert-time (current-time)))

(setq buffer-timer-switch-time (buffer-timer-current-time))
(setq buffer-timer-last-file-name (buffer-timer-get-current-buffer-string))

(defvar buffer-timer-recursive-watch nil)
(defun buffer-timer-debug-msg (msg)
  (if buffer-timer-debug
      (save-excursion
	(if (not buffer-timer-recursive-watch)
	    (progn
	      (if (not (bufferp buffer-timer-debug-buf))
		  (progn
		    (setq buffer-timer-recursive-watch t)
		    (if (eq buffer-timer-debug 'file)
			(setq buffer-timer-debug-buf
			      (find-file-noselect 
			       (format-time-string buffer-timer-debug-file)))
		      (setq buffer-timer-debug-buf 
			    (get-buffer-create buffer-timer-debug-buffer)))
		    (setq buffer-timer-recursive-watch nil)))
	      (if (bufferp buffer-timer-debug-buf)
		  (progn
		    (set-buffer buffer-timer-debug-buf)
		    (goto-char (point-max))
		    (insert (format "%s %s" (current-time-string) msg)))
		(message "buffer-timer: couldn't create log")))))))

;
; add a time to an alist
;
(defun buffer-timer-add-time (thetime thename thelist)
  (let ((currentnum (cdr (assoc thename thelist))))
    (if currentnum
	(setcdr (assoc thename thelist) 
		(+ currentnum thetime))
      (cons (cons rename timespent) thelist))))

;
; record the last length of time as associated with a particular buffer
;
(defun buffer-timer-remember (name &optional timeval thelist)
  "remember a length of visiting time"
  (let* ((now (buffer-timer-current-time))
	 (havelist (if thelist t))
	 (thelist (if (not havelist) buffer-timer-data thelist))
	 (timespent (or timeval (- now buffer-timer-switch-time)))
	 (rename (if buffer-timer-rename-always
		     (buffer-timer-maybe-rename name)
		   name))
	 (currentnum (cdr (assoc rename thelist))))
    (if rename
	(if timespent
	    (progn
	      (if currentnum
		  (setcdr (assoc rename thelist) 
			  (+ currentnum timespent))
		(setq thelist (cons (cons rename timespent) thelist)))
	      (buffer-timer-debug-msg 
	       (format "%4d %s %s\n" timespent 
		       (buffer-timer-time-string timespent) rename))))
      (bt-warn "empty buffer name passed in"))
    (setq buffer-timer-switch-time now)
    (if (not havelist)
	(setq buffer-timer-data thelist)
      thelist)))

;
; transfer time from one subject to another
;
(defun buffer-timer-transfer-time (from to timeamount)
  "transfer TIMEAMOUNT seconds from FROM to TO"
  (interactive (list
		(completing-read 
		 (concat "From Subject [" buffer-timer-idle-buffer "]: ")
		 buffer-timer-data nil t nil nil buffer-timer-idle-buffer)
		(completing-read (concat "To Subject: [" 
					 (caar buffer-timer-data) "]: ")
				 buffer-timer-data
				 nil nil nil nil (caar buffer-timer-data))
		(buffer-timer-convert-time-string
		 (let ((tstring
			(buffer-timer-time-string
			 (if buffer-timer-switch-idle-time
			     (+ 300 (- (buffer-timer-current-time) 
				       buffer-timer-switch-idle-time))
			   0))))
		   (read-string (format "Transfer time [%s]: " tstring)
				nil nil tstring)))))
  (buffer-timer-remember from (- 0 timeamount))
  (buffer-timer-remember to timeamount)
  (message (format "transfered %s seconds from %s to %s" 
		   (buffer-timer-time-string timeamount) from to)))

(defun buffer-timer-adjust-time (to timeamount)
  "add TIMEAMOUNT seconds to TO"
  (interactive (list
		(completing-read (concat "To Subject: [" 
					 (caar buffer-timer-data) "]: ")
				 buffer-timer-data
				 nil nil nil nil (caar buffer-timer-data))
		(buffer-timer-convert-time-string
		 (let ((tstring
			(buffer-timer-time-string
			 (if buffer-timer-switch-idle-time
			     (+ 300 (- (buffer-timer-current-time) 
				       buffer-timer-switch-idle-time))
			   0))))
		   (read-string (format "Transfer time [%s]: " tstring)
				nil nil tstring)))))
  (buffer-timer-remember to timeamount)
  (message (format "added %s to %s" (buffer-timer-time-string timeamount) to)))

;
; write out our data to a save file
;
(defun buffer-timer-write-results ()
  (interactive)
  (buffer-timer-write-el-results)
  (buffer-timer-write-text-results)
  (if (and (eq buffer-timer-debug 'file)
	   (bufferp buffer-timer-debug-buf))
      (progn
	(set-buffer buffer-timer-debug-buf)
	(save-buffer)))
)

(defun buffer-timer-create-file-name (&optional inputfilename)
  (let* ((inputfilename (or inputfilename buffer-timer-output-file))
	 (newname (format-time-string inputfilename)))
    (if buffer-timer-clear-data-on-filename-change
	(progn
	  (if (not (equal buffer-timer-last-outputfile-name newname))
	      (buffer-timer-clear))
	  (setq buffer-timer-last-outputfile-name newname)))
    newname))

(defun buffer-timer-write-text-results ()
  (save-excursion
    (let ((buf (find-file-noselect (buffer-timer-create-file-name)))
	  (list buffer-timer-data))
      (set-buffer buf)
      (erase-buffer)
      (while list
	(insert (format "%2d\t%s\n" (cdar list) (caar list)))
	(setq list (cdr list))))
    (save-buffer)))

(defun buffer-timer-write-el-results ()
  (interactive)
  (save-excursion
    (let ((buf 
	   (find-file-noselect (concat (buffer-timer-create-file-name) ".el")))
	  (list buffer-timer-data))
      (set-buffer buf)
      (erase-buffer)
      (insert "(setq buffer-timer-data '(\n")
      (while list
	(insert (format "  (\"%s\" . %2d)\n" (caar list) (cdar list)))
	(setq list (cdr list))))
      (insert "))\n")
    (save-buffer)))

;
; summarize timed data into a seperate buffer
;
(defun buffer-timer-break-time (intime)
  (let* ((hours (/ intime 3600))
	 (minutes (/ (- intime (* hours 3600)) 60))
	 (seconds (mod intime 60)))
    (list seconds minutes hours)))

(defun buffer-timer-time-string (intime)
  (let* ((tlist   (buffer-timer-break-time intime))
	 (seconds (first tlist))
	 (minutes (second tlist))
	 (hours   (third tlist)))
    (format "%2dh %2dm %2ds" hours minutes seconds)))

(defun buffer-timer-reclassify ()
  (interactive)
  (setq buffer-timer-backup-data buffer-timer-data)
  (let ((list buffer-timer-data))
    (setq buffer-timer-data nil)
    (while list
      (buffer-timer-remember (caar list) (cdar list))
      (setq list (cdr list)))))

(defun buffer-timer-sort-by-seconds (a b)
  (< (cdr a) (cdr b)))

(defun buffer-timer-sort-by-name (a b)
  (string-lessp (car a) (car b)))

;
; generate a master report
;
(defun buffer-timer-add-time (thetime thename thelist)
  (let ((currentnum (cdr (assoc thename thelist))))
    (if currentnum
	(setcdr (assoc thename thelist) 
		(+ currentnum thetime))
      (cons (cons rename timespent) thelist))))

(defun buffer-timer-assign-time (name time list)
  (let ((sub (assoc name list)))
    (if (listp sub)
	(setq list (buffer-timer-assign-time name time sub))))
)
(defun buffer-timer-generate-master-summary (&optional inlist)
  (interactive)
  (let ((list (or inlist buffer-timer-data))
	(regexplist buffer-timer-master-list)
	(ret (cons (cons "dummy" 0) nil)))
    (while list
      (setq ret (buffer-timer-assign-time (caar list) (cdar list) ret))
      (setq list (cdr list)))))

;
; 
;

;
; print the straight list of stuff
;
(defun buffer-timer-summarize (&optional sortby)
  (interactive)
  (save-excursion
    (switch-to-buffer-other-window "*buffer-timer-results*")
    (erase-buffer)
    (let ((list (copy-sequence buffer-timer-data))
	  (addedtime 0)
	  (sortby (or sortby buffer-timer-summarize-sort-by))
	  (idletime 0))
      ; sort the list
      (cond
       ((eq sortby 'time)
	(setq list (sort list 'buffer-timer-sort-by-seconds)))
       ((eq sortby 'name)
	(setq list (sort list 'buffer-timer-sort-by-name))))
      ; display the list
      (while list
	(let* ((totaltime (cdar list))
	       (bufname (caar list))
	       (tstring (buffer-timer-time-string totaltime)))
	  (if (> totaltime 0)
	      (progn
		(setq addedtime (+ addedtime totaltime))
		(if (or (equal bufname buffer-timer-idle-buffer)
			(equal bufname "idle"))
		    (setq idletime (+ idletime totaltime)))
		(insert (format "%s\t%s\n" tstring bufname))))
	  (setq list (cdr list))))
	  (insert "----------------------------------------------------------------------\n")
      (if (> idletime 0)
	  (insert (format "%s\tTotal not idle\n" (buffer-timer-time-string 
						  (- addedtime
						     idletime)))))
      (insert (format "%s\tTotal\n" (buffer-timer-time-string addedtime)))
      (insert (format "\nTimer Started:  %s\n" (current-time-string 
					       buffer-timer-start-time)))
      (insert (format "Running for:   %s"
		      (buffer-timer-time-string (- (buffer-timer-current-time)
						   (buffer-timer-convert-time
						    buffer-timer-start-time)))))
    )))

;
; convert a string like "15m 30s" and "1h 20s" to a second count.
;
(defun buffer-timer-convert-time-string (timestr)
  (if (not (string-match "[hsm]" timestr))
      ; straight seconds, no specfiers
      (string-to-int timestr)
    (let ((hrs 0) (min 0) (sec 0) (time 0))
      (if (string-match "\\([0-9]+s\\)" timestr)
	  (setq time (string-to-int
		      (substring timestr (match-beginning 1) 
				 (1- (match-end 1))))))
      (if (string-match "\\([0-9]+m\\)" timestr)
	  (setq time (+ time
			(* 60 (string-to-int
			       (substring timestr 
					  (match-beginning 1) 
					  (1- (match-end 1))))))))
      (if (string-match "\\([0-9]+h\\)" timestr)
	  (setq time (+ time
			(* 3600 (string-to-int
				 (substring timestr 
					    (match-beginning 1) 
					    (1- (match-end 1))))))))
      time)))

; (buffer-timer-convert-time-string "4h")
;
; print the regexp merged stuff
;
(defun buffer-timer-report (&optional sortby)
  (interactive)
  (save-excursion
    (switch-to-buffer-other-window "*buffer-timer-results*")
    (erase-buffer)
    (let ((list (copy-sequence buffer-timer-data))
	  (addedtime 0)
	  (sortby (or sortby buffer-timer-summarize-sort-by))
	  (idletime 0)
	  (reportlist (cons (cons "dummy" 0) nil))
	  )

      ; reclassify the list
      (while list
	(setq reportlist 
	      (buffer-timer-remember 
	       (if buffer-timer-rename-always (caar list) 
		 (buffer-timer-maybe-rename (caar list)))
	       (cdar list) reportlist))
	(setq list (cdr list)))
      
      ; sort the list
      (cond
       ((eq sortby 'time)
	(setq reportlist (sort reportlist 'buffer-timer-sort-by-seconds)))
       ((eq sortby 'name)
	(setq reportlist (sort reportlist 'buffer-timer-sort-by-name))))

      ; display the list
      (while reportlist
	(let* ((totaltime (cdar reportlist))
	       (bufname (caar reportlist))
	       (tstring   (buffer-timer-time-string totaltime)))
	  (if (> totaltime 0)
	      (progn
		(setq addedtime (+ addedtime totaltime))
		(if (or (equal bufname buffer-timer-idle-buffer)
			(equal bufname "idle"))
		    (setq idletime (+ idletime totaltime)))
		(insert (format "%s\t%s\n" tstring bufname))))
	  (setq reportlist (cdr reportlist))))

      ; display a summary count
      (insert "----------------------------------------------------------------------\n")
      (if (> idletime 0)
	  (insert (format "%s\tTotal not idle\n" (buffer-timer-time-string 
						  (- addedtime
						     idletime)))))
      (insert (format "%s\tTotal\n" (buffer-timer-time-string addedtime)))
      (insert (format "\nTimer Started:  %s\n" (current-time-string 
					       buffer-timer-start-time)))
      (insert (format "Running for:   %s"
		      (buffer-timer-time-string (- (buffer-timer-current-time)
						   (buffer-timer-convert-time
						    buffer-timer-start-time)))))
    )))

;
; read in saved data to our list
;
(defun buffer-timer-read-data ()
  (interactive)
  (save-excursion
    (let ((buf (find-file-noselect (buffer-timer-create-file-name))))
      (set-buffer buf)
      (beginning-of-buffer)
      (while list
	(insert (format "%s\t%2d\n" (caar list) (cdar list)))
	(setq list (cdr list))))
    (save-buffer)))

;
; idle timer functions
;
(defvar buffer-timer-do-early-idle-count 0)
(defun buffer-timer-do-early-idle ()
  (interactive)
  (if buffer-timer-save-when-idle
      (progn
;	(message (format "saving data %d" buffer-timer-do-early-idle-count))
	(setq buffer-timer-do-early-idle-count 
	      (+ buffer-timer-do-early-idle-count 1))
	(if (> buffer-timer-do-early-idle-count 
	       buffer-timer-save-every-x-idletimes)
	    (progn
	      (setq buffer-timer-do-early-idle-count 0)
	      (buffer-timer-write-results))))))
  
;
(defun buffer-timer-go-idle (&optional subtracttime)
  "switch to the idle buffer"
  (interactive)
  ; subtract off a certain number of minutes from the current timer
  (if buffer-timer-locked
      (message (concat "not going idle: currently locked to \"" 
		       buffer-timer-locked "\""))
    (if (and subtracttime buffer-timer-switch-time)
	;; we need to manually calculate the times for buffers dealing
	;; with the fact that the last X number of seconds should be
	;; marked as idle.
	(progn 
	  (cond
	   ;; we've switched early.  Only record the idle time.
	   ((> (+ buffer-timer-switch-time subtracttime)
	       (buffer-timer-current-time))
	    (bt-warn (format "buffer-timer: idle timer gave too few seconds: %d"
			     (- (buffer-timer-current-time)
				buffer-timer-switch-time)))
	    (buffer-timer-remember buffer-timer-idle-buffer
				   (- (buffer-timer-current-time)
				      buffer-timer-switch-time)))
	   ;; we've switched and need to remember an amount of time spent
	   ;; in the current buffer.
	   ((< (+ buffer-timer-switch-time subtracttime)
	       (buffer-timer-current-time))
	    (buffer-timer-remember (buffer-timer-get-current-buffer-string)
				   (- (buffer-timer-current-time)
				      buffer-timer-switch-time subtracttime))
	    (buffer-timer-remember buffer-timer-idle-buffer subtracttime))
	   ;; exactly equal.  Only the idle timer is incremented.
	   (t
	    (buffer-timer-remember buffer-timer-idle-buffer subtracttime)))
	  ;; zero the switch time so we don't record anything about the
	  ;; past X amount of time.
	  (setq buffer-timer-switch-time (buffer-timer-current-time)))))
  ;; change to the idle buffer, don't increment anything.
  (setq buffer-timer-switch-idle-time buffer-timer-switch-time)
  (switch-to-buffer buffer-timer-idle-buffer))

(defun buffer-timer-toggle-idle (&optional subtracttime)
  "switch to or from the idle buffer"
  (interactive)
  (if (equal (buffer-name) buffer-timer-idle-buffer)
      ; switch back to something else
      (switch-to-buffer (other-buffer))
    (buffer-timer-go-idle subtracttime)))
;
; easy to use functions
;
(defun buffer-timer-idle-switch (&rest args)
  (let ((newname (buffer-timer-get-current-buffer-string)))
    (if (and
	 (not buffer-timer-locked)
	 (not (eq newname buffer-timer-last-file-name)))
	(progn
	  (buffer-timer-remember buffer-timer-last-file-name)
	  (setq buffer-timer-last-file-name newname)))))

(defun buffer-timer-clear ()
  (interactive)
  (setq buffer-timer-data nil)
  (setq buffer-timer-start-time     (current-time))
)

(defun buffer-timer-lock (lockto)
  (interactive
   (list
    (completing-read (concat "Lock To [" 
			     (buffer-timer-get-current-buffer-string) "]: ")
		     buffer-timer-data
		     nil nil nil nil (buffer-timer-get-current-buffer-string))))
   (setq buffer-timer-lock-started (buffer-timer-current-time))
   (setq buffer-timer-locked lockto)
   (buffer-timer-debug-msg (format "locking to %s\n" lockto))
   (setq buffer-timer-status buffer-timer-locked-gl))

(defun buffer-timer-unlock ()
  (interactive)
  (if buffer-timer-locked
      (let ((time-locked (- (buffer-timer-current-time) 
			    buffer-timer-lock-started)))
	(buffer-timer-remember buffer-timer-locked time-locked)
	(let ((msg (format "locked to %s for %s"
			   buffer-timer-locked
			   (buffer-timer-time-string time-locked))))
	  (message msg)
	  (buffer-timer-debug-msg (format "%s\n" msg)))
	(setq buffer-timer-locked nil))
    (error "buffer-timer: can't unlock since we weren't locked"))
  (setq buffer-timer-status ""))


(defun buffer-timer-view-log ()
  (interactive)
  (if (bufferp buffer-timer-debug-buf)
      (switch-to-buffer buffer-timer-debug-buf)
    (warn "debugging log not turned on")))

;
; complex reporting
;
(defvar buffer-timer-munge-map (make-sparse-keymap "buffer-timer-munge-keys")
  "Keymap to show/hide sub-groups of buffer-timer munge reports.")

(define-key buffer-timer-munge-map [(button2)] 'buffer-timer-toggle-munge-state)
(define-key buffer-timer-munge-map [(button3)] 'buffer-timer-toggle-munge-state)
(define-key buffer-timer-munge-map [(return)] 'buffer-timer-toggle-munge-state)

(defun buffer-timer-make-invis-button (ext &optional subregionext startinvis)
  (if startinvis
      (set-extent-property subregionext 'invisible t))
  (set-extent-property ext 'end-open t)
  (set-extent-property ext 'start-open t)
  (set-extent-property ext 'keymap buffer-timer-munge-map)
  (set-extent-property ext 'mouse-face buffer-timer-mouse-face)
  (set-extent-property ext 'intangible t)
  (if (and subregionext (extentp subregionext))
      (set-extent-property ext 'subregion subregionext))
  ;; Help
  (set-extent-property
   ext 'help-echo
   "button2 toggles visibilty of sub-groups below this one.")
)

(defun buffer-timer-toggle-munge-state (event)
  "Toggle smiley at given point."
  (interactive "e")
  (let* ((ext (event-glyph-extent event))
	 (subregion (if ext (extent-property ext 'subregion)))
	 (subregionvis (if subregion (extent-property ext 'invisible)))
	 (pt (event-closest-point event)))
    (if (not ext)
	(when pt
	  (while 
	      (and
	       (setq ext (extent-at pt (event-buffer event) nil ext 'at))
	       (not (setq subregion (extent-property ext 'subregion)))))))
    (if subregion
	(if (not (extent-property subregion 'invisible))
	    (set-extent-property subregion 'invisible t)
	  (set-extent-property subregion 'invisible nil)))))

(defun buffer-timer-copy-sequence (sequence)
  (let* ((ret (copy-sequence sequence))
	 (iter ret))
    (while (and iter (listp iter))
      (if (listp (car iter))
	  (progn
	    (setcar iter (buffer-timer-copy-sequence (car iter)))
	    (setcar iter (cons 0 (car iter)))))
      (setq iter (cdr iter)))
    ret))

(defun buffer-timer-add-to-master (master addstring value indent)
  (let ((ret nil))
    (while (and (not ret) master)
      (let ((currentnum (caar master))
	    (rest (cdar master)))
	(cond
	 ((and (listp (cdr rest))
	       (listp (cadr rest))
	       (integerp (caadr rest)))
	  (if (setq ret 
		    (buffer-timer-add-to-master (cdr rest) addstring value 
						(format "%s  " indent)))
	      (progn
		(setcar (car master) (+ value currentnum))
		)))
	 ((and (listp (cdr rest))
	       (stringp (cadr rest)))
	  (if (string-match (cadr rest) addstring)
	      (progn
		(setcar (car master) (+ value currentnum))
		(setcdr rest (append (cdr rest) (list addstring value)))
		(setq ret t))
	    ))
	 ((stringp (cdr rest))
	  (if (string-match (cdr rest) addstring)
	      (progn
		(setcar (car master) (+ value currentnum))
		(setcdr rest (list (cdr rest) addstring value))
		(setq ret t))
	    ))
	 (t
	  (if (setq ret 
		    (buffer-timer-add-to-master (cdr rest) addstring value 
						(format "%s  " indent)))
	      (progn
		(setcar (car master) (+ value currentnum))
		)))
	 ))
      (setq master (cdr master)))
    ret))

(defun buffer-timer-munge-sort-by-seconds (a b)
  (> (car a) (car b)))

(defun buffer-timer-munge-sort-by-name (a b)
  (string-lessp (cadr a) (cadr b)))

(defun buffer-timer-display-munge-results (master indent depth)
  (let ((sorted 
	 (cond
	  ((eq buffer-timer-summarize-sort-by 'time)
	   (sort master 'buffer-timer-munge-sort-by-seconds))
	  ((eq buffer-timer-summarize-sort-by 'name)
	   (sort master 'buffer-timer-munge-sort-by-name)))))
    (while sorted
      (let ((ourstart (point)) ext1 ext2)
	(if (and buffer-timer-munge-dont-show-zeros (not (eq 0 (caar sorted))))
	    (progn
	      (insert (format "%s %-30s %10s     %d\n" indent (cadar sorted) 
			      (buffer-timer-time-string (caar sorted))
			      (caar sorted)))
	      (if (and (listp (cddar sorted))
		       (stringp (caddar sorted)))
		  (let ((startpt (point)) (startlist (cdddar sorted)))
		    (while startlist
		      (insert (format "  %s %-30s %10s     %d\n" 
				      indent (car startlist)
				      (buffer-timer-time-string 
				       (second startlist))
				      (second startlist)))
		      (setq startlist (cddr startlist)))
;		    (setq ext1 (make-extent ourstart startpt))
;		    (setq ext2 (make-extent startpt (1- (point))))
		    (let ((newext (make-extent ourstart startpt))
			  (subext (make-extent startpt (point))))
		      (buffer-timer-make-invis-button newext subext
						      (> 1 depth)))
		    ))))
	(if (and (listp (cddar sorted)) (listp (caddar sorted))
		 (integerp (car (caddar sorted))))
	    (let ((substart (point)))
	      (buffer-timer-display-munge-results (cddar sorted) 
						  (concat "  " indent)
						  (1- depth))
	      (let ((newext (make-extent ourstart substart))
		    (subext (make-extent substart (point))))
		(buffer-timer-make-invis-button newext subext
						(> 1 depth)))))
;	(if (and ext1 ext2)
;	    (buffer-timer-make-invis-button ext1 ext2
;					    (> 1 depth)))
	)
      (setq sorted (cdr sorted)))))

;(progn
;  (switch-to-buffer-other-window "*buffer-timer-results*")
;  (make-local-variable 'buffer-timer-data)
;  (load "/home/hardaker/.buffer-timer/timesheet-2000-12-04.el")
;  (buffer-timer-munge buffer-timer-data t)
;  (load "/home/hardaker/.buffer-timer/timesheet-2000-12-05.el")
;  (buffer-timer-munge buffer-timer-data t)
;  (load "/home/hardaker/.buffer-timer/timesheet-2000-12-06.el")
;  (buffer-timer-munge buffer-timer-data t)
;  (kill-local-variable 'buffer-timer-data))

(defun buffer-timer-munge-date-range (daychgone daychgtwo)
  "display info from TODAY-DAYCHGONE to TODAY-DAYCHGTWO"
  (interactive "nNumber of days ago marking start of range to view: \nnNumber of days ago marking end of range to view: ")
  (switch-to-buffer-other-window "*buffer-timer-results*")
  (erase-buffer)
  (kill-local-variable 'buffer-timer-data)
  (make-local-variable 'buffer-timer-data)
  (setq daychgone (- 0 daychgone))
  (setq daychgtwo (- 0 daychgtwo))
  (while (<= daychgone daychgtwo)
    (let* ((now (current-time))
	   (low (+ (second now) (* daychgone 60 60 24)))
	   (high (first now)))
      ; stupid stupid time format.  Who uses 16 bit machines anymore?
      (while (< low 0)
	(setq low (+ low 65536))
	(setq high (- high 1)))
      (while (> low 65535)
	(setq low (- low 65536))
	(setq high (+ high 1)))
      ; insert a date stamp
      (insert (format-time-string "\nDate:  %Y-%m-%d  %a\n" (list high low)))
      (let ((filename 
	     (format-time-string (concat buffer-timer-output-file ".el")
				 (list high low))))
;	(insert (format "File (%d):  %s\n" daychgone filename))
	(if (file-exists-p filename)
	    (progn
	      (load filename)
	      (buffer-timer-munge buffer-timer-data t))
	  (insert "  No data\n")))
      (setq daychgone (1+ daychgone))))
  (kill-local-variable 'buffer-timer-data)
  (buffer-timer-munge-mode))

;(buffer-timer-munge-date-range -15 -1)

(defun buffer-timer-munge (&optional list nodestroy)
  (interactive)
  (switch-to-buffer-other-window "*buffer-timer-results*")
  (kill-local-variable 'buffer-timer-data)
  (if (not nodestroy)
      (erase-buffer))
  (let* ((list (or list (copy-sequence buffer-timer-data)))
	 (master (buffer-timer-copy-sequence buffer-timer-regexp-master-list)))
    (while list
;      (insert (format "starting: %s\n" (caar list)))
      (buffer-timer-add-to-master master (caar list) (cdar list) "")
;      (insert "\n\n")
      (setq list (cdr list)))
    (buffer-timer-display-munge-results master "" 
					buffer-timer-munge-visible-depth)
    )
  (buffer-timer-munge-mode))

(defun buffer-timer-munge-mode ()
  "Major mode for the munge-buffer."
  (interactive)
  (setq mode-name "Munge")
  (setq major-mode 'buffer-timer-munge-mode))

;
; note when we go idle for too long
;
(run-with-idle-timer buffer-timer-idle-limit t 
		     'buffer-timer-go-idle buffer-timer-idle-limit)

(run-with-idle-timer buffer-timer-small-idle-time t 'buffer-timer-do-early-idle)

;
; clean up/saving upon exit of emacs
;
(add-hook 'pre-idle-hook 'buffer-timer-idle-switch)
(add-hook 'kill-emacs-hook 'buffer-timer-write-results)

;
; keybindings
;

; reporting
(global-set-key "\C-cts" 'buffer-timer-summarize)
(global-set-key "\C-ctr" 'buffer-timer-report)
(global-set-key "\C-ctS" 'buffer-timer-write-results)
(global-set-key "\C-ctc" 'buffer-timer-clear)
(global-set-key "\C-ctm" 'buffer-timer-munge)
(global-set-key "\C-ctM" 'buffer-timer-munge-date-range)

; modifying data
(global-set-key "\C-ctt" 'buffer-timer-transfer-time)
(global-set-key "\C-cta" 'buffer-timer-adjust-time)

; locking to a subject
(global-set-key "\C-cti" 'buffer-timer-go-idle)
(global-set-key "\C-ctl" 'buffer-timer-lock)
(global-set-key "\C-ctu" 'buffer-timer-unlock)
(global-set-key "\C-ctL" 'buffer-timer-view-log)

;
; maybe load previous data set
;

(let ((elfile (concat (buffer-timer-create-file-name) ".el")))
  (if (and buffer-timer-load-previous (file-exists-p elfile))
      (load-file elfile)))

;
;
;
;(setq ext-test (make-extent nil nil))
;(set-extent-begin-glyph ext-test gnus-xmas-modeline-glyph)
;(setq buffer-timer-status " ")
;
; setup default mode line
;
(if buffer-timer-display-status-in-modeline
    (progn
      (setq buffer-timer-status "")
      (setq default-modeline-format
	    (append '("") '(buffer-timer-status) (cdr default-modeline-format)))))


