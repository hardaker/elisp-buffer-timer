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

(defvar buffer-timer-save-every-x-idletimes 15)
  "save data only every buffer-timer-save-every-x-idletimes number of idle times.")

(defvar buffer-timer-rename-always  nil
  "if t, sort/classify the buffer names as they are processed.")

(defvar buffer-timer-regexp-merge-list
  "A list of (regexp . summary) pairs to make condensed reports from."
  '(("^\\*Group\\*"   	      . "news")  ; or maybe mail!
    ("^\\*Summary\\*" 	      . "news")
    ("drafts/[0-9]+$" 	      . "news-post")
    ("^\\*idle\\*"            . "idle")
    ("^\\*cvs.*\\*"           . "cvs")
    ("^\\*compilation\\*"     . "compiling")
    ("^\\*"         	      . "emacs-internal")
    ("^ "         	      . "emacs-really-internal")))

;
; internal variables
;
(defvar buffer-timer-do-warnings    nil)
(defvar buffer-timer-locked         nil)
(defvar buffer-timer-debug          nil)
(defvar buffer-timer-debug-buffer   "*buffer-timer-log*")
(defvar buffer-timer-last-file-name nil)
(defvar buffer-timer-last-outputfile-name nil)
(defvar buffer-timer-data           nil)
(defvar buffer-timer-start-time     (current-time))
(defvar buffer-timer-switch-time    nil)
(defvar buffer-timer-regexp-ignore-switch-errors
  '("^ \\*Minibuf"))

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

(defun buffer-timer-debug-msg (msg)
  (save-excursion
    (set-buffer (get-buffer-create buffer-timer-debug-buffer))
    (goto-char (point-max))
    (insert msg)))

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
	      (if buffer-timer-debug
		  (buffer-timer-debug-msg 
		   (format "%4d\t%s\n" timespent rename)))))
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
		(read-number "Number of Seconds: " t)))
  (buffer-timer-remember from (- 0 timeamount))
  (buffer-timer-remember to timeamount)
  (message (format "transfered %s seconds from %s to %s" 
		   (buffer-timer-time-string timeamount) from to)))

;
; write out our data to a save file
;
(defun buffer-timer-write-results ()
  (interactive)
  (buffer-timer-write-el-results)
  (buffer-timer-write-text-results)
)

(defun buffer-timer-create-file-name ()
  (let ((newname (format-time-string buffer-timer-output-file)))
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

(let ((tstring "*idle*"))
  (if (or (equal tstring buffer-timer-idle-buffer)
			(equal tstring "idle"))
      (message "yes")
    (message "no")))

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
	       (tstring   (buffer-timer-time-string totaltime)))
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
  (switch-to-buffer buffer-timer-idle-buffer))
  
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
   (setq buffer-timer-locked lockto))

(defun buffer-timer-unlock ()
  (interactive)
  (if buffer-timer-locked
      (let ((time-locked (- (buffer-timer-current-time) 
			    buffer-timer-lock-started)))
	(buffer-timer-remember buffer-timer-locked time-locked)
	(message (format "locked to %s for %s"
			 buffer-timer-locked
			 (buffer-timer-time-string time-locked)))
	(setq buffer-timer-locked nil))
    (error "buffer-timer: can't unlock since we weren't locked")))


  

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
(global-set-key "\C-cts" 'buffer-timer-summarize)
(global-set-key "\C-ctS" 'buffer-timer-write-results)
(global-set-key "\C-cti" 'buffer-timer-go-idle)
(global-set-key "\C-ctc" 'buffer-timer-clear)
(global-set-key "\C-ctr" 'buffer-timer-report)
(global-set-key "\C-ctt" 'buffer-timer-transfer-time)
(global-set-key "\C-ctl" 'buffer-timer-lock)
(global-set-key "\C-ctu" 'buffer-timer-unlock)

;
; maybe load previous data set
;

(let ((elfile (concat (buffer-timer-create-file-name) ".el")))
  (if (and buffer-timer-load-previous (file-exists-p elfile))
      (load-file elfile)))
