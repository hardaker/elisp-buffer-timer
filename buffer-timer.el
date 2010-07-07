;; buffer-timer.el: Track your time based on the buffers you edit.
;;
;; Copyright (C) 2002-2004  Wes Hardaker <elisp@hardakers.net>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to psmith@BayNetworks.com) or
;; from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;; 02139, USA.
;;

(require 'gnus-spec)
;
; user setable variables
;
(defvar buffer-timer-running-xemacs
  (string-match "XEmacs\\|Lucid" emacs-version)
  "set to true if XEmacs is in use")

(defvar buffer-timer-idle-limit 300
  "the amount of time to wait for user input before switching to the 
buffer-timer-idle-buffer buffer")

(defvar buffer-timer-output-file    (concat (getenv "HOME") "/.buffer-timer")
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

(defvar buffer-timer-display-status-in-modeline (if buffer-timer-running-xemacs t nil)
  "Should the buffer-timer status be displayed in the modeline.")

(defvar buffer-timer-do-idle-buttons t
  "Put transfer buttons into the idle buffer for easy switch away.")

(defvar buffer-timer-frequent-topic-list nil
  "A list of frequent topics utilized a user of the buffer-timer")

(defvar buffer-timer-use-gutter (if buffer-timer-running-xemacs t nil)
  "display buffer-timer status information in the default-gutter")


(defface buffer-timer-locked-face '((((class color)
				      (background dark))
				     (:foreground "red" :bold t))
				    (((class color)
				      (background light))
				     (:foreground "red" :bold t))
				    (t
				     (:bold t)))
  "Locked face.")

(defface buffer-timer-normal-face '((((class color)
				      (background dark))
				     (:foreground "black"))
				    (((class color)
				      (background light))
				     (:foreground "black")))
  "normal face.")

(defvar buffer-timer-gutter-format "%l this: %t")

(defvar buffer-timer-recent-transfer-list-max 5
  "Maximum number of recent tranfsers to keep for buttons in the idle window")

(defvar buffer-timer-recent-buffer-max 5
  "Maximum number of recent buffers to make a button for in the idle window")

;
; internal variables
;
(defvar buffer-timer-mytime 0)
(defvar buffer-timer-gutter-format-alist
  `((?l (or buffer-timer-locked "") ?s)
    (?L (let* ((mystr (copy-sequence " "))
	       (myext (make-overlay 0 1 mystr)))
	  (if buffer-timer-running-xemacs
	      (set-extent-begin-glyph myext buffer-timer-locked-gl))
	    mystr))
    (?x buffer-timer-topic-depth-1 ?s)
    (?y buffer-timer-topic-depth-2 ?s)
    (?z buffer-timer-topic-depth-3 ?s)
    (?X buffer-timer-search-depth-1 ?s)
    (?Y buffer-timer-search-depth-2 ?s)
    (?Z buffer-timer-search-depth-3 ?s)
    (?t (buffer-timer-time-string buffer-timer-mytime) ?s)
    (?a buffer-timer-search-string-a ?s)
    (?b buffer-timer-search-string-b ?s)
    (?c buffer-timer-search-string-c ?s)
    (?d buffer-timer-search-string-d ?s)
    (?T buffer-timer-mytime ?d)))

(defvar buffer-timer-do-warnings    	  nil)
(defvar buffer-timer-locked         	  nil)
(defvar buffer-timer-debug          	  'file)
(defvar buffer-timer-debug-file   	 
  (concat (getenv "HOME") "/.buffer-timer-log"))
(defvar buffer-timer-debug-buffer   	  "*buffer-timer-log*")
(defvar buffer-timer-debug-buf            nil)
(defvar buffer-timer-last-file-name 	  nil)
(defvar buffer-timer-last-outputfile-name nil)
(defvar buffer-timer-data           	  nil)
(defvar buffer-timer-backup-data      	  nil)
(defvar buffer-timer-start-time     	  (current-time))
(defvar buffer-timer-switch-time    	  nil)
(defvar buffer-timer-switch-idle-time     nil)
(defvar buffer-timer-lock-started         nil)
(defvar buffer-timer-search-a             nil)
(defvar buffer-timer-search-string-a      "")
(defvar buffer-timer-topic-depth-1        "")
(defvar buffer-timer-topic-depth-2        "")
(defvar buffer-timer-topic-depth-3        "")
(defvar buffer-timer-search-depth-1       "")
(defvar buffer-timer-search-depth-2       "")
(defvar buffer-timer-search-depth-3       "")
(defvar buffer-timer-search-1          	  nil)
(defvar buffer-timer-search-2          	  nil)
(defvar buffer-timer-search-3          	  nil)
(defvar buffer-timer-do-depth             nil)
(defvar buffer-timer-search-int-a         0)
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

(if buffer-timer-running-xemacs
    (defvar buffer-timer-locked-gl (make-glyph (vector 'xpm :data buffer-timer-locked-xpm))))

(defvar buffer-timer-recent-transfer-list '())

;
; functions
;
(defun bt-warn (arg &rest moreargs)
  (if buffer-timer-do-warnings
      (warn arg moreargs)))
  
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
		    (set-buffer buffer-timer-debug-buf)
		    (make-local-variable 'save-buffers-skip)
		    (setq save-buffers-skip t)
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
(defun buffer-timer-get-a-time (name &optional thelist)
  (let* ((rename (if buffer-timer-rename-always
		    (buffer-timer-maybe-rename name)
		  name))
	(havelist (if thelist t))
	(thelist (if (not havelist) buffer-timer-data thelist)))
    (cdr (assoc rename thelist))))

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
(defun buffer-timer-transfer-time (from to timeamount &optional confirm)
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
  (if (or (not confirm)
	  (y-or-n-p (format "transfer %s seconds from %s to %s? " 
			       (buffer-timer-time-string timeamount) from to)))
      (progn
	(buffer-timer-remember from (- 0 timeamount))
	(buffer-timer-remember to timeamount)
	(message (format "transfered %s seconds from %s to %s" 
			 (buffer-timer-time-string timeamount) from to))
	(buffer-timer-add-to-recent-list to)
	)
    (message "transfer canceled")))

(defun buffer-timer-add-to-recent-list (item)
  "adds a string to the recent transfer list"
  (push item buffer-timer-recent-transfer-list)
  (if (> (length buffer-timer-recent-transfer-list) 
	 buffer-timer-recent-transfer-list-max)
      (pop buffer-timer-recent-transfer-list)))


(defun buffer-timer-adjust-older-time (daysago to timeamount)
  "add TIMEAMOUNT seconds to TO for DAYSAGO in time (appends .el file)"
  (interactive (list
		(read-number "Add to how many days ago: ")
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
  (setq daysago (- 0 daysago))
  (save-excursion
    (let* ((date (buffer-timer-get-days-ago daysago))
	   (filename 
	    (format-time-string (concat buffer-timer-output-file ".el")
				date))
	   buf)
	(if (file-exists-p filename)
	    (progn
	      (setq buf (find-file-noselect filename))
	      (set-buffer buf)
	      (goto-char (point-max))
	      (insert (format "(buffer-timer-adjust-time \"%s\" %d)\n"
			      to timeamount))
	      (save-buffer)
	      (message (format "added %s to %s, %d days ago in %s"
			       (buffer-timer-time-string timeamount) to 
			       daysago filename)))
	  (error (format "no file for that day: %s" filename))))))


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
(defun buffer-timer-assign-time (name time list)
  (let ((sub (assoc name list)))
    (if (listp sub)
	(setq list (buffer-timer-assign-time name time sub))))
)
(defun buffer-timer-generate-master-summary (&optional inlist)
  (interactive)
  (let ((list (or inlist buffer-timer-data))
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
    (let ((list (copy-list buffer-timer-data))
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
    (let ((time 0))
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
    (let ((list (copy-list buffer-timer-data))
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
; idle timer functions
;
(defvar buffer-timer-do-early-idle-count 0)
;(buffer-timer-do-idle-calculations)
(defun buffer-timer-do-idle-calculations ()
  (interactive)
  (progn
    (setq buffer-timer-do-early-idle-count 0)
    (if buffer-timer-do-depth
	(let ((chain (buffer-timer-find-munge-chain
		      (buffer-timer-get-current-buffer-string))))
	  (setq buffer-timer-topic-depth-1 (nth 0 chain))
	  (setq buffer-timer-topic-depth-2 (nth 1 chain))
	  (setq buffer-timer-topic-depth-3 (nth 2 chain))))
    (if buffer-timer-search-a
	(setq buffer-timer-search-string-a 
	      (buffer-timer-find-munge-string
	       buffer-timer-search-a)))
    (if buffer-timer-search-1
	(setq buffer-timer-search-depth-1
	      (buffer-timer-find-munge-string buffer-timer-topic-depth-1)))
    (if buffer-timer-search-2
	(setq buffer-timer-search-depth-2
	      (buffer-timer-find-munge-string buffer-timer-topic-depth-2)))
    (if buffer-timer-search-3
	(setq buffer-timer-search-depth-3
	      (buffer-timer-find-munge-string buffer-timer-topic-depth-3)))
    (if buffer-timer-save-when-idle
	(progn
	  (buffer-timer-write-results)))))

(defun buffer-timer-do-early-idle ()
  (interactive)
;	(message (format "saving data %d" buffer-timer-do-early-idle-count))
  (setq buffer-timer-do-early-idle-count 
	(+ buffer-timer-do-early-idle-count 1))
  (if (> buffer-timer-do-early-idle-count 
	 buffer-timer-save-every-x-idletimes)
      (buffer-timer-do-idle-calculations)))

(defun buffer-timer-do-idle-application (event)
  (interactive "e")
  (let* ((ext (event-glyph-extent event))
	 (pt (event-closest-point event))
	 to)
    (if (not ext)
	(when pt
	  (setq ext (extent-at pt (event-buffer event) nil ext 'at))))
    (if (and (overlayp ext) (overlay-get ext 'unlock))
	(buffer-timer-unlock)
      
      (if ext (setq to (overlay-get ext 'towhat)))
      (if (symbolp to) (setq to (symbol-name to)))
      (if to
	  (buffer-timer-transfer-time buffer-timer-idle-buffer to
				      (+ 300 (- (buffer-timer-current-time) 
						buffer-timer-switch-idle-time)) 
				      t)
	(call-interactively 'buffer-timer-transfer-time)))))

(defun buffer-timer-idle-message ()
  (interactive)
  (erase-buffer)
  (insert "Ok....  You've gone idle.  Do you want to:\n\n")
  (let ((here (point)) 
	(frequent buffer-timer-frequent-topic-list)
	(frequent2 buffer-timer-recent-transfer-list)
	(lastbuf (buffer-name (other-buffer)))
	(bufferlist (buffer-list))
	(count 0)
	newext)

    (if buffer-timer-locked
	(progn
	  (insert (concat "\tUnlock from " buffer-timer-locked "\n"))
	  (setq newext (make-overlay here (point)))
	  (overlay-put newext 'unlock t)
	  (buffer-timer-make-invis-button newext nil nil 
					  buffer-timer-idle-button-map
					  (concat "Unlock from" 
						  buffer-timer-locked "\n")))

      ;; not locked
      ;; generic button
      (insert "\tApply current idle time to something generic\n")
      (setq newext (make-overlay here (point)))
      (buffer-timer-make-invis-button newext nil nil 
				      buffer-timer-idle-button-map
				      "apply idle time to something else")

      ;; last visited buffers
      (insert "\nRecent buffers:\n\n")
      (while (and (< count buffer-timer-recent-buffer-max) bufferlist)
	(setq count (1+ count))
	(setq lastbuf (buffer-name (pop bufferlist)))
	(setq here (point))
	(insert (concat "\tApply current idle time to \"" lastbuf "\"\n"))
	(setq newext (make-overlay here (point)))
	(overlay-put newext 'towhat lastbuf)
	(buffer-timer-make-invis-button newext nil nil 
					buffer-timer-idle-button-map
					(concat "\tApply current idle time to \"" 
						lastbuf "\"\n")))

      ;; user specified frequent topics list
      (insert "\n\nYour frequent list:\n\n")
      (while frequent
	(while frequent
	  (let* ((thesymbol (car frequent))
		 (thestring (concat "\tApply current idle time to \"" 
				     (if (symbolp (car frequent))
					 (symbol-name (car frequent) )
				       (car frequent))
				     "\"\n")))
	    (setq here (point))
	    (insert thestring)
	    (setq newext (make-overlay here (point)))
	    (overlay-put newext 'towhat thesymbol)
	    (buffer-timer-make-invis-button newext nil nil 
					    buffer-timer-idle-button-map
					    thestring)
	    (setq frequent (cdr frequent))))
	(when frequent2
	  (setq frequent frequent2)
	  (setq frequent2 nil)
	  (insert "\n\nRecent transfers:\n\n")
	  )
	)
      (insert "\n\n(buffer-timer-idle-message)\n")
    ))
)
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
	    (bt-warn
	     "buffer-timer: idle timer gave too few seconds")
	     ;(format "buffer-timer: idle timer gave too few seconds: %d"
		;	     (- (buffer-timer-current-time)
			;	buffer-timer-switch-time)))
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
  (switch-to-buffer buffer-timer-idle-buffer)
  (if buffer-timer-do-idle-buttons
      (buffer-timer-idle-message)))

(defun buffer-timer-toggle-idle (&optional subtracttime)
  "switch to or from the idle buffer"
  (interactive)
  (if (equal (buffer-name) buffer-timer-idle-buffer)
      ; switch back to something else
      (switch-to-buffer (other-buffer))
    (buffer-timer-go-idle subtracttime)))

;
; set gutter string
;

(defvar buffer-timer-lock-map (make-sparse-keymap "buffer-timer-lock-keys")
  "keymap for gutter")
(define-key buffer-timer-lock-map [(button1)] 'buffer-timer-unlock)
(define-key buffer-timer-lock-map [(button2)] 'buffer-timer-unlock)
(define-key buffer-timer-lock-map [(button3)] 'buffer-timer-unlock)
;(setq buffer-timer-use-gutter nil)
(defvar buffer-timer-old-extent nil)

(defun buffer-timer-do-gutter-string ()
  (if buffer-timer-use-gutter
      (let* ((newname (if buffer-timer-locked
			  buffer-timer-locked
			  (buffer-timer-get-current-buffer-string)))
	     (now (buffer-timer-current-time))
;	     (buffer-timer-time-string (buffer-timer-time-string mytime))
	     (thestring
	      (copy-list
	       (eval (gnus-parse-format buffer-timer-gutter-format
					buffer-timer-gutter-format-alist))))
	     (myext (if buffer-timer-locked
			(make-overlay 0 (length buffer-timer-locked)
				     thestring)))
	     (theext (make-overlay 0 (length thestring) thestring)))
	(setq buffer-timer-mytime (+ (- now (or buffer-timer-switch-time 0))
				     (or (buffer-timer-get-a-time newname) 0)))
	(overlay-put theext 'face 'buffer-timer-normal-face)
	(if myext
	    (progn
	      (set-extent-end-glyph myext buffer-timer-locked-gl)
	      (overlay-put myext 'mouse-face buffer-timer-mouse-face)
	      (overlay-put myext 'face 'buffer-timer-locked-face)
	      (overlay-put myext 'keymap buffer-timer-lock-map)
	      ))

	; cleanup old stuff?  This isn't cleaned in garbage collection?
	(remove-gutter-element default-gutter 'buffer-timer)
	(if buffer-timer-old-extent
	    (while buffer-timer-old-extent
	      (if (overlayp (car buffer-timer-old-extent))
		  (delete-overlay (car buffer-timer-old-extent)))
	      (setq buffer-timer-old-extent (cdr buffer-timer-old-extent))))
	(setq buffer-timer-old-extent
	      (list myext theext))
	(set-gutter-element default-gutter 'buffer-timer 
			    thestring)
	)))
  
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
	  (setq buffer-timer-last-file-name newname)))
    (buffer-timer-do-gutter-string)))

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
   (buffer-timer-do-gutter-string)
   (buffer-timer-debug-msg (format "locking to %s\n" lockto))
   (buffer-timer-add-to-recent-list lockto)
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
	(buffer-timer-do-gutter-string)
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

; idle buffer map
(defvar buffer-timer-idle-button-map 
  (make-sparse-keymap "buffer-timer-idle-button-keys")
  "Keymap to apply transforms.")

(define-key buffer-timer-idle-button-map [(button2)] 
  'buffer-timer-do-idle-application)
(define-key buffer-timer-idle-button-map [(button3)] 
  'buffer-timer-do-idle-application)
(define-key buffer-timer-idle-button-map [(return)] 
  'buffer-timer-do-idle-application)

(defun buffer-timer-make-invis-button (ext &optional subregionext startinvis keymap help)
  (if startinvis
      (overlay-put subregionext 'invisible t))
  (let ((mykeymap (or keymap buffer-timer-munge-map))
	(helpstr 
	 (or help "button2 toggles visibilty of sub-groups below this one.")))
    (overlay-put ext 'end-open t)
    (overlay-put ext 'start-open t)
    (overlay-put ext 'keymap mykeymap)
    (overlay-put ext 'mouse-face buffer-timer-mouse-face)
    (overlay-put ext 'intangible t)
    (if (and subregionext (overlayp subregionext))
	(overlay-put ext 'subregion subregionext))
  ;; Help
    (overlay-put
     ext 'help-echo
     helpstr))
)

(defun buffer-timer-toggle-munge-state (event)
  "Toggle smiley at given point."
  (interactive "e")
  (let* ((ext (event-glyph-extent event))
	 (subregion (if ext (overlay-get ext 'subregion)))
	 (pt (event-closest-point event)))
    (if (not ext)
	(when pt
	  (while 
	      (and
	       (setq ext (extent-at pt (event-buffer event) nil ext 'at))
	       (not (setq subregion (overlay-get ext 'subregion)))))))
    (if subregion
	(if (not (overlay-get subregion 'invisible))
	    (overlay-put subregion 'invisible t)
	  (overlay-put subregion 'invisible nil)))))

(defun buffer-timer-copy-sequence (sequence)
  (let* ((ret (copy-list sequence))
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
      (let ((ourstart (point))
	    ;ext1 ext2
	    )
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
;		    (setq ext1 (make-overlay ourstart startpt))
;		    (setq ext2 (make-overlay startpt (1- (point))))
		    (let ((newext (make-overlay ourstart startpt))
			  (subext (make-overlay startpt (point))))
		      (buffer-timer-make-invis-button newext subext
						      (> 1 depth)))
		    ))))
	(if (and (listp (cddar sorted)) (listp (caddar sorted))
		 (integerp (car (caddar sorted))))
	    (let ((substart (point)))
	      (buffer-timer-display-munge-results (cddar sorted) 
						  (concat "  " indent)
						  (1- depth))
	      (let ((newext (make-overlay ourstart substart))
		    (subext (make-overlay substart (point))))
		(buffer-timer-make-invis-button newext subext
						(> 1 depth)))))
;	(if (and ext1 ext2)
;	    (buffer-timer-make-invis-button ext1 ext2
;					    (> 1 depth)))
	)
      (setq sorted (cdr sorted)))))

(defun buffer-timer-get-days-ago (num)
  "return (high low) representing emacs' stupid date method for NUM days ago"
  (let* ((now (current-time))
	 (low (+ (second now) (* num 60 60 24)))
	 (high (first now)))
    ;; stupid stupid time format.  Who uses 16 bit machines anymore?
    (while (< low 0)
      (setq low (+ low 65536))
      (setq high (- high 1)))
    (while (> low 65535)
      (setq low (- low 65536))
      (setq high (+ high 1)))
    (list high low)))
  
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
    (let* ((date (buffer-timer-get-days-ago daychgone)))
      ; insert a date stamp
      (insert (format-time-string "\nDate:  %Y-%m-%d  %a\n" date))
      (let ((filename 
	     (format-time-string (concat buffer-timer-output-file ".el")
				 date)))
;	(insert (format "File (%d):  %s\n" daychgone filename))
	(if (file-exists-p filename)
	    (progn
	      (load filename)
	      (buffer-timer-munge buffer-timer-data t t))
	  (insert "  No data\n")))
      (setq daychgone (1+ daychgone))))
  (kill-local-variable 'buffer-timer-data)
  (buffer-timer-munge-mode))

(defun buffer-timer-generate-munged (&optional list)
  (let* ((list (or list (copy-list buffer-timer-data)))
	 (master (buffer-timer-copy-sequence buffer-timer-regexp-master-list)))
    (while list
      (buffer-timer-add-to-master master (caar list) (cdar list) "")
      (setq list (cdr list)))
    master))
;(setq master (cdr (buffer-timer-generate-munged)))
;(cddar master)
;(caddar master)
;(car (cdddar master))
;(cadar master)n
;(cdr master)
;(car master)
;(setq search-for "perl")

;(buffer-timer-time-string (buffer-timer-find-munge-node "total" (buffer-timer-generate-munged)))
;)
; (buffer-timer-find-munge-string "total")

(defun buffer-timer-find-munge-string (search-for &optional master)
  (interactive)
  (let ((result (buffer-timer-find-munge-node search-for master)))
    (if result
	(buffer-timer-time-string result)
      "n/a")))

(defun buffer-timer-find-munge-node (search-for &optional master)
  (interactive)
  (let ((master (or master (buffer-timer-generate-munged)))
	ret)
    (while (and (not ret) master)
      (cond
       ;; exact match
       ((equal search-for (cadar master))
	(setq ret (caar master)))
       ;; top level of a sub-list
       ((and (listp (car master))
	     (integerp (caar master))
	     (not (stringp (cddar master)))
	     (listp (caddar master))
	     (integerp (car (caddar master))))
	(setq ret (buffer-timer-find-munge-node search-for (cddar master))))
       )
      (setq master (cdr master)))
    ret))

(defun buffer-timer-find-munge-chain (search-for &optional master listtot)
  (interactive)
  (let* ((master (or master (buffer-timer-generate-munged)))
	 ret)
    (while (and (not ret) master)
      (cond
       ;; exact match
       ((equal search-for (cadar master))
	(setq ret (cadar master)))
       ;; top level of a sub-list
       ((and (listp (car master))
	     (integerp (caar master))
	     (not (stringp (cddar master)))
	     (listp (caddar master))
	     (integerp (car (caddar master))))
	(setq ret (buffer-timer-find-munge-chain search-for (cddar master))))
       ((and (listp (car master))
	     (integerp (caar master))
	     (not (stringp (cddar master)))
	     (stringp (caddar master))
	     (string-match (caddar master) search-for))
	(setq ret search-for)))
      (if (not ret)
	  (setq master (cdr master))))
    (when ret
      (if (listp ret)
	  (setq ret (append (list (cadar master)) ret))
	(setq ret (list (cadar master)))
	))
    ret))


;(nth 1 (buffer-timer-find-munge-chain (buffer-timer-get-current-buffer-string)))
;(nth 1 (buffer-timer-find-munge-chain "*idle*"))
;(buffer-timer-find-munge-node (buffer-timer-get-current-buffer-string))

(defun buffer-timer-munge (&optional list nodestroy noswitch)
  (interactive)
  (if (not noswitch)
      (switch-to-buffer-other-window "*buffer-timer-results*"))
  (kill-local-variable 'buffer-timer-data)
  (if (not nodestroy)
      (erase-buffer))
  (let* ((master (buffer-timer-generate-munged list)))
    (buffer-timer-display-munge-results master "" 
					buffer-timer-munge-visible-depth))
  (buffer-timer-munge-mode))

(defun buffer-timer-munge-mode ()
  "Major mode for the munge-buffer."
  (interactive)
  (setq mode-name "Munge")
  (setq major-mode 'buffer-timer-munge-mode))

(defun buffer-timer-is-locked-p (&rest list)
  "Are we currently locked?"
  (if buffer-timer-locked
      t
    nil))

(defun buffer-timer-lockable-items (menu)
  (let ((results
	 (mapcar #'(lambda (x)
		     (vector (symbol-name x)
			     (list 'buffer-timer-lock (symbol-name x))))
		 buffer-timer-frequent-topic-list)))
	(if (not buffer-timer-locked)
	    (append menu results))))

(defun buffer-timer-do-menus ()
  "Adds menu items to the Tools menu"
  (if buffer-timer-running-xemacs
      (add-menu-button '("Tools") '("Timer"
				    ("Lock to"
				     :filter buffer-timer-lockable-items)
				    [ "unlock" buffer-timer-unlock
				      :active buffer-timer-locked ]))))

;
; note when we go idle for too long
;
(run-with-idle-timer buffer-timer-idle-limit t 
		     'buffer-timer-go-idle buffer-timer-idle-limit)

(run-with-idle-timer buffer-timer-small-idle-time t 'buffer-timer-do-early-idle)

;
(defun buffer-timer-start ()
  "turn on the buffer timer"
  (interactive)

  ;;
  ;; maybe load previous data set
  ;;
  (let ((elfile (concat (buffer-timer-create-file-name) ".el")))
    (if (and buffer-timer-load-previous (file-exists-p elfile))
	(load-file elfile)))

  ;; do this before the gutten needs to display things
  (buffer-timer-do-idle-calculations)

  (add-hook 'pre-idle-hook 'buffer-timer-idle-switch)
  (add-hook 'kill-emacs-hook 'buffer-timer-stop)

  (if buffer-timer-use-gutter
      (set-gutter-element-visible-p default-gutter-visible-p 'buffer-timer t))

  (if buffer-timer-use-gutter
      (set-specifier default-gutter-height 15))
  (buffer-timer-debug-msg "   buffer-timer-starting\n")
  (buffer-timer-do-menus))


; clean up for exiting
(defun buffer-timer-stop ()
  "exit buffer timer (turn it off)"
  (interactive)
  (if buffer-timer-locked
      (buffer-timer-unlock))
  (remove-hook 'pre-idle-hook 'buffer-timer-idle-switch)
  (buffer-timer-write-results)
  (delete-menu-item '("Tools" "Timer"))
  (buffer-timer-debug-msg "   buffer-timer-stopping\n")
  (message "buffer-timer exiting")
)

(buffer-timer-start)
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
(global-set-key "\C-ctA" 'buffer-timer-adjust-older-time)

; locking to a subject
(global-set-key "\C-cti" 'buffer-timer-go-idle)
(global-set-key "\C-ctl" 'buffer-timer-lock)
(global-set-key "\C-ctu" 'buffer-timer-unlock)
(global-set-key "\C-ctU" 'buffer-timer-do-idle-calculations)
(global-set-key "\C-ctL" 'buffer-timer-view-log)

;
;
;
;(setq ext-test (make-overlay nil nil))
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


(provide 'buffer-timer)
