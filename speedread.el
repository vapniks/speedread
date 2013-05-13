;;; speedread.el --- Aid for speedreading emacs buffers

;; Filename: speedread.el
;; Description: Aid for speedreading emacs buffers
;; Author: Bob Newell <chungkuo@chungkuo.org>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyright 2004, Bob Newell
;; Created: 2004 - Bob Newell
;; Version: 0.1
;; Last-Updated: 2013-05-12 03:05:02
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/speedread
;; Keywords: help
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: 
;;
;; Features that might be required by this library:
;;
;; 
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Read all of the documentation text below before doing anything!

;; This program is originally Copyright (C) 2004 by Bob Newell.
;; The copyright has been assigned to the Free Software Foundation.

;; The code may be used freely and without restriction by anyone,
;; but no rights of ownership are granted, conceded, or relinquished
;; by the copyright holders or the author.


;; Concept: load a (text) file into a buffer for speedreading.
;; Be sure this package is loaded just as you would load any other
;; package.  Put a 'load-library' command in your .emacs file if
;; you wish to make this easier.  Load the byte-compiled version!

;; Position the cursor to the point at which you wish to start
;; reading and give the command

;; M-x speedread

;; The file will be displayed to you in the echo area (!) a bit
;; at a time, in "flashes" with a delay between each flash.  The
;; customization variables control the minimum size (in
;; characters) of each flash group, and the pause between groups.

;; After a certain number of groups have been displayed (there is
;; a customization variable for this too) there is a 'hard' pause.
;; This is quite necessary to avoid incredible eye fatigue!  

;; At this point you can continue reading, stop, or change some
;; reading parameters.  To continue, press spacebar or the ENTER key.
;; To quit, type 'q' to quit and save your place with a bookmark,
;; or 'e' to exit speed-reading without saving your current place.
;; (Side effect: all bookmarks get saved, not just this one.  Be
;; aware.)  The commands described immediately below are also active.
;; '?' or 'h' will get you a help screen.

;; Rather than waiting for a pause between flashes, if you like,
;; you can alter the speed, the flash group size, or the number of flashes
;; on-the-fly at just about any time.  

;;; Keybindings:
;;
;;   'f' to go 20% faster,
;;   's' to go 20% slower,
;;   'w' to widen the flash group 20%,
;;   'n' to narrow it 20%,
;;   'm' for 20% more flashes between pauses,
;;   'l' for 20% less flashes between pauses,
;;   'b' to go back and repeat the current set of flashes,
;;   'r' to completely restart from whatever point in the buffer
;;       you began the session.
;;   'q' to quit and save the bookmark at point;
;;   'e' to exit without saving the bookmark.

;; When starting a speedread session, if a bookmark exists you are
;; asked if you wish to use it.  If you choose not to use it, the
;; display starts at the current cursor position.

;; Newline characters are converted to spaces.  This causes a little
;; weirdness at times but leaving newlines intact makes a big mess.

;; Again, tune the display parameters!  You may find that as your
;; speed-reading skills improve you can increase the number of
;; characters in a flash group, and/or decrease the pause time
;; between groups.

;; Project Gutenberg is a fabulous source of texts to use.

;; COMMENTS

;;  The parameters 'out of the box' tend to result in roughly 600 words
;;  per minute.  This is probably too fast for many people; adjust to suit.
;;  Don't attempt too much speed initially or you will become very
;;  frustrated and probably give up.  As you learn how to work with
;;  the technique you can build up the speed and the flash group size.
;;  On the other hand, push yourself a little.  Go as fast as you can
;;  without losing comprehension.  Different types of reading material
;;  will require different speed settings!  You can read a scifi novel
;;  faster than you can read existential philosophy.

;;  Your comments on both speed-reading, the flash technique, and the
;;  program itself are welcome.  Write

;;  chungkuo@chungkuo.org

;;  I also have a Perl version of similar (older, less functional) code.  
;;  If you want it let me know.

;; 
;;;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `speedread'
;;    Speedread a buffer by timed flashing of groups of words in the echo area
;;  `speedread-faster'
;;    Increase reading speed temporarily
;;  `speedread-narrower'
;;    Narrow flash-group temporarily
;;  `speedread-help'
;;    Get speedreading command help
;;  `speedread-save-changes'
;;    save all customization variables changed this Emacs session
;;


;;; Installation:
;;
;; Put speedread.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'speedread)

;; For performance reasons it's recommended that you byte-compile the file.
;; This can be achieved using the byte-compile-file command.
;; When compiling ignore any warnings; hopefully I've found most
;; of these by now in any case.

;; You should also fine-tune the customization variables to optimize performance.
;; These are shown below.

;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `speedread-chars'
;;    Minimum characters per flash group (an integer)
;;    default = 20
;;  `speedread-delay-milliseconds'
;;    Milliseconds of delay between flashes (an integer)
;;    default = 300
;;  `speedread-end-sentence-delay-milliseconds'
;;    Pause between sentences in milliseconds (an integer)
;;    default = 500
;;  `speedread-final-delay-milliseconds'
;;    Milliseconds to wait before exiting after reaching end of buffer
;;    default = 2000
;;  `speedread-top-window-size'
;;    How much of the frame to use for displaying the original buffer whilst speedreading.
;;    default = 5
;;  `speedread-font-size-scale-factor'
;;    Scale factor for size of font
;;    default = 2.0
;;  `speedread-text-justification'
;;    Justification of text in speedread buffer.
;;    default = (quote center)
;;  `speedread-end-sentence-regexp'
;;    Regular expression to match with end of sentence words.
;;    default = "\\(\\.\\|\\:\\)\\(\n\\|>\\|)\\|'\\|\"\\)*$"

;; All of the above can customized by:
;;      M-x customize-group RET speedread RET
;;

;;; Change log:
;;
;; 12/05/2013
;;      * Tidy up code and documentation - Joe Bloggs
;;
;;
;; 22/09/2008
;;      * Added the following commands: `speedread-toggle-pause'
;;        `speedread-next-line' `speedread-previous-line' `speedread-get-previous-flash-line'
;;        `speedread-get-previous-flash-line' - Joe Bloggs
;;      * Change variable names to begin with speedread instead of iread - Joe Bloggs
;;
;; 13/10/2004
;;      * Fixed severe bug with % character in flash group, can't
;;        imagine this wasn't found earlier.  Changed 'minibuffer'
;;        terminology to more accurate 'echo area'.  Strange echo
;;        bug reappeared; hope the % fix killed it but not certain.
;;        Alpha 0.22.
;; 03/09/2004
;;      * Remove spaces at front of a flash group.  
;;        Seemingly fixed strange minibuffer echo bug; still not so
;;        sure though.  
;;        Fixed most if not all free-variable compiler complaints.
;;        Moved speedread customization group out of local group,
;;        making an incompatibility with prior releases.
;;        Changed bookmarking to use bookmark-buffer-file-name,
;;        also incompatible with previous releases.
;;        Attempted to fix leading punctuation problem by changing
;;        forward-word method to search-forward-regexp.  It's not a
;;        complete solution but seems to help a lot.
;;        Changed to fixed delay when end of buffer is reached.
;;         Alpha 0.21 (release version)
;; 01/09/2004
;;      * Incorporated additional ideas from Joakim to
;;        allow command entry at any time, not just at
;;        major pauses.  Required extensive changes to the
;;        previous alpha.  Added comments and improved
;;        legibility.  Still some bugs.
;;        Alpha 0.20 (not for public release yet)
;; 12/08/2004
;;      * Major rev to include commands to change params
;;        temporarily on the fly, and all the supporting
;;        code to go with it, plus doc revs, help screen,
;;        etc etc.  Thanks to Joakim Verona for numerous
;;        good ideas for improved functionality.
;;        This is close to a rewrite.
;;        Added speedread-save-changes to save any altered
;;        customization variables.
;;        Alpha 0.10 (not for public release yet)
;; 06/08/2004
;;      * Added summary statistics per user input.
;;        Alpha 0.03
;; 30/07/2004
;;      * Changed, reordered, added to documentation.
;;        Fixed a few > col 80 line wraps in source code.
;;        Minor prompt change.
;;        Alpha 0.02
;; 29/072004
;;      * Santa Fe, New Mexico.  Initial release.
;;        Alpha 0.01


;;; Acknowledgements:
;;
;; Bob Newell - original author
;;

;;; TODO
;;
;;  * Maybe bookmark to a 'speedread' file rather than default file?
;;

;;; UNRESOLVED KNOWN BUGS/ISSUES

;;  BUGS
;;  * The bookmark is not found unless the bookmark file has been
;;    preloaded.
;;  * In X-windows, moving focus out of the window during flashing
;;    causes problems.

;;  ISSUES/COMMENTS
;;  * There is no symmetry in increasing/decreasing rates, counts,
;;    etc.  If you increase speed 20% and then decrease speed 20%
;;    you end up at 96% of original speed, for example.
;;  * Temporary changes are not saved and can be hard to reproduce.
;;  * Pause time when waiting for command/continuation input is not
;;    counted against the reading time.  I don't think it should be,
;;    though.

;;; Require


;;; Code:

;; Make sure bookmarks are loaded when compiling file.
(eval-when-compile (bookmark-load bookmark-default-file))

(defgroup speedread nil
  "Speed reading customization variables")

(defcustom speedread-chars 20
  "Minimum characters per flash group (an integer)"
  :group 'speedread)

(defcustom speedread-delay-milliseconds 300
  "Milliseconds of delay between flashes (an integer)"
  :group 'speedread)

(defcustom speedread-end-sentence-delay-milliseconds 500
  "Pause between sentences in milliseconds (an integer)"
  :group 'speedread)

(defcustom speedread-final-delay-milliseconds 2000
  "Milliseconds to wait before exiting after reaching end of buffer
 (an integer)"
  :group 'speedread)

(defcustom speedread-top-window-size 5
  "How much of the frame to use for displaying the original buffer whilst speedreading.
Can be an integer greater than 0 or any number between 0 and 1.
An integer greater than 0 indicates the number of lines to use.
Whereas a number between 0 and 1 indicates the fraction of the frame to use."
  :group 'speedread)

(defcustom speedread-font-size-scale-factor 2.0
  "Scale factor for size of font"
  :group 'speedread)

(defcustom speedread-text-justification 'center
  "Justification of text in speedread buffer.
Can be: left, right, full, center or none.")

(defcustom speedread-end-sentence-regexp "\\(\\.\\|\\:\\)\\(\n\\|\>\\|)\\|'\\|\"\\)*$"
  "Regular expression to match with end of sentence words."
  :group 'speedread)

;; MAIN COMMAND
(defun speedread nil
  "Speedread a buffer by timed flashing of groups of words in the echo area"
  (interactive)
  ;; stuff to avoid at least some of the free variable complaints
  (defvar speedread-average)
  (defvar speedread-bookmark)
  (defvar speedread-chars)
  (defvar speedread-continue)
  (defvar speedread-count)
  (defvar speedread-delay-milliseconds)
  (defvar speedread-flash-length)
  (defvar speedread-flash-line)
  (defvar speedread-group-start)
  (defvar speedread-mark)
  (defvar speedread-start-start)
  (defvar speedread-time-now)
  (defvar speedread-time-used)
  (defvar speedread-words-read)
  (defvar speedread-words-recent)
  (defvar speedread-buffer-name)
  (defvar speedread-buffer-raise-value)
  ;; vector definition for asynchronous command processing
  (defvar speedread-command-vector nil)
  (setq  speedread-command-vector (make-vector 256 'speedread-toggle-pause))
  (aset  speedread-command-vector ?q 'speedread-quit)
  (aset  speedread-command-vector ?e 'speedread-exit)
  (aset  speedread-command-vector ?f 'speedread-faster)
  (aset  speedread-command-vector ?s 'speedread-slower)
  (aset  speedread-command-vector ?w 'speedread-wider)
  (aset  speedread-command-vector ?n 'speedread-narrower)
  (aset  speedread-command-vector ?m 'speedread-more)
  (aset  speedread-command-vector ?l 'speedread-less)
  (aset  speedread-command-vector ?b 'speedread-back)
  (aset  speedread-command-vector ?r 'speedread-restart)
  (aset  speedread-command-vector ?h 'speedread-help)
  (aset  speedread-command-vector ?? 'speedread-help)
  (aset  speedread-command-vector 254 'speedread-get-next-flash-line)
  (aset  speedread-command-vector 255 'speedread-get-previous-flash-line)  
  (aset  speedread-command-vector 252 'speedread-next-line)  
  (aset  speedread-command-vector 253 'speedread-previous-line)  
  ;; look for a bookmark and optionally go there
  ;; if we don't go there start at point
  (if (and bookmark-current-bookmark
           (setq speedread-bookmark (bookmark-get-position bookmark-current-bookmark)))
      (if (yes-or-no-p "Bookmark exists, use it? ")
	  (goto-char speedread-bookmark)))
  ;; initialize
  (setq speedread-count 0)
  (setq speedread-flash-line nil)
  (setq speedread-flash-length 0)
  (setq speedread-mark (point))
  (setq speedread-group-start (point))
  (setq speedread-start-start (point))
  (setq speedread-time-now (float-time))
  (setq speedread-time-used 0)
  (setq speedread-words-read 0)
  (setq speedread-words-recent 0)
  (setq speedread-continue t)
  ;; setup speedread buffer
  (setq speedread-buffer-name (concat "*Speedread - " (buffer-name) "*"))
  (switch-to-buffer-other-window speedread-buffer-name)
  (setq cursor-type nil)
  ;; size buffer windows correctly
  (let ((enlarge-window-amount 
	 (- (frame-height) 5 (window-height) (if (integerp speedread-top-window-size) speedread-top-window-size (round (* speedread-top-window-size (- (frame-height) 5)))))))
    (enlarge-window enlarge-window-amount))
  (setq speedread-buffer-raise-value (- (/ (window-height) (* 2 speedread-font-size-scale-factor))))
  ;; go back to original buffer window and turn of cursor blinking
  (other-window 1)
  (if blink-cursor-mode (blink-cursor-mode))
  (catch 'iread
    ;; get next flash line (unless we are at the end of the buffer)
    (while (setq speedread-results (speedread-get-next-flash-line)
		 speedread-words-read (+ speedread-words-read (nth 1 speedread-results))
		 speedread-end-sentence (nth 2 speedread-results)
		 ;; this must be last setq, to indicate when we are at end of buffer
		 speedread-flash-line (car speedread-results))
      ;; after flashing group check and process pending input, ONE
      ;; command letter only - otherwise multiple or repeat keypresses
      ;; could create havoc
      (if (input-pending-p)
	  (let ((keyinput (read-event)))
	    (if (not (integerp keyinput))
		(if (equal keyinput 'right) (setq keyinput 254)
		  (if (equal keyinput 'left) (setq keyinput 255)
		    (if (equal keyinput 'up) (setq keyinput 253)
		      (if (equal keyinput 'down) (setq keyinput 252))))))
	    (funcall (aref speedread-command-vector keyinput))))
      ;; flush anything extra beyond a single char
      (while (input-pending-p) (read-event))
      ;; then do the between-flash pause
      (if speedread-end-sentence 
	  (sleep-for 0 speedread-end-sentence-delay-milliseconds)
	(sleep-for 0 speedread-delay-milliseconds))
      (setq speedread-flash-line nil)
      (setq speedread-flash-length 0)
      (setq speedread-count (1+ speedread-count)))
    ;; We have reached the end of the file.  
    ;; We still have a little bit undisplayed, so show it and then we're done.
    (setq speedread-flash-line (buffer-substring speedread-mark (point-max)))   
    (setq speedread-flash-line
	  (subst-char-in-string (string-to-char "\n")
				(string-to-char " ") speedread-flash-line))
    ;; trim whitespace from start and end of speedread-flash-line 
    (setq speedread-flash-line (replace-regexp-in-string "^\\s-*\\|\\s-*$" "" speedread-flash-line))
    ;; print final flash line and message to indicate end of buffer
    (with-current-buffer speedread-buffer-name 
      (erase-buffer)
      ;; print the flash line to the speedread buffer
      (princ speedread-flash-line (get-buffer speedread-buffer-name))
      (add-text-properties
       ;; set size and position, and other properties of text
       1 (+ (length speedread-flash-line) 1)
       (list 'face (list :foreground "white" :background "black" :height speedread-font-size-scale-factor)
             'display (list 'raise speedread-buffer-raise-value))
       (get-buffer speedread-buffer-name))
      (justify-current-line speedread-text-justification)
      (princ "\n\n\n(END OF BUFFER)" (get-buffer speedread-buffer-name))
      (add-text-properties
       ;; set size and position, and other properties of text
       (- (point-max) 15) (point-max)
       (list 'face (list :foreground "white" :background "black" :height 1.0))
       (get-buffer speedread-buffer-name))
      (justify-current-line 'speedread-text-justification))
    ;; update window and redisplay
    (force-window-update (get-buffer speedread-buffer-name))
    (redisplay)
    ;; Show closeout stats.
    (setq speedread-time-used
	  (+ speedread-time-used (/ (- (float-time) speedread-time-now) 60) ))
    (setq speedread-average (/ speedread-words-read speedread-time-used))
    (message (format "%d words read in %6.2f minutes; %6.1f words per minute."
		     speedread-words-read speedread-time-used speedread-average))
    ;; pause then exit
    (sleep-for 0 speedread-final-delay-milliseconds)
    (speedread-exit)))
;; END OF SPEEDREAD COMMAND

;; the throws work here even though I thought they should not !?

(defun speedread-quit nil
  "quit and save bookmark"
  (if (not blink-cursor-mode) (blink-cursor-mode))
  (bookmark-set (bookmark-buffer-file-name))
  (bookmark-save)
  (kill-buffer speedread-buffer-name)
  (delete-other-windows)
  (throw 'iread t))

(defun speedread-exit nil
  "quit and don't save bookmark"
  (if (not blink-cursor-mode) (blink-cursor-mode))
  (kill-buffer speedread-buffer-name)
  (delete-other-windows)
  (throw 'iread t))

(defun speedread-do-continue nil
  "continue reading now"
  (setq speedread-continue t))

(defun speedread-faster nil
  "Increase reading speed temporarily"
  (interactive)
  (setq speedread-delay-milliseconds
        (truncate (* 0.8 speedread-delay-milliseconds))))

(defun speedread-slower nil
  "Decrease reading speed temporarily"
  (setq speedread-delay-milliseconds
        (truncate (* 1.2 speedread-delay-milliseconds))))

(defun speedread-wider nil
  "Widen flash group temporarily"
  (setq speedread-chars
        (truncate (* 1.2 (float speedread-chars)))))

(defun speedread-narrower nil
  "Narrow flash-group temporarily"
  (interactive)
  (setq speedread-chars
        (truncate (* 0.8 (float speedread-chars)))))

(defun speedread-back nil
  "Re-read last full set of flash groups"
  (goto-char speedread-group-start)
  (setq speedread-words-read (- speedread-words-read speedread-words-recent))
  (setq speedread-words-recent 0)
  (setq speedread-mark (point))
  (setq speedread-continue t))

(defun speedread-restart nil
  "Restart from session start"
  (goto-char speedread-start-start)
  (setq speedread-mark (point))
  ;; kill the words read if we're starting over
  (setq speedread-words-read 0)
  (setq speedread-continue t))

(defun speedread-help nil
  "Get speedreading command help"
  (interactive)
  (defvar speedread-saved-buffername)
  (if (get-buffer "*iread.help*")
      (kill-buffer "*iread.help*"))
  (setq speedread-saved-buffername (buffer-name))
  (switch-to-buffer "*iread.help*")
  (goto-char (point-min))
  (insert "Command keys:\n\n")
  (insert " f   read faster by 20%\n")
  (insert " s   read slower by 20%\n")
  (insert " w   widen flash group by 20%\n")
  (insert " n   narrow flash group by 20%\n")
  (insert " m   read 20% more groups between pauses\n")
  (insert " l   read 20% less groups between pauses\n")
  (insert " b   go back and reread from the last pause point\n")
  (insert " r   restart from session starting point\n")
  (insert " e   exit without bookmarking\n")
  (insert " q   quit and bookmark current location\n")
  (insert " h,? get command help")
  (insert " c, spacebar   continue reading\n")
  (insert " arrow keys  navigate buffer\n")
  (insert "\nAll changes except bookmarks are retained during the current\n")
  (insert "Emacs session only (but bookmarks are permanent).\n")
  (insert "If you wish to retain the other changes permanently,\n")
  (insert "use the command 'speedread-save-changes'.")
  (read-from-minibuffer "Press ENTER to leave help screen")     
  (switch-to-buffer speedread-saved-buffername))

(defun speedread-save-changes nil
  "save all customization variables changed this Emacs session"
  (interactive)
  (if (yes-or-no-p "REALLY overwrite all saved speedread settings? ")
      (progn
	(customize-save-variable 'speedread-chars speedread-chars)
	(customize-save-variable 'speedread-delay-milliseconds speedread-delay-milliseconds)
	(customize-save-variable 'speedread-font-size-scale-factor speedread-font-size-scale-factor)
	(customize-save-variable 'speedread-number-display-lines-in-top-window speedread-number-display-lines-in-top-window)
	(customize-save-variable 'speedread-final-delay-milliseconds 'speedread-final-delay-milliseconds)
	(customize-save-variable 'speedread-end-sentence-delay-milliseconds 'speedread-end-sentence-delay-milliseconds))))

(defun speedread-toggle-pause nil
  "toggle pausing of speedread"
  (setq speedread-time-used (+ speedread-time-used
			       (/ (- (float-time) speedread-time-now) 60)))
  (setq speedread-time-now (float-time))
  (setq speedread-continue (not speedread-continue))
  (while  (not speedread-continue)
    ;; show stats and prompt, and also process command input
    (setq speedread-average (/ speedread-words-read speedread-time-used ))
    (message
     (format
      "%d msec delay %d chr/flash %6.1f wd/min. Command/?/ENTER:"
      speedread-delay-milliseconds speedread-chars speedread-average))
    ;; wait unconditionally for input with 1/10 second polling
    (while (not (input-pending-p))
      (sleep-for 0 100))
    ;; try to avoid timer problem on quit/exit commands
    (setq speedread-time-now (float-time))
    ;; read key input and call appropriate function
    (let ((keyinput (read-event)))
      (if (not (integerp keyinput))
	  (if (equal keyinput 'right) (setq keyinput 254)
	    (if (equal keyinput 'left) (setq keyinput 255)
	      (if (equal keyinput 'up) (setq keyinput 253)
		(if (equal keyinput 'down) (setq keyinput 252))))))
      (funcall (aref speedread-command-vector keyinput)))
    ;; flush extra input
    (while (input-pending-p) (read-event))))

(defun speedread-next-line nil
  "move to next line"
  (next-line)
  (redisplay))

(defun speedread-previous-line nil
  "move to previous line"
  (previous-line)
  (redisplay))

(defun speedread-get-next-flash-line nil
  "Get next line of flash text. from buffer starting from point.
Returns list containing flash line and number of words read."
  (setq speedread-mark (point))
  (let ((speedread-words-recent 0)
	(speedread-flash-line nil)
	(speedread-end-sentence nil)
	speedread-current-word)
    (while (and (< (length speedread-flash-line) speedread-chars) 
		(not speedread-end-sentence)
		(search-forward-regexp "\\(\\s-\\|\n\\)+" nil t))
      (setq speedread-current-word 
	    (replace-regexp-in-string "^\\(\\s-\\|\n\\)+\\|\\(\\s-\\|\n\\)+$" "" (buffer-substring speedread-mark (point))))
      (setq speedread-words-recent (1+ speedread-words-recent))
      (setq speedread-flash-line 
	    (if (equal speedread-words-recent 1) speedread-current-word 
	      (concat speedread-flash-line " " speedread-current-word)))
      (setq speedread-end-sentence 
	    (if (< (length speedread-current-word) 1) nil
	      (string-match speedread-end-sentence-regexp speedread-current-word)))
      (setq speedread-mark (point)))
    ;; remove surrounding whitespace and newlines
    (setq speedread-flash-line 
	  (replace-regexp-in-string "^\\(\\s-\\|\n\\)+\\|\\(\\s-\\|\n\\)+$" "" speedread-flash-line))
    ;; print flash line to speedread buffer
    (with-current-buffer speedread-buffer-name 
      (erase-buffer)
      (princ speedread-flash-line (get-buffer speedread-buffer-name))
      (add-text-properties
       ;; set size, position, and other properties of text
       1 (point-max);;(+ (length speedread-flash-line) 1)
       (list 'face (list :foreground "white" :background "black" :height speedread-font-size-scale-factor) 'display (list 'raise speedread-buffer-raise-value))
       (get-buffer speedread-buffer-name))
      (justify-current-line speedread-text-justification nil t))
    ;; update display
    (redisplay)
    ;; return flash line, number of words read, 
    ;; and indicate if we have reached the end of a sentence
    (list speedread-flash-line speedread-words-recent speedread-end-sentence)))

(defun speedread-get-previous-flash-line nil
  "get previous line of flash text from buffer starting from point.
Returns list containing flash line and number of words read."
  (setq speedread-mark (point))
  (let ((speedread-words-recent 0)
	(speedread-flash-line nil)
	(speedread-end-sentence nil)
	speedread-current-word)
    ;; don't break on \r or \n, only real spaces or tabs!
    (while (and (< (length speedread-flash-line) speedread-chars) 
		(not speedread-end-sentence)
		(search-backward-regexp "\\(\\s-\\|\n\\)+" nil t))
      (setq speedread-current-word 
	    (replace-regexp-in-string "^\\(\\s-\\|\n\\)+\\|\\(\\s-\\|\n\\)+$" "" (buffer-substring speedread-mark (point))))
      (setq speedread-words-recent (1+ speedread-words-recent))
      (setq speedread-flash-line 
	    (if (equal speedread-words-recent 1) speedread-current-word 
	      (concat speedread-flash-line " " speedread-current-word)))
      (setq speedread-end-sentence 
	    (if (< (length speedread-current-word) 1) nil
	      (equal (substring speedread-current-word (- (length speedread-current-word) 1)) ".")))
      (setq speedread-mark (point)))
    ;; remove surrounding whitespace and newlines
    (setq speedread-flash-line 
	  (replace-regexp-in-string "^\\(\\s-\\|\n\\)+\\|\\(\\s-\\|\n\\)+$" "" speedread-flash-line))
    ;; print flash line to speedread buffer
    (with-current-buffer speedread-buffer-name 
      (erase-buffer)
      (princ speedread-flash-line (get-buffer speedread-buffer-name))
      (add-text-properties
       ;; set size, position, and other properties of text
       1 (point-max) ;;(+ (length speedread-flash-line) 1)
       (list 'face (list :foreground "white" :background "black" :height speedread-font-size-scale-factor) 'display (list 'raise speedread-buffer-raise-value))
       (get-buffer speedread-buffer-name))
      (justify-current-line speedread-text-justification nil t))
    ;; update display
    (redisplay)
    ;; return flash line and number of words read
    (list speedread-flash-line speedread-words-recent)))

(provide 'speedread)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "speedread.el" (buffer-name) (buffer-string) "update")

;;; speedread.el ends here

