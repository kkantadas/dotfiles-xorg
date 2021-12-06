 (in-package :stumpwm)
 (define-key *root-map* (kbd "d") "exchange-direction")

 (set-module-dir "~/.stumpwm.d/modules")
 (load-module "command-history")
 (load-module "amixer")
 (setf *startup-message* nil
      *input-window-gravity* :center
      *time-format-string-default* "%Y/%m/%d(%a) %H:%M"
      *message-window-timer* 4
      *message-window-gravity* :top-right
      *maxsize-border-width* 1
      *transient-border-width* 1
      *normal-border-width* 1
      *window-border-style* :thin)

 ;;set terminal
  (define-key *root-map* (kbd "c")
   "exec urxvt -name Terminal")

;; Transperrent windows
 (defun hide-all-lower-windows (current last)
      (declare (ignore current last))
      (when (typep (current-group) 'stumpwm::tile-group)
        (mapc (lambda (win)
                (unless (eq win (stumpwm::frame-window
                                 (stumpwm::window-frame win)))
                  (stumpwm::hide-window win)))
              (group-windows (current-group)))))

    (defcommand enable-hiding-lower-windows () ()
  (add-hook *focus-window-hook* 'hide-all-lower-windows))
 (enable-hiding-lower-windows)


;; Message window font (change by run  xlsfonts
;;(set-font "-xos4-terminus-bold-r-normal--18-180-72-72-c-100-iso10646-1")
 (set-font "-xos4-terminus-bold-r-normal--16-160-72-72-c-80-iso10646-1")


(stumpwm:define-key stumpwm:*root-map* (stumpwm:kbd "X") "exec urxvt -e ranger")

;; window config
(set-focus-color 'Black)
(set-unfocus-color 'Black)
(set-normal-gravity :center)
(set-border-color "#363535")
;;(set-border-color "#707070")
;;(set-bg-color "#8a8a8a")
(set-bg-color 'Black)
;;(set-bg-color "#312812")
(set-fg-color 'gray)
;;(set-fg-color 'Black)

(define-key *top-map* (kbd "s-s") "vsplit 4/5")
;;(define-key *top-map* (kbd "s-x") "exec  urxvt -e ranger")
(define-key *top-map* (kbd "M-a") "mode-line")
(define-key *top-map* (kbd "s-d") "exec Dmenu2")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "amixer-Front-1-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "amixer-Front-1+")

(defcommand long-frame () ()
  (run-commands "restore .stumpwm.d/long.le" "pull-hidden-next"))

;;(define-key *root-map* (kbd "j") "restart-hard")

;;Set the mouse policy to focus follows mouse;
;;(setf *mouse-focus-policy* :click) ;; :click, :ignore, :sloppy
;;??
(define-key *root-map* (kbd "Y")      "move-window down")
(define-key *root-map* (kbd "C")      "run-shell-command")

;; workspaces
(stumpwm:grename "Web")
(stumpwm:gnewbg "Files")
(stumpwm:gnewbg "Edit")
;;(stumpwm:gnewbg-float "float")
;; worspaces switch
;;(define-key *top-map* (kbd "M-F1")      "gselect 1")
(define-key *top-map* (kbd "M-F1")      "gselect 1")
(define-key *top-map* (kbd "M-F2")      "gselect 2")
(define-key *top-map* (kbd "M-F3")      "gselect 3")

(load-module "winner-mode")
(load-module "amixer")

(defvar *winner-map* (make-sparse-keymap))

(define-key *root-map* (kbd "u") '*winner-map*)

;;(define-key *root-map* (kbd "U") "undo-frame-back")
(define-key *winner-map* (kbd "U") "undo-frame-back")

(define-key *winner-map* (kbd "u") "winner-undo")
(define-key *winner-map* (kbd "p") "winner-redo")

(add-hook *post-command-hook* (lambda (command)
                               (when (member command winner-mode:*default-commands*)
                                  (winner-mode:dump-group-to-file))))

;;(load-module "winner-mode")
(setf winner-mode:*tmp-folder* (merge-pathnames #p".stumpwm.d/tmp/" (user-homedir-pathname)))

(define-key *root-map* (kbd "C-Up") "exchange-direction up")
(define-key *root-map* (kbd "C-Down") "exchange-direction down")
(define-key *root-map* (kbd "C-Left") "exchange-direction left")
(define-key *root-map* (kbd "C-Right") "exchange-direction right")

(define-key *root-map* (kbd "C-k") "exchange-direction up")
(define-key *root-map* (kbd "C-j") "exchange-direction down")
(define-key *root-map* (kbd "C-h") "exchange-direction left")
(define-key *root-map* (kbd "C-l") "exchange-direction right")

(define-key *root-map* (kbd "k") "move-focus up")
(define-key *root-map* (kbd "j") "move-focus down")
(define-key *root-map* (kbd "h") "move-focus left")
(define-key *root-map* (kbd "l") "move-focus right")


(define-key *root-map* (kbd "s-k") "describe-key")
(define-key *root-map* (kbd "s-c") "describe-command")
(define-key *root-map* (kbd "s-f") "describe-function")
(define-key *root-map* (kbd "s-v") "describe-variable")

(define-key *root-map* (kbd "|") "balance-frames")
(define-key *root-map* (kbd "s-v") "describe-variable")
(define-key *root-map* (kbd "s-w") "where-is")

(define-key *root-map* (kbd "C-K") "delete-window")
(define-key *root-map* (kbd "K") "kill-close")
;;split and move down
(define-key *root-map* (kbd "s") "move-after-vsplit")
;;split and move right
(define-key *root-map* (kbd "S") "move-after-hsplit")

;;exchange windows
(define-key *root-map* (kbd "T") "rotate-windows")


;; extra keybinding with second prefix like key
(defun alist-define-keys (map alist)
  "define key using alist."
  (loop for (key . command) in alist
        do (define-key map (kbd key) command)))
(defmacro create-map (var key &key (on *top-map*))
  `(progn
     (defparameter ,var (make-sparse-keymap))
   (define-key ,on (kbd ,key) ',var)
     ,var))
(alist-define-keys (create-map *window-map* "s-x")
                   '(("x" . "run-ranger")
                     ("X" . "exec urxvt -e ranger")
                     ("d" . "exec") 
                     ("h" . "htop")
                     ("m" . "mutt")
                     ("j" . "alsamix")
                     ("a" . "exec rat-line.sh")
                     ("f" . "file-frame")
                     ("1" . "long-frame")
                     ("2" . "wide-frame")
                     ("8" . "exec  transset-df 0.8")
                     ("9" . "exec  transset-df 0.9")
                     ("0" . "exec  transset-df 1")
                     ("P" . "go-net")
                     ("p" . "ping-test")
                     ("f" . "run-or-raise-pcmanfm")
                     ("w" . "run-or-raise-chromium")
                     ("S" . "stumpish-terminal")
                     ("c" . "Network-dmenu")
                     ("t" . "rxvt")
                     ("u" . "undo-split")
                     ("L" . "float-term")
                     ("Q" . "quit")
                     ("C" . "term-white")
                     ("r" . "redshift-kill")
                     ("R" . "redshift-run")
                     ("z" . "loadrc")
                     ("v" . "Vedatxt")
                     ("g" . "google")
                     ("i" . "info-stump")
                     ("e" . "Emacs")
                     ("s" . "split-down")))

(defcommand move-after-vsplit () ()
  (run-commands "vsplit" "move-focus down"))

(defcommand move-after-hsplit () ()
  (run-commands "hsplit" "move-focus right"))

(defcommand test-term () ()
  (run-commands "long-frame" "exec xterm" ))

(defcommand float-term () ()
  (run-commands "gnew-float xx" "exec xterm"))

(defcommand split-down () ()
  "Open windows down"
  (run-commands "vsplit 5/6" "move-focus down"))

(defcommand kill-close () ()
  "kill windows and close space "
  (run-commands "delete-window")
  (run-commands "exec sleep 0.02s && stumpish remove"))


(defcommand long-frame () ()
  (run-commands "dump-group-to-file .stumpwm.d/tmp.le" "restore-from-file .stumpwm.d/lang.le" ))

;;;(defcommand wide-frame () ()
  ;;;;(run-commands "restore-from-file .stumpwm.d/wide.le" ))

(defcommand info-stump () ()
  (run-commands "exec urxvt -e less .stumpwm.d/stumpInfo.txt"))

(defcommand wide-frame () ()
  (run-commands "dump-group-to-file .stumpwm.d/tmp.le" "restore-from-file .stumpwm.d/wide.le" ))

(defcommand undo-frame-back () ()
  (run-commands "restore-from-file .stumpwm.d/tmp.le" ))

(defcommand redshift-kill () ()
  (run-commands "exec killall redshift" "echo S T O P  R E D S H I F T "))

(defcommand redshift-run () ()
  (run-commands "exec redshift" "echo R U N  R E D S H I F T "))

(defcommand go-net () ()
  (run-commands "exec stump-ping.sh"  "echo Checking Ethernet .........."))

(defcommand go-connman () ()
  (run-commands "exec con " "echo Checking Ethernet .........."  "exec rat-ping.sh"))

(defcommand file-frame () ()
  (run-commands "dump-group-to-file .stumpwm.d/tmp.le" "restore-from-file .stumpwm.d/ping.le" )
  (run-commands "exec pcmanfm &&  exec stumpish restore-from-file .stumpwm.d/tmp.le"))

(defcommand ping-test () ()
  (run-commands "dump-group-to-file .stumpwm.d/tmp.le" "exec sleep .08s && exec stumpish restore-from-file .stumpwm.d/ping.le" )
  (run-commands "exec urxvt -e ping -c4 google.com && exec stumpish restore-from-file .stumpwm.d/tmp.le"))


(defcommand alsamix () ()
  (run-commands "dump-group-to-file .stumpwm.d/tmp.le" "exec sleep .28s && exec stumpish restore-from-file .stumpwm.d/wide.le" )
  (run-commands "exec urxvt -e alsamixer && exec stumpish restore-from-file .stumpwm.d/tmp.le"))


;;(defcommand ping-frame () ()
;;   (run-commands  "restore-from-file .stumpwm.d/ping.le" "exec urxvt -e ping -c4 google.com"  ))

(defcommand weather () ()
  (run-commands "exec Weather"))

(defcommand term-white () ()
  (run-commands "exec urxvt-white.sh"))

(defcommand test-&&() ()
(run-commands "exec xterm" :wait nil)
(run-commands "exec urxvt -e ranger" :wait nil))

;;(defcommand Network-dmenu () ()
;;      (run-commands "exec networkmanager_dmenu -sf yellow -sb '#7B5656' -nb '#391D1D' -fn xft:monofur:size=9:bold"))

(defcommand stumpish-terminal () ()
  (run-commands  "exec urxvt -e ~/.stumpwm.d/stumpish" ))

;;(defcommand hsplit-save () ()
;;  (run-commands "dump-desktop-to-file split-dump" "hsplit" ))

;;(defcommand undo-split () ()
;;  (run-commands "restore-from-file split-dump" ))

(defcommand restart-x () ()
  (run-commands "restart-hard" ))


;;(defcommand long-frame2 () ()
  ;;(run-commands "only" "hsplit 5/7" "hsplit 2/6" "exchange-direction right" "move-focus right" "fclear" "move-focus left" "move-focus left" "fclear" "move-focus right" ))
;;mode-Line
;(load-module "battery-portable")
(load-module "hostname")
;(load-module "wifi")
;(load-module "net")
(load-module "disk")
;(load-module "mem")
(load-module "cpu")

(setf *colors*
      '("black"
       "red"
       "green"
       "yellow"
       "blue"
       "magenta"
       "cyan"
       "white"
       "GreenYellow"
       "#009696"))
(update-color-map (current-screen))

;;(setf *mode-line-timeout* 5)
(setf *mode-line-border-width* 0)
(setf *mode-line-background-color* "#000809")
(setf *mode-line-foreground-color* "DeepSkyBlue")
;;(setf stumpwm:*screen-mode-line-format*
(setf *window-format* "%m%n%s%c")
(setf *mode-line-timeout* 0.7)


(defun get-volume ()
 (string-trim
 (string #\newline)
 (run-shell-command "amixer sget Master | awk '/^ +Front L/{print $5}'" t)))

(defun get-net ()
  (string-trim
 (string #\newline)
   (run-shell-command "cat /sys/class/net/e*/operstate" t)))

(defun get-signal-strength ()
  (string-trim
 (string #\newline)
   (run-shell-command "cat /sys/class/net/wl*/operstate" t)))

(defun get-battery-status ()
  (string-trim
  (string #\newline)
   (run-shell-command (concat
                       "upower -i /org/freedesktop/UPower/devices/battery_BAT0 "
                       "| grep perc "
                     "| awk '{print $2}'") t)))


(setf stumpwm:*screen-mode-line-format*
      (list "^2%d^]    ^1[|>^] %g    ^1[|>^] %W "
            " ^> "                      ; remaining elements become left-aligned
            "^2%c %t^] "               ; CPU usage indicators, from load-module
            ;; my own indicators:
             "  Net:[" '(:eval (get-net))
            "]  WiFi:[" '(:eval (get-signal-strength))
            "]  BAT:["  '(:eval (get-battery-status)) "]"))


(defcommand rxvt () ()
            "Launch the urxvt-client"
            (Run-or-raise "urxvtc"
                          '(:class "URXVT")))
(defun run-or-raise-prefer-group (cmd win-cls)
  "If there are windows in the same class, cycle in those. Otherwise call
run-or-raise with group search t."
  (let ((windows (group-windows (current-group))))
    (if (member win-cls (mapcar #'window-class windows) :test #'string-equal)
        (run-or-raise cmd `(:class ,win-cls) nil T)
        (run-or-raise cmd `(:class ,win-cls) T T))))


(defcommand run-or-raise-vimb () ()
            (run-or-raise-prefer-group "vimb" "Vimb"))

(defcommand run-or-raise-chromium () ()
            (run-or-raise-prefer-group "chromium" "Chromium"))

(defcommand run-or-raise-pcmanfm () ()
            (run-or-raise-prefer-group "pcmanfm" "Pcmanfm"))
;;????????????
(defcommand run-or-raise-ranger () ()
            (run-or-raise-prefer-group "ranger" "Ranger"))

(define-key *root-map* (kbd "B") "run-or-raise-ranger")

(define-key *root-map* (kbd "X") "run-ncmpcpp")

(defcommand run-ranger () ()
            "launch ncmpcpp in a terminal"
            (run-or-raise "urxvtc -e ranger"
                          '(:title "ranger")))

(defcommand terminal-raise () ()
  "Run or raise the urxvt terminal."
  (run-or-raise "urxvt -name Terminal -e tmux" '(:instance "Terminal")))

(define-key *root-map* (kbd "C") "terminal-raise")


(defcommand now-we-are-six (name age)
    ((:string "Enter your name: ")
     (:number "Enter your age: "))
  (message "~a, in six years you will be ~a" name (+ 6 age)))

(defcommand htop () ()
  "Run htop."
  (run-or-raise "urxvt -name HTop -e htop" '(:instance "HTop")))


(defcommand mutt () ()
  "Run mutt."
  (run-or-raise "urxvt -name Mutt -e mutt" '(:instance "Mutt")))

;;
;;(setf *grab-pointer-foreground* (xlib:make-color :green 0.1 :red 0.25 :blue 0.5)) ;;; blue pointer
(setf *grab-pointer-foreground* (xlib:make-color :green 0.1 :red 0.25 :blue 0.5))
(setf *grab-pointer-background* (lookup-color (current-screen) "blue"))
(setf *grab-pointer-character* 39);;; ball
;;(setf *grab-pointer-character-mask* 52);;; kreuz
(setf *grab-pointer-character-mask* 52)
;;(setf *grab-pointer-font* "fixed")


(amixer::defvolcontrol amixer-Master-5- "Master" "5%-")
(amixer::defvolcontrol amixer-Master-5+ "Master" "5%+")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "amixer-Master-5-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "amixer-Master-5+")
;;(define-key *top-map* (kbd "XF86AudioMute") "amixer-Master-toggle")


(defcommand raise-brightness () ()
  (run-s
hell-command "light -A 5"))

(defcommand lower-brightness () ()
  (run-shell-command "light -U 5"))


(define-key *top-map* (kbd "XF86Launch8") "lower-brightness")
(define-key *top-map* (kbd "XF86Launch9") "raise-brightness")

;;(define-key *top-map* (kbd "XF86MonBrightnessUp") "raise-brightness")
;;(define-key *top-map* (kbd "XF86MonBrightnessDown") "lower-brightness")

;; Web jump (works for Google)
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
    (substitute #\+ #\Space search)
    (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "google" "qutebrowser http://www.google.fr/search?q=")

(define-key *root-map* (kbd "M-s") "google")

;; search Vedbase
(defmacro make-veda-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
    (substitute #\+ #\Space search)
    (run-shell-command (concatenate 'string ,prefix search))))

(make-veda-jump "Vedatxt" "cd Vedatxt/ && urxvt --hold -e grep --color=auto -irn ")


(defcommand try () ()
        (let  ((*message-window-gravity* :center)
               (*message-window-padding* 50)
                (*timeout-wait 10))
        (message "~a" (run-shell-command "ls -a > a && cat a && rm a" t))))


(clear-window-placement-rules)

(define-frame-preference "Web"
  ;; frame raise lock (lock AND raise == jumpto)
  (0 t   t :instance "Mutt")
  (0 t   t :class "Vimb")
  (0 t   t :class "Chromium"))


(define-frame-preference "Files"
  ;; frame raise lock (lock AND raise == jumpto)
  (0 t   t :title "ranger")
  (0 t   t :class "Ranger"))


(defun shift-windows-forward (frames win)
"Exchange windows through cycling frames."
  (when frames
          (let ((frame (car frames)))
                  (shift-windows-forward (cdr frames)
                                         (frame-window frame))
                  (when win
                           (pull-window win frame)))))

(defcommand rotate-windows () ()
  (let* ((frames (group-frames (current-group)))
            (win (frame-window (car (last frames)))))
          (shift-windows-forward frames win)))

