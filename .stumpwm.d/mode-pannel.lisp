;;mode-Line
;(load-module "battery-portable")
;(load-module "wifi")
;(load-module "net")
;(load-module "mem")

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


