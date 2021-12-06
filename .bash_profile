# ~/.bash_profile

[[ -f ~/.bashrc ]] && . ~/.bashrc

[[ $(fgconsole 2>/dev/null) == 1 ]] && exec startx -- vt1 &> /dev/null
#[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx

