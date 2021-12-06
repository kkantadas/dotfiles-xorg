#!/bin/bash

exec ratpoison -c "frestore `tail -n 1 .config/ratframe/dump`" & llpp SB\ Vishvanatha\ Cakravarti/Sarartha\ Darshini.pdf & sleep .1s && urxvt -e vim Sarartha/SarathaDharsini.txt & sleep .1s  && exec ratpoison -c "vsplit 5/6" -c focusdown &


