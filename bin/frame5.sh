#!/bin/bash

ratpoison -c "frestore `tail -n 1 .config/ratframe/ratframe5`" ;  ratpoison -c next;  ratpoison -c focusright; ratpoison -c next; ratpoison -c focusdown; ratpoison -c next; ratpoison -c focusleft
