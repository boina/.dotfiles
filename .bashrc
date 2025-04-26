#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return



#alias ls='ls --color=force'


# Changing "ls" to "exa"
##alias ls='exa -l --color=always --group-directories-first' # my preferred listing
##alias la='exa -al --color=always --group-directories-first'  # all files and dirs
##alias ll='exa -a --color=always --group-directories-first'  # long format
##alias lt='exa -T --color=always --group-directories-first'  # tree listing not dot files
##alias lta='exa -aT --color=always --group-directories-first' # tree listing with dot files
##alias l.='exa -a | egrep "^\."'


# Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# confirm before overwriting something
alias cp="cp -i"
alias mv='mv -i'
#alias rm='rm -i'

PS1='[\u@\h \W]\$ '

##eval "$(starship init bash)"
