if status is-interactive
    # Commands to run in interactive sessions can go here
end

starship init fish | source

# Changing "ls" to "exa"
alias ls='exa -l --color=always --group-directories-first' # my preferred listing
alias la='exa -al --color=always --group-directories-first'  # all files and dirs
alias ll='exa -a --color=always --group-directories-first'  # long format
alias lt='exa -T --color=always --group-directories-first'  # tree listing not dot files
alias lta='exa -aT --color=always --group-directories-first' # tree listing with dot files
alias l.='exa -a | egrep "^\."'


# Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'


# Custom key bindings. Similar to emacs.

bind \eb 'prevd-or-backward-word'
bind \ef 'nextd-or-forward-word'
