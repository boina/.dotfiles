# Start neofetch before powerlevel otherwise there is a warning printed in the console.
neofetch

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# The following lines were added by compinstall
zstyle :compinstall filename '/home/netlak/.zshrc'

autoload -Uz compinit
compinit

##### End of lines added by compinstall

# Zsh history
HISTFILE=~/.zshHistfile
HISTSIZE=11000
SAVEHIST=10000

# Load aliases file
source $HOME/.config/aliases

# Emacs key-bindings
bindkey -e

# Options
setopt extendedglob nomatch
unsetopt autocd beep

setopt prompt_subst
setopt always_to_end
setopt append_history
setopt auto_menu
setopt complete_in_word
setopt hist_expire_dups_first
setopt hist_ignore_dups

zstyle ':completion:*' menu select

# Load plugins
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Options for history substring search plugin
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=white,fg=red,bold'
HISTORY_SUBSTRING_SEARCH_FUZZY=1
bindkey '^[^P' history-substring-search-up
bindkey '^[^N' history-substring-search-down

# Options for autosuggestions plugin
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE=\'fg=60\'

# Load powerlevel theme
source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh


export LS_COLORS="$(vivid generate $HOME/.dotfiles/LS_COLOR_Themes/catppuccin-mocha.yml)"
