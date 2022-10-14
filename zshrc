# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="minimal"

ZSH_COLORIZE_TOOL=pygmentize
ZSH_COLORIZE_STYLE=rrt

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=6

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git docker brew tmux wakeonlan virtualenv colored-man-pages colorize fancy-ctrl-z )

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
export EDITOR='vim'
if [[ -z $SSH_CONNECTION ]]; then
  export EDITOR='nano'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

export PATH="/usr/local/sbin:$PATH"
export PATH="${HOME}/bin:$PATH"
export PATH="/usr/local/opt/openjdk/bin:$PATH"
export TERM="xterm-256color"

# minimal theme overwrite {
export ZSH_THEME_GIT_PROMPT_PREFIX="%{$reset_color%}%{$fg[white]%}"
export ZSH_THEME_GIT_PROMPT_SUFFIX=""
export ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%};%{$fg[white]%}%{$reset_color%}"
export ZSH_THEME_GIT_PROMPT_CLEAN="%{$reset_color%}"

vcs_status() {
  if (( ${+functions[in_svn]} )) && in_svn; then
    svn_prompt_info
  elif (( ${+functions[in_hg]} )) && in_hg; then
    hg_prompt_info
  else
    if ! __git_prompt_git rev-parse --git-dir &> /dev/null \
       || [[ "$(__git_prompt_git config --get oh-my-zsh.hide-info 2>/dev/null)" == 1 ]]; then
      return 0
    else
      echo "${ZSH_THEME_GIT_PROMPT_PREFIX}$(parse_git_dirty)${ZSH_THEME_GIT_PROMPT_SUFFIX}"
    fi
  fi
}
export PROMPT='$(vcs_status)%(?,%{$fg[green]%};,%{$fg[red]%};)%{$reset_color%}%b '
# }

# Local zshrc
source ${HOME}/.local.zshrc

#aliases
alias yabai-restart='killall yabai ; (cd /private/tmp && nohup yabai &)'
alias sha256="shasum -a 256"
alias ls='exa'
alias tree='exa --tree'
alias webserve='python3 -m http.server 8080'
[ -f /usr/local/bin/vim ] && alias vim='/usr/local/bin/vim' 

#functions
function fullpath() { echo $(pwd)/${1/.\//} }
function vman() {
  man -t "$@" | open -f -a Preview
}

function rmdsstore() {
  find "${@:-.}" -type f -name .DS_Store -delete
}

function tidy-brew() {
  brew update && brew upgrade
  brew cleanup -s
  brew doctor
  brew missing
}

function urlencode() {
  perl -pe "s/([^^a-z0-9\-_.'\!()])/sprintf('%%%02x',ord(\$1))/ieg"
}

function qr-gen() {
  read -r data
  [ -z "$data" ] && return

  local url='https://api.duckduckgo.com/?q=qr+code+%s&format=json'
  curl -LsSf $( printf "$url" $( urlencode <<< "$data" ) ) | jq -r .Answer | perl -ne 'print $1 if(/base64,([^"]+)/)' | \
    base64 -D | open -f -a Preview
}

# in case I keep writing `wget` and it doesn't exists...
if ! which wget >/dev/null ;then
  function wget() {
    curl --remote-header-name -O $@
  }
fi

# enables macOS to do wordleft/wordright
bindkey "^[[1;3C" forward-word
bindkey "^[[1;3D" backward-word
