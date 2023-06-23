# vim: set foldmethod=marker:
# env {{{
# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.oh-my-zsh"

ZSH_THEME="minimal"
ZSH_COLORIZE_TOOL=pygmentize
ZSH_COLORIZE_STYLE=rrt
DISABLE_MAGIC_FUNCTIONS=true

export UPDATE_ZSH_DAYS=6
export PATH="/opt/homebrew/bin:$PATH"
plugins=(git docker brew tmux virtualenv colored-man-pages colorize fancy-ctrl-z fzf)

source $ZSH/oh-my-zsh.sh

# Preferred editor for local and remote sessions
export EDITOR='vim'
if [[ -z $SSH_CONNECTION ]]; then
  export EDITOR='nano'
fi

export PATH="/usr/local/sbin:$PATH"
export PATH="${HOME}/bin:$PATH"
export PATH="/usr/local/opt/openjdk/bin:$PATH"
export TERM="xterm-256color"
# }}}

# minimal theme overwrite {{{
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
# }}}

# Local zshrc
source ${HOME}/.local.zshrc

# enables macOS to do wordleft/wordright
bindkey "^[[1;3C" forward-word
bindkey "^[[1;3D" backward-word

#aliases {{{
alias yabai-restart='killall yabai ; (cd /private/tmp && nohup yabai &)'
alias sha256="shasum -a 256"
alias ls='exa'
alias lnew='ll -snew'
alias tree='exa --tree'
alias webserve='python3 -m http.server 8080'
alias vscode="open -a Visual\\ Studio\\ Code"
[ -f /usr/local/bin/vim ] && alias vim='/usr/local/bin/vim' 
# }}}

# functions {{{
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

function filesum() {
  local FILE=$1
  echo -n "* Size: " && stat -f '%z' $FILE
  echo -n "* Type: " && file -b $FILE
  echo -n "* MD5: " && md5 -q $FILE
  echo -n "* SHA1: " && (shasum -a 1 $FILE | cut -d' ' -f1)
  echo -n "* SHA256: " && (shasum -a 256 $FILE | cut -d' ' -f1)
}

# zettelkasten
function zettelkasten_sync() {
  local DO=$1

  case $DO in
    up)
      /usr/local/bin/rclone copy --update --verbose --transfers 30 --checkers 8 --contimeout 60s --timeout 300s --retries 3 \
        --low-level-retries 10 --stats 1s ~/Documents/zettelkasten gdrive:zettelkasten
              ;;
    down)
      /usr/local/bin/rclone copy --update --verbose --transfers 30 --checkers 8 --contimeout 60s --timeout 300s --retries 3 \
        --low-level-retries 10 --stats 1s gdrive:zettelkasten ~/Documents/zettelkasten
              ;;
    *)
      echo "actions: up/down"
  esac
}

# Ooooo
function beats() {
  local ARG=$1
  [ -z $ARG ] && {
    echo "beats [start|stop]" && return
  }

  local VOL=${VOL:-50}
  local FRQ=${FRQ:-45}
  local TON=${TON:-120}
  case $ARG in
    start) open -j -g "ooooo://start?frequency=${FRQ}&tone=${TON}&binaural=true&volume=${VOL}" ;;
    stop) open -j -g "ooooo://stop" ;;
  esac
}

# in case I keep writing `wget` and it doesn't exists...
if ! which wget >/dev/null ;then
  function wget() {
    curl --remote-header-name -O $@
  }
fi

# }}}

