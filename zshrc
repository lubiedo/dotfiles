# vim: set foldmethod=marker:
# env {{{
# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.oh-my-zsh"

ZSH_THEME="minimal"
ZSH_COLORIZE_TOOL=pygmentize
ZSH_COLORIZE_STYLE=rrt
DISABLE_MAGIC_FUNCTIONS=true

export UPDATE_ZSH_DAYS=6
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:$PATH"
plugins=(git docker brew tmux virtualenv colored-man-pages colorize fancy-ctrl-z fzf)

source $ZSH/oh-my-zsh.sh

# Preferred editor for local and remote sessions
export EDITOR='vim'
if [[ -z $SSH_CONNECTION ]]; then
  export EDITOR='nano'
fi

export PATH="/usr/local/sbin:$PATH"
export PATH="${HOME}/bin:$PATH"
export PATH="${HOME}/.config/emacs/bin:$PATH"
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
alias sha256="shasum -a 256"
alias ls='exa'
alias lnew='ll -snew'
alias tree='exa --tree'
alias webserve='python3 -m http.server 8080'
alias die="/Applications/DiE.app/Contents/MacOS/DiE"
alias emacstart="emacs --bg-daemon && emacsclient -c"
[ -f /usr/local/bin/vim ] && alias vim='/usr/local/bin/vim' 
[ -d /opt/homebrew/Cellar/binutils ]&& alias strings="/opt/homebrew/Cellar/binutils/*/bin/gstrings"
# }}}

# functions {{{
function fullpath() { echo $(pwd)/${1/.\//} }
function vman() {
  man -t "$@" | open -f -a Preview
}

function rmdsstore() {
  find "${@:-.}" -type f -name .DS_Store -delete
}

function filesum() {
  local FILES=( $* )
  OLDIFS=$IFS
  IFS=' '
  for FILE in ${FILES[@]};do
    if ! [ -f "$FILE" ]; then
      echo "Error: file '${FILE}' does not exist."
      continue
    fi
    echo -n "* File: " && basename $FILE
    echo -n "* Size: " && stat -f '%z' $FILE
    echo -n "* Type: " && file -b $FILE
    echo -n "* MD5: " && md5 -q $FILE
    echo -n "* SHA1: " && (shasum -a 1 $FILE | cut -d' ' -f1)
    echo -n "* SHA256: " && (shasum -a 256 $FILE | cut -d' ' -f1)
  done
  IFS=$OLDIFS
}

# in case I keep writing `wget` and it doesn't exists...
if ! which wget >/dev/null ;then
  function wget() {
    curl --remote-header-name -O $@
  }
fi

# {{{ binary refinery zsh hack
function alias-noglob {
    while read -r entrypoint; do
        alias $entrypoint="noglob $entrypoint"
    done
}

python3 <<EOF 2>/dev/null | alias-noglob
import pkg_resources
for ep in pkg_resources.iter_entry_points('console_scripts'):
    if ep.module_name.startswith('refinery'):
        print(ep.name)
EOF
# }}}

# }}}

