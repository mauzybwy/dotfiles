export BASH_SILENCE_DEPRECATION_WARNING=1

################################################################################
# Prompt
################################################################################

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

################################################################################

parse_git_branch() {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
}

#if [ "$color_prompt" = yes ]; then
case "$TERM" in
    xterm*|rxvt*|screen*)
        export PS1="\[\033[0;36m\]\h\[\033[1;0m\] [\[\033[1;36m\]\w\[\033[00m\]]\[\033[0;33m\] \$(parse_git_branch)\[\033[00m\]\n\[\033[1;32m\]\u\[\033[1;0m\] $ "
        
        export PROMPT_COMMAND="echo && echo \$(pwd) > /tmp/whereami"
        ;;
    *)
        PS1='$'
        ;;
esac
unset color_prompt force_color_prompt

################################################################################

# If this is an xterm set the title to user@host:dir
case "$TERM" in
    xterm*|rxvt*)
	PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
	;;
    *)
	;;
esac

################################################################################

export REACT_EDITOR="/Applications/Emacs.app/Contents/MacOS/Emacs"

################################################################################

_nvmrc_hook() {
  if [[ $PWD == $PREV_PWD ]]; then
    return
  fi
  
  PREV_PWD=$PWD
  [[ -f ".nvmrc" ]] && nvm use
}

if ! [[ "${PROMPT_COMMAND:-}" =~ _nvmrc_hook ]]; then
  PROMPT_COMMAND="_nvmrc_hook${PROMPT_COMMAND:+;$PROMPT_COMMAND}"
fi

################################################################################

# source ~/.bash_profile

# pnpm
export PNPM_HOME="/Users/mauzy/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end


# tat: tmux attach
function tat {
  name=$(basename `pwd` | sed -e 's/\.//g')

  if tmux ls 2>&1 | grep "$name"; then
    tmux attach -t "$name"
  elif [ -f .envrc ]; then
    direnv exec / tmux new-session -s "$name"
  else
    tmux new-session -s "$name"
  fi
}

# zat: zellij attach
function zat {
  name=$(basename `pwd` | sed -e 's/\.//g')
  zellij attach "$name" --create
}

export PATH="/Library/TeX/texbin:/opt/homebrew/bin:/Users/mauzy/.local/bin:/Users/mauzy/.local/share/google-cloud-sdk/bin:$PATH"
export PATH="$PATH":"$HOME/.maestro/bin"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/mauzy/.local/share/google-cloud-sdk/path.bash.inc' ]; then . '/Users/mauzy/.local/share/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/mauzy/.local/share/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/mauzy/.local/share/google-cloud-sdk/completion.bash.inc'; fi

export NPM_TOKEN="2565b89efbdb2fa02c71948f502f969d8df5dbe2"
export ANDROID_HOME="/Users/mauzy/Library/Android/sdk"
export SENTRY_AUTH_TOKEN="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"

alias ls='eza --git'
alias l='ls -CF'
alias la='ls -A'
alias ll='ls -alF'
alias ssh="kitty +kitten ssh"
# alias ls='ls --color=auto'

alias p='pnpm'

alias port='sudo lsof -i'

alias cgrep="grep --color=auto"

if [ -f ~/.git-completion.bash ]; then
  . ~/.git-completion.bash
fi

 eval "$(zoxide init bash)"

 [[ -f ~/.bash-preexec.sh ]] && source ~/.bash-preexec.sh

# source ~/.local/share/blesh/ble.sh
eval "$(atuin init bash)"

[[ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]] && . "/opt/homebrew/etc/profile.d/bash_completion.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
