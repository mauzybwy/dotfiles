export BASH_SILENCE_DEPRECATION_WARNING=1

export PATH=$PATH:$HOME/.maestro/bin

# history size
export HISTFILESIZE=1000000
export HISTSIZE=1000000

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
    source "$HOME/.bashrc"
    fi
fi

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/mauzy/Downloads/google-cloud-sdk/path.bash.inc' ]; then . '/Users/mauzy/Downloads/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/mauzy/Downloads/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/mauzy/Downloads/google-cloud-sdk/completion.bash.inc'; fi
