alias k="kubectl"
alias kcc="kubectl config current-context"
alias kgc="kubectl config get-contexts"
alias ksc="kubectl config set-context"
alias t="tmux"
alias g="git"
alias d="docker"
alias gc="gcloud"
alias tf="terraform"
alias latr="ls -latr"
alias helm3="/Users/brunoflores/Downloads/helm/helm-v3.1.1-darwin-amd64/helm"
alias grep="rg"
alias log="git log --show-signature -5"
alias docker_rmi_dangling="docker rmi (docker images -qa -f 'dangling=true')"

set PATH /usr/local/go/bin $PATH

test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Applications/google-cloud-sdk/path.fish.inc' ]; . '/Applications/google-cloud-sdk/path.fish.inc'; end

[ -f /usr/local/share/autojump/autojump.fish ]; and source /usr/local/share/autojump/autojump.fish

source /Users/brunoflores/.ghcup/env

set PATH $HOME/.cargo/bin $PATH

set JAVA_HOME (/usr/libexec/java_home)
set ANT_HOME /Users/brunoflores/apache-ant-1.10.6
set PATH $ANT_HOME/bin $PATH
set PATH /Users/brunoflores/Downloads/gcc-arm-none-eabi-9-2020-q2-update/bin $PATH
set PATH /Applications/STMicroelectronics/STM32Cube/STM32CubeProgrammer/STM32CubeProgrammer.app/Contents/MacOs/bin $PATH
set PATH /Applications/STMicroelectronics/STM32Cube/STM32CubeProgrammer/STM32CubeProgrammer.app/Contents/MacOs/bin $PATH

set PATH /opt/anaconda3/bin $PATH
set PATH /usr/local/opt/qt/bin $PATH
set PATH /opt/local/bin $PATH
set PATH /opt/ghc/bin $PATH
set PATH /Applications/Julia-1.6.app/Contents/Resources/julia/bin $PATH
set PATH /Users/brunoflores/Downloads/flyway-7.5.3 $PATH

set PATH /Users/brunoflores/Downloads/dafny $PATH

set PATH /Users/brunoflores/aws-cli $PATH

# fish_vi_key_bindings
fish_default_key_bindings

source /Users/brunoflores/.config/fish/kubectl_completion.fish

# https://www.jenv.be
status --is-interactive; and source (jenv init -|psub)

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /opt/anaconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

conda config --set changeps1 False

history --merge

# opam configuration
source /Users/brunoflores/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
