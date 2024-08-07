if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Supress welcome message
set fish_greeting

# Copy and Paste.
# Requires `pacman -S xsel`
alias pbcopy "xsel --clipboard --input"
alias pbpaste "xsel --clipboard --output"

# Aliases.
alias sys_update "fish -c ~/devel/dotfiles/sys_update.sh"
alias grep "rg"
alias ll "ls -ltrh"
alias docker_rmi_dangling "docker rmi (docker images -qa -f 'dangling=true')"
alias emacs "emacsclient -c"
alias tlmgr "/usr/share/texmf-dist/scripts/texlive/tlmgr.pl --usermode"

# Set our default text editor
set -gx EDITOR "emacsclient --tty"

# Merge history from other terminal windows.
history --merge

# Start X at login.
if status is-interactive
	if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
		exec startx -- -keeptty
	end
end

# Load the default Opam switch.
eval (opam env --set-switch)

# opam configuration
source /home/bruno/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true

# Rust
set -gx PATH "$HOME/.cargo/bin" $PATH

# Add ~/.local/bin to PATH
set -gx PATH "$HOME/.local/bin" $PATH

# Set LD_LIBRARY_PATH
set -gx LD_LIBRARY_PATH "/usr/local/lib" $LD_LIBRARY_PATH