if status is-interactive
    # Commands to run in interactive sessions can go here
end

# Copy and Paste.
# Requires `pacman -S xsel`
alias pbcopy "xsel --clipboard --input"
alias pbpaste "xsel --clipboard --output"

# Aliases.
alias grep "rg"
alias latr "ls -latr"
alias docker_rmi_dangling "docker rmi (docker images -qa -f 'dangling=true')"

# Merge history from other terminal windows.
history --merge

# Start X at login.
if status is-interactive
	if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
		exec startx -- -keeptty
	end
end

# Load the default Opam switch.
eval (opam env --switch=default)

# Added automatically by Opam:
# opam configuration
source /home/bruno/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
