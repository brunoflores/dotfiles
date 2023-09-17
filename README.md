# Installation

```fish
ln -s ~/devel/dotfiles/arch/.xserverrc ~/.xserver
ln -s ~/devel/dotfiles/arch/.xinitrc ~/.xinitrc
ln -s ~/devel/dotfiles/fish/config.fish ~/.config/fish/config.fish
```

Add to `~/.emacs`:

```fish
(load "~/devel/dotfiles/emacs/emacs.el")
```

Make executable:

```fish
chmod +x sys_update.sh
```
