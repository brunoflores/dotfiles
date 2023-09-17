# Installation

```fish
ln -s ~/devel/dotfiles/arch/.xserverrc ~/.xserver
ln -s ~/devel/dotfiles/arch/.xinitrc ~/.xinitrc
ln -s ~/devel/dotfiles/fish/config.fish ~/.config/fish/config.fish
ln -s ~/devel/dotfiles/fish/fish_prompt.fish ~/.config/fish/functions/fish_prompt.fish
ln -s ~/devel/dotfiles/i3/config ~/.config/i3/config
```

Add to `~/.emacs`:

```fish
(load "~/devel/dotfiles/emacs/emacs.el")
```

Make executable:

```fish
chmod +x sys_update.sh
```
