DOTFILES_DIR=$(shell pwd)

osx:
	ln -sf $(DOTFILES_DIR)/bash_profile.osx ~/.bash_profile
	ln -sf $(DOTFILES_DIR)/.aliases ~/.aliases
	ln -sf $(DOTFILES_DIR)/.bash_prompt ~/.bash_prompt
	ln -sf $(DOTFILES_DIR)/.exports ~/.exports
	ln -sf $(DOTFILES_DIR)/.functions ~/.functions
	ln -sf $(DOTFILES_DIR)/gitconfig ~/.gitconfig
	ln -sf $(DOTFILES_DIR)/gitignore ~/.gitignore
	mkdir -p ~/.emacs.d
	ln -sf $(DOTFILES_DIR)/init.el ~/.emacs.d/
	ln -sf $(DOTFILES_DIR)/snippets ~/.emacs.d/
	ln -sf $(DOTFILES_DIR)/emacs-color-theme-solarized ~/.emacs.d/
	ln -sf $(DOTFILES_DIR)/RProfile ~/.RProfile
	ln -sf $(DOTFILES_DIR)/ssh_config ~/.ssh/config
	ln -sf $(DOTFILES_DIR)/scripts ~/bin

linux:
	ln -sf $(DOTFILES_DIR)/bash_profile.linux ~/.bash_profile

	ln -sf $(DOTFILES_DIR)/gitconfig ~/.gitconfig
	ln -sf $(DOTFILES_DIR)/gitignore ~/.gitignore

	ln -sf $(DOTFILES_DIR)/emacs ~/.emacs
	ln -sf $(DOTFILES_DIR)/snippets ~/.emacs.d/

	ln -sf $(DOTFILES_DIR)/RProfile ~/.RProfile
	ln -sf $(DOTFILES_DIR)/ssh_config ~/.ssh/config

	ln -sf $(DOTFILES_DIR)/scripts ~/bin

clean:
	rm -f ~/.RProfile
	rm -f -d ~/.emacs.d/snippets
	rm -f ~/.emacs
	rm -f ~/.gitignore
	rm -f ~/.gitconfig
	rm -f ~/.bash_profile
	rm -f ~/.aliases
	rm -f ~/.bash_prompt
	rm -f ~/.exports
	rm -f ~/.functions
	rm -f ~/.ssh/config
	rm -f ~/bin
