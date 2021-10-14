# Some useful emacs keybindings and workflows

#### My Emacs install and doom setup
- `brew install emacs`
- [Follow doom installation guide](https://github.com/hlissner/doom-emacs#install)
- launch from terminal with `$ emacs`
- I am on Fedora btw

#### My SICP exercises setup
- ```Alt-x eww-mode``` for browser mode
- ```Alt-X eww``` and type in [SICP URL](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html)
- ```Alt-X scheme-mode``` to enable syntax highlighting **in the SICP textbook**.
- ```Space W v``` for new window in vertical split
- ```Space .``` and open exercise files

#### Navigating Windows
- ```Space W s``` for horizontal split
- ```Space W```
  - `j` to move to window below current
  - `k` for window up
  - `h` for left window
  - `l` for right window (confusing right?)
  - All above letters in capital (e.g `Space W J`)move the current window to desired position
  
#### Executing a Scheme program
- make sure `(scheme +mit)` is uncommented in lang section in ~/.doom.d/init.el
- `Alt X run-scheme` to get scheme repl in new buffer
- `Ctrl c Ctrl l` and then type in file path

