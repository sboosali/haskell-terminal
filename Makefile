.PHONY: install test terminal-ev terminal-app

install:
	stack install

test:
	stack test --ta "-t 3 -j1" --coverage

doc:
	stack haddock

terminal-ev: install
	~/.local/bin/terminal-ev

termina-demo: install
	~/.local/bin/terminal-out
