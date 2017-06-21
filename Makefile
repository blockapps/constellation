.PHONY: hlint stylish
hlint:
	hlint Constellation; \
		hlint test

stylish:
	find ./Constellation -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i; \
		find ./test -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i; \
