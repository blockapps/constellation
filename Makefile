.PHONY: hlint stylish
hlint:
	hlint Constellation "--ignore=Parse error" -XTypeApplications; \
		hlint test "--ignore=Parse error" -XTypeApplications

stylish:
	find ./Constellation -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i; \
		find ./test -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i; \
