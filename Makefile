TFILES=$(wildcard ./blog/*.html)

clean-html:
	rm -rf $(TFILES)

clean-store:
	rm -rf ./.stores

clean-public:
	rm -rf ./public/*

clean: clean-html clean-store clean-public
	
