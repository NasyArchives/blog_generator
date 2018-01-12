TFILES=$(wildcard ./blog/*.html)

clean:
	rm -rf $(TFILES)
	rm -rf ./.stores
