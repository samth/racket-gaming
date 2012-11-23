clean:
	find . -name "*~" -exec rm -f {} \;
	find . -name compiled -exec rm -rf {} \;
