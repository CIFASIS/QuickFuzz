# Publish files from a local darcs repo to a remote server via rsync.
# This is a workaround for a remote server which does not have darcs
# installed.

PATH=/usr/local/bin:/usr/bin:/bin

PROJECT=hogg

IMGNAME=$(PROJECT)-commits.png

SERVER=www
REMOTE=public_html/software/

MKDIR=mkdir -p

all: publish

# Update the code
darcs:
	darcs get ..

# Update the Haddock docs
haddock: darcs
	cd $(PROJECT)			&& \
	chmod +x Setup.hs		&& \
	./Setup.hs configure		&& \
	./Setup.hs haddock

# Update the activity graph
graph: darcs
	$(MKDIR) $(PROJECT)/images				&& \
	darcs-graph $(PROJECT)					&& \
	mv "/tmp/$(IMGNAME)" "$(PROJECT)/images/$(IMGNAME)"	&& \
	chmod 644 "$(PROJECT)/images/$(IMGNAME)"

#publish: haddock graph
publish: graph
	rsync -ave ssh $(PROJECT) $(SERVER):$(REMOTE)

clean:
	rm -rf $(PROJECT)
