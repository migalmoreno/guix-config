GUIX_PROFILE := target/profiles/guix
GUIX := ./pre-inst-env ${GUIX_PROFILE}/bin/guix
CHANNELS_LOCK := ${GUIX} time-machine -C rde/channels-lock.scm --
SRC_DIR := ./src
ENTRY := ${SRC_DIR}/dotfiles/dispatch.scm
HOST := $(shell hostname)
USER := $(shell whoami)

.PHONY: all
all: guix pull upgrade home system iso

rde/channels-lock.scm: rde/channels.scm
	guix time-machine -C rde/channels.scm -- \
	describe -f channels > rde/channels-lock-tmp.scm
	mv rde/channels-lock-tmp.scm rde/channels-lock.scm

rde/channels-lock-local.scm: rde/channels-local.scm
	guix time-machine -C rde/channels-local.scm -- \
	describe -f channels > rde/channels-lock-tmp.scm
	mv rde/channels-lock-tmp.scm rde/channels-lock-local.scm

guix: target/profiles/guix-time-marker

repl:
	./pre-inst-env target/profiles/guix/bin/guix repl --listen=tcp:37146

target:
	mkdir target

target/profiles:
	mkdir -p target/profiles

target/profiles/guix-time-marker: rde/channels-lock.scm
	make target/profiles/guix
	touch $@

target/profiles/guix: target/profiles rde/channels-lock.scm
	guix pull --allow-downgrades -C rde/channels-lock.scm \
	-p ${GUIX_PROFILE}

target/profiles/guix-local: target/profiles rde/channels-lock-local.scm
	guix pull --allow-downgrades -C rde/channels-lock-local.scm \
	-p ${GUIX_PROFILE}

target/live.iso: guix target
	RDE_TARGET=system RDE_HOST=live $(CHANNELS_LOCK) \
	system -L . image -t iso9660 $(ENTRY) -r target/live-tmp.iso \
	mv target/live-tmp.iso target/live.iso

clean-target:
	rm -rf ./target

clean: clean-target

build/%: guix
	RDE_TARGET=system $(if $(word 3, $(subst /, , $@)),RDE_HE_IN_OS=true )\
	RDE_USER=$(word 3, $(subst /, ,$@)) \
	RDE_HOST=$(word 2, $(subst /, ,$@)) $(CHANNELS_LOCK) \
	system build $(ENTRY)

deploy/%: guix
	RDE_TARGET=deploy $(if $(word 3, $(subst /, , $@)),RDE_HE_IN_OS=true )\
	RDE_USER=$(word 3, $(subst /, ,$@)) \
	RDE_SYSTEM=$(word 2, $(subst /, ,$@)) $(CHANNELS_LOCK) \
	deploy $(ENTRY)

home: home/reconfigure/${USER}

home/build/%: guix
	RDE_TARGET=home RDE_HOST= RDE_USER=$* $(CHANNELS_LOCK) \
	home build $(ENTRY)

home/reconfigure/%: guix
	RDE_TARGET=home RDE_USER=$* $(CHANNELS_LOCK) home --allow-downgrades \
	reconfigure $(ENTRY)

system: system/reconfigure/${HOST}

system/init/%:
	RDE_TARGET=system RDE_HOST=$* RDE_HE_IN_OS=true \
	$(CHANNELS_LOCK) init $(ENTRY) /mnt

system/build/%: guix
	RDE_TARGET=system RDE_HOST=$* RDE_USER= $(CHANNELS_LOCK) \
	system build $(ENTRY)

system/reconfigure/%: guix
	RDE_TARGET=system RDE_HOST=$* sudo -E $(CHANNELS_LOCK) \
	system reconfigure $(ENTRY)
