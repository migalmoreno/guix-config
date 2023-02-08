GUIX_PROFILE := target/profiles/guix
GUIX := ./pre-inst-env ${GUIX_PROFILE}/bin/guix
CHANNELS_LOCK := ${GUIX} time-machine -C channels-lock.scm --
SRC_DIR := ./src
CONFIG := ${SRC_DIR}/conses/config.scm
HOST := $(shell hostname)
USER := $(shell whoami)

.PHONY: all
all: guix pull upgrade home system iso

channels-lock.scm: channels.scm
	guix time-machine -C channels.scm -- \
	describe -f channels > channels-lock-tmp.scm
	mv channels-lock-tmp.scm channels-lock.scm

channels-lock-local.scm: channels-local.scm
	guix time-machine -C channels-local.scm -- \
	describe -f channels > channels-lock-tmp.scm
	mv channels-lock-tmp.scm channels-lock-local.scm

guix: target/profiles/guix.lock

target:
	mkdir target

target/profiles:
	mkdir -p target/profiles

target/profiles/guix.lock: channels-lock.scm
	make target/profiles/guix

target/profiles/guix: target/profiles channels-lock.scm
	guix pull --allow-downgrades -C channels-lock.scm -p ${GUIX_PROFILE}

target/profiles/guix-local: target/profiles channels-lock-local.scm
	guix pull --allow-downgrades -C channels-lock-local.scm -p ${GUIX_PROFILE}

target/live.iso: guix target
	RDE_TARGET=system RDE_HOST=live $(CHANNELS_LOCK) \
	system -L . image -t iso9660 $(CONFIG) -r target/live-tmp.iso \
	mv target/live-tmp.iso target/live.iso

build/%: guix
	RDE_TARGET=system $(if $(word 3, $(subst /, , $@)),RDE_HE_IN_OS=true )\
	RDE_USER=$(word 3, $(subst /, ,$@)) \
	RDE_HOST=$(word 2, $(subst /, ,$@)) $(CHANNELS_LOCK) \
	system build $(CONFIG)

deploy/%: guix
	RDE_TARGET=deploy $(if $(word 3, $(subst /, , $@)),RDE_HE_IN_OS=true )\
	RDE_USER=$(word 3, $(subst /, ,$@)) \
	RDE_SYSTEM=$(word 2, $(subst /, ,$@)) $(CHANNELS_LOCK) \
	deploy $(CONFIG)

home: home/reconfigure/${USER}

home/build/%: guix
	RDE_TARGET=home RDE_HOST= RDE_USER=$* $(CHANNELS_LOCK) \
	home build $(CONFIG)

home/reconfigure/%: guix
	RDE_TARGET=home RDE_USER=$* $(CHANNELS_LOCK) home --allow-downgrades \
	reconfigure $(CONFIG)

system: system/reconfigure/${HOST}

system/init/%:
	RDE_TARGET=system RDE_HOST=$* RDE_HE_IN_OS=true \
	$(CHANNELS_LOCK) init $(CONFIG) /mnt

system/build/%: guix
	RDE_TARGET=system RDE_HOST=$* RDE_USER= $(CHANNELS_LOCK) \
	system build $(CONFIG)

system/reconfigure/%: guix
	RDE_TARGET=system RDE_HOST=$* sudo -E $(CHANNELS_LOCK) \
	system reconfigure $(CONFIG)
