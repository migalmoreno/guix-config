export GUILE_LOAD_PATH := $(pwd):$(XDG_CONFIG_HOME)/guix:$(GUILE_LOAD_PATH)

CHANNELS-LOCK := guix time-machine -C channels-lock --
CONFIG := conses/config.scm
HOST := $(shell hostname)
USER := $(shell whoami)

.PHONY: all
all: pull upgrade home system iso

pull:
	guix pull --allow-downgrades -C channels-lock

upgrade:
	guix time-machine -C channels -- describe -f channels > channels-lock.2
	mv channels-lock.2 channels-lock
	$(CHANNELS-LOCK) upgrade

init/%:
	RDE_TARGET=system RDE_SYSTEM=$* RDE_HE_IN_OS=true $(CHANNELS-LOCK) init -L . $(CONFIG) /mnt

home: home/${USER}

home/%:
	RDE_TARGET=home RDE_USER=$* $(CHANNELS-LOCK) home --allow-downgrades -L . reconfigure $(CONFIG)

system: system/${HOST}

system/%:
	RDE_TARGET=system RDE_SYSTEM=$* sudo -E $(CHANNELS-LOCK) system -L . reconfigure $(CONFIG)

build/home/%:
	RDE_TARGET=home RDE_SYSTEM= RDE_USER=$* $(CHANNELS-LOCK) home build -L . $(CONFIG)

build/system/%:
	RDE_TARGET=system RDE_SYSTEM=$* RDE_USER= $(CHANNELS-LOCK) system build -L . $(CONFIG)

build/%:
	RDE_TARGET=system $(if $(word 3, $(subst /, , $@)),RDE_HE_IN_OS=true )RDE_USER=$(word 3, $(subst /, ,$@)) \
	RDE_SYSTEM=$(word 2, $(subst /, ,$@)) $(CHANNELS-LOCK) system build -L . $(CONFIG)

deploy/%:
	RDE_TARGET=deploy $(if $(word 3, $(subst /, , $@)),RDE_HE_IN_OS=true )RDE_USER=$(word 3, $(subst /, ,$@)) \
	RDE_SYSTEM=$(word 2, $(subst /, ,$@)) $(CHANNELS-LOCK) deploy -L . $(CONFIG)

iso:
	RDE_TARGET=system RDE_SYSTEM=iso $(CHANNELS-LOCK) system -L . image -t iso9660 $(CONFIG)
