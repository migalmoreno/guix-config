export GUILE_LOAD_PATH := $(pwd):$(GUILE_LOAD_PATH):$(XDG_CONFIG_HOME)/guix

channels-lock := guix time-machine -C channels-lock --

HOSTNAME := $(shell hostname)

.PHONY: all
all: pull update init home system build iso deploy

pull:
	guix pull --allow-downgrades -C channels-lock

update:
	guix time-machine -C channels -- describe -f channels > channels-lock.2
	mv channels-lock.2 channels-lock
	$(channels-lock) upgrade

init/%:
	$(channels-lock) init -L . quasar/system/$*.scm /mnt

home: home/${HOSTNAME}

home/%:
	$(channels-lock) home --allow-downgrades -L . reconfigure quasar/home/$*.scm

system: system/${HOSTNAME}

system/%:
	sudo -E $(channels-lock) system -L . reconfigure quasar/system/$*.scm

build/system/%:
	$(channels-lock) system build -L . quasar/system/$*.scm

build/home/%:
	$(channels-lock) home build -L . quasar/home/$*.scm

deploy/%:
	$(channels-lock) deploy -L . quasar/deployment/$*.scm

iso:
	$(channels-lock) system -L . image -t iso9660 quasar/system/install.scm
