GUIX_PROFILE := target/profiles/guix
GUIX_PROFILE_LOCAL := target/profiles/guix-local
PROFILE := ${GUIX_PROFILE_LOCAL}
LOCK := rde/channels-lock.scm
LOCK_LOCAL := rde/channels-lock-local.scm
CHANNELS := ${LOCK_LOCAL}
GUIX := ./pre-inst-env ${PROFILE}/bin/guix
GUIX_LOCAL := ../../guile/guix/pre-inst-env ./pre-inst-env guix
GUIX_LOCK := ${GUIX} time-machine --disable-authentication -C ${CHANNELS} --
CMD := $(GUIX_LOCK)
SRC_DIR := ./src
ENTRY := ${SRC_DIR}/migalmoreno/dispatcher.scm
HOST := $(shell hostname)
USER := $(shell whoami)
EXTRA_OPTIONS := --substitute-urls=https://ci.guix.gnu.org

%/local: CMD := ${GUIX_LOCAL}
%/local: CHANNELS := ${LOCK_LOCAL}
%/local: PROFILE := ${GUIX_PROFILE_LOCAL}

%/external: CHANNELS := ${LOCK}
%/external: PROFILE := ${GUIX_PROFILE}

.PHONY: all
all: guix pull upgrade home system iso

rde/channels-lock.scm: rde/channels.scm
	guix time-machine -C rde/channels.scm -- \
	describe -f channels > rde/channels-lock-tmp.scm
	mv rde/channels-lock-tmp.scm rde/channels-lock.scm

rde/channels-lock-local.scm: rde/channels-local.scm
	guix time-machine --disable-authentication -C rde/channels-local.scm -- \
	describe -f channels > rde/channels-lock-tmp.scm
	mv rde/channels-lock-tmp.scm rde/channels-lock-local.scm

guix: target/guix-time-marker

repl:
	${GUIX} shell guile-next guix guile-ares-rs -- guile \
	-c "((@ (nrepl server) run-nrepl-server) #:port 7888)"

target:
	mkdir target

target/profiles:
	mkdir -p target/profiles

target/guix-time-marker: ${CHANNELS}
	make ${PROFILE}
	touch $@

target/profiles/guix: target/profiles rde/channels-lock.scm
	guix pull --allow-downgrades -C rde/channels-lock.scm \
	-p ${GUIX_PROFILE}

target/profiles/guix-local: target/profiles rde/channels-lock-local.scm
	guix pull --disable-authentication --allow-downgrades \
	-C rde/channels-lock-local.scm -p ${GUIX_PROFILE_LOCAL}

target/live.iso: guix target
	RDE_TARGET=system RDE_HOST=live ${CMD} \
	system -L . image -t iso9660 $(ENTRY) -r target/live-tmp.iso \
	mv target/live-tmp.iso target/live.iso

clean-target:
	rm -rf ./target

clean: clean-target

build/%: guix
	RDE_TARGET=system $(if $(word 3, $(subst /, , $@)),RDE_HE_IN_OS=true )\
	RDE_USER=$(word 3, $(subst /, ,$@)) \
	RDE_HOST=$(word 2, $(subst /, ,$@)) ${CMD} \
	system build $(ENTRY)

deploy/%: guix
	RDE_TARGET=deploy $(if $(word 3, $(subst /, , $@)),RDE_HE_IN_OS=true )\
	RDE_USER=$(word 3, $(subst /, ,$@)) \
	RDE_HOST=$(word 2, $(subst /, ,$@)) ${CMD} \
	deploy $(ENTRY)

home: home/reconfigure/${USER}
home/local: home/reconfigure/${USER}/local

home/build/%: guix
	RDE_TARGET=home RDE_HOST= RDE_USER=$(word 1, $(subst /, ,$*)) ${CMD} \
	home build $(ENTRY)

home/reconfigure/%: guix
	RDE_TARGET=home RDE_USER=$(word 1, $(subst /, ,$*)) ${CMD} \
	home --allow-downgrades reconfigure $(ENTRY)

system: system/reconfigure/${HOST}
system/local: system/reconfigure/${HOST}/local

system/init/%:
	RDE_TARGET=system RDE_HOST=$(word 1, $(subst /, ,$*)) \
	RDE_HE_IN_OS=true ${CMD} init $(ENTRY) /mnt

system/build/%: guix
	RDE_TARGET=system RDE_HOST=$(word 1, $(subst /, ,$*)) \
	RDE_USER= ${CMD} system build $(ENTRY)

system/reconfigure/%: guix
	RDE_TARGET=system RDE_HOST=$(word 1, $(subst /, ,$*)) \
	sudo -E ${CMD} system --allow-downgrades reconfigure $(ENTRY)
