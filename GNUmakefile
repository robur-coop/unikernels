-include Makefile.config

#  secondary-git
UNIKERNELS = \
  primary \
  primary-with-zone \
  primary-git \
  secondary \
  resolver \
  stub-resolver \
  certificate \
  lets-encrypt

MIRAGE_FLAGS ?=
MODE ?= "unix"

BUILD  = $(patsubst %, %-unikernel, $(UNIKERNELS))

build: $(BUILD)

%-unikernel:
	cd $* && \
	mirage configure -t $(MODE) $(MIRAGE_FLAGS) && \
	$(MAKE) depend && \
	$(MAKE) && \
	mirage clean
