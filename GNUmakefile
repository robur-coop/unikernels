-include Makefile.config

UNIKERNELS = \
  primary \
  primary-with-zone \
  secondary \
  resolver \
  stub-resolver \
  certificate \
  lets-encrypt \
  client \
  primary-git \
  tlstunnel

ifeq ($(MODE),unix)
UNIKERNELS += secondary-git
endif

MODE ?= "unix"

BUILD  = $(patsubst %, %-build, $(UNIKERNELS))
CI     = $(patsubst %, %-ci, $(UNIKERNELS))
CLEAN  = $(patsubst %, %-clean, $(UNIKERNELS))

build: $(BUILD)
ci: $(CI)
clean: $(CLEAN)

%-build:
	cd $* && \
	mirage configure -t $(MODE) $(MIRAGE_FLAGS) && \
	$(MAKE)

%-ci:
	cd $* && \
	mirage configure -t $(MODE) $(MIRAGE_FLAGS) && \
	make depend && \
	$(MAKE)

%-clean:
	-cd $* && mirage clean
