-include Makefile.config

#  secondary-git
#  primary-git
UNIKERNELS = \
  primary \
  primary-with-zone \
  secondary \
  resolver \
  stub-resolver \
  certificate \
  lets-encrypt

MIRAGE_FLAGS += --prng fortuna
MODE ?= "unix"

BUILD  = $(patsubst %, %-unikernel, $(UNIKERNELS))
CLEAN  = $(patsubst %, %-clean, $(UNIKERNELS))

build: $(BUILD)
clean: $(CLEAN)

%-unikernel:
	cd $* && \
	mirage configure -t $(MODE) $(MIRAGE_FLAGS) && \
	$(MAKE) depend && \
	$(MAKE) && \
	mirage clean

%-clean:
	-cd $* && mirage clean
