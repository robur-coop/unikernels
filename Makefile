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

build:
	for dir in $(UNIKERNELS); do \
	   (cd $$dir && \
		echo "entered $$dir" && \
		echo "configure -t $(MODE) $(MIRAGE_FLAGS)" && mirage configure -t $(MODE) $(MIRAGE_FLAGS) && \
		echo "make depend" && $(MAKE) depend && \
		echo "make" && $(MAKE) && \
		echo "mirage clean" && mirage clean \
		echo "leaving $$dir") \
	done
