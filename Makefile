
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

MIRAGE_FLAGS="--no-depext"

configure:
	for dir in $(UNIKERNELS); do \
	  (cd $$dir && echo "configuring $$dir" && mirage configure -t $(MODE) $(MIRAGE_FLAGS)) \
	done

build:
	for dir in $(UNIKERNELS); do \
	   (cd $$dir && echo "make depend $$dir" && $(MAKE) depend && echo "make $$dir" && $(MAKE)) \
	done

clean:
	for dir in $(UNIKERNELS); do \
	   (cd $$dir && echo "cleaning $$dir" && mirage clean && rm -f *.opam) \
	done
