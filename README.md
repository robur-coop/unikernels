# Unikernels

A MirageOS unikernel repository

All source code in this repository was developed from scratch by the specific authors. This code is put in the public domain.

If you want to use them in a corporate environment, and avoid any legal issues, you can buy a license, please contact https://robur.io

## Installation

You need to have `mirage` installed on your system.

In either of the directories, run `mirage configure` (see `mirage help
configure` for options), followed by `make depend` and `make` (read more
information [Hello MirageOS world](https://mirage.io/wiki/hello-world)).

Depending on the target, the name and type of the resulting binary varies. In
the default target, `unix`, its name is `./main.native`, and which may require
superuser privileges to listen on privileged ports
(use `doas (or sudo) ./main.native -l \*:debug`).

If you want to compile for Linux KVM, FreeBSD BHyve, OpenBSD VMM (by using
[solo5](https://github.com/solo5/solo5)), run `mirage configure -t hvt` (or
`-t virtio` for Google Compute Engine).

All unikernels require you to have µDNS installed (`opam pin add udns https://github.com/roburio/udns`).

## Primary authoritative nameservers

The [`primary`](primary/) subdirectory contains an unikernel with the
hardcoded zone (in its [unikernel.ml](primary/unikernel.ml)) named `mirage`,
listening on `10.0.42.2/24`, and some resource records.  It also
configures several TSIG keys, one for the seconday, another for update,
transfer, and key-management.

The [`primary-with-zone`](primary-with-zone/) contains no hardcoded
configuration, but serves [`data/zone`](primary-with-zone/data/zone) instead.

The [`primary-git`](primary-git/) subdirectory contains a unikernel which get as
boot parameter (`--remote`) a git repository where it expects at the top level
zonefiles, parses and serves them via DNS.

## Secondary authoritative nameserver

The [`secondary`](secondary/) subdirectory contains an unikernel which
listens on `10.0.42.4/24` by default and accepts TSIG keys as command line
arguments (`--keys`, can be provided multiple times).

The [`secondary-git`](secondary-git/) subdirectory contains a secondary that at
the moment only works with the unix target of mirage and dumps zonefiles in a
configurable local git repository (whenever a notify is received / AXFR has
succeeded).

A setup how they play together could be:
```
# ./solo5-hvt --net=tap0 -- primary/primary.hvt -l \*:debug
# ./solo5-hvt --net=tap1 -- secondary/secondary.hvt -l \*:debug --keys 10.0.42.2.10.0.42.4._transfer.mirage:SHA256:E0A7MFr4kfcGIRngRVBcBdFPg43XIb2qbGswcn66q4Q=
```

## Let's encrypt certification unikernel

The [`certificate`](certificate/) subdirectory contains an unikernel which
receives a key seed, and looks in DNS for a let's encrypt certificate.  If none
is found, a certificate signing request (`TLSA` record, type private (255)) is
put into DNS, and DNS is polled until a certificate occurs.

## Let's encrypt hidden secondary

This unikernels requires lets-encrypt to be installed as well (`opam pin add letsencrypt https://github.com/hannesm/ocaml-letsencrypt.git#nsupdate`).

The [`lets-encrypt`](lets-encrypt/) subdirectory contains an unikernel which
waits for zone transfers, and if a certificate signing request (`TLSA` record,
type private (255)) is received, a certificate is provisioned by the let's
encrypt servers using the DNS challenge. This certificate is put back into
DNS as `TLSA` record.

## Caching resolvers

The [`resolver`](resolver/) subdirectory contains a recursive resolver listening
on `10.0.42.5/24`.  A single key-management key is included,
`foo._key-management:SHA256:/NzgCgIc4yKa7nZvWmODrHMbU+xpMeGiDLkZJGD/Evo=`.

The [`stub`](stub/) subdirectory contains a stub resolver, which forwards all
requests to `141.1.1.1`.

