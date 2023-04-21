all: ocaml rust rustrelease

ocaml:
	cd ocaml; dune build

rust:
	cd rust; cargo build

rustrelease:
	cd rust; cargo build --release

refinterp:
	$(MAKE) -C lua all

test: refinterp ocaml rustrelease
	ocaml -I +unix unix.cma tests/runtests.ml tests/

clean:
	cd ocaml; dune clean
	cd rust; cargo clean; rm -f Cargo.lock
	$(MAKE) -C lua clean

.PHONY: all ocaml rust rustrelease refinterp test clean
