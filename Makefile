.phony: test release

test:
	scripts/smoke.sh
	./epsilon --exec epsilon.release:selftest

release:
	./epsilon --exec epsilon.release:generate
