YAML    = stack.yaml
TESTS   = $(wildcard examples/*.pi)
ERRORS  = $(wildcard errors/*.pi)
DEST    = padovani@pianeta.di.unito.it:public_html/Software/FairCheck/
STACK   = stack --stack-yaml $(YAML)
NULL    =
SOURCES = \
  Atoms.hs \
  Checker.hs \
  Common.hs \
  Exceptions.hs \
  Formula.hs \
  Node.hs \
  Predicate.hs \
  Process.hs \
  Relation.hs \
  Render.hs \
  Resolver.hs \
  Interpreter.hs \
  Tree.hs \
  Type.hs \
  $(NULL)

all:
	@$(STACK) build

docs:
	@$(STACK) exec haddock -- --package-name FairCheck --hide Parser --ignore-link-symbol=Parser --html -o docs $(SOURCES:%=src/%)

watch:
	@$(STACK) build --file-watch

install:
	@$(STACK) install

dist:
	@cabal sdist

sync:
	@make -C html
	@scp html/*.* $(DEST)
	@scp dist/*.tar.gz $(DEST)

info:
	@$(STACK) exec happy -- -i src/Parser.y

%.j: examples/%.java
	@stack run $<

%.class: %.j
	@java -jar jasmin.jar $<

%.check_ok:
	@faircheck --log $(@:%.check_ok=%) || echo

check_examples:
	@echo
	@echo "SCRIPTS THAT MUST PASS"
	@echo "–————————————————————–"
	@for i in $(TESTS); do make -s $$i.check_ok; done

%.check:
	@faircheck --log $(@:%.check=%) || echo

check_errors:
	@echo
	@echo "SCRIPTS THAT MUST FAIL"
	@echo "–————————————————————–"
	@for i in $(ERRORS); do make -s $$i.check; done

update_licence:
	for hs in `find src -name "*.hs"`; do \
		TEMP=`mktemp`; \
		cp LICENSE.hs $$TEMP; \
		tail -n +15 <$$hs >>$$TEMP; \
		mv $$TEMP $$hs; \
	done

check: check_examples check_errors
	@echo

.PHONY: dist clean check check_examples check_errors docs

clean:
	@$(STACK) clean
	@rm -f `$(STACK) path --local-bin`/faircheck
