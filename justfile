ALL_VERSIONS := "27.1 27.2 28.1 28.2 29.1 29.2 29.3 master"

install *versions="master":
    for version in {{ versions }}; do \
      eask docker $version install-deps --dev ; \
    done

lint *versions="master":
    for version in {{ versions }}; do \
      eask docker ${version} lint checkdoc ; \
      eask docker ${version} lint declare ; \
      eask docker ${version} lint elint ; \
      eask docker ${version} lint indent ; \
      eask docker ${version} lint keywords ; \
      eask docker ${version} lint package ; \
    done

_test version:
    eask docker {{ version }} exec buttercup -L . --traceback pretty

test *versions="master":
    for version in {{ versions }}; do \
      just _test ${version} ; \
    done

install-all:
    just install {{ ALL_VERSIONS }}

test-all:
    just test {{ ALL_VERSIONS }}
