test pattern="":
    eask exec buttercup -L . --traceback pretty --only-error --pattern "{{ pattern }}"

lintaf:
    eask lint checkdoc
    eask lint declare
    eask lint elint
    eask lint indent
    eask lint keywords
    eask lint package
    eask lint regexps
