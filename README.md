graphql-mode
============

[![MELPA](https://melpa.org/packages/graphql-mode-badge.svg)](https://melpa.org/#/graphql-mode)

`graphql-mode` is an emacs mode to edit GraphQL schema and queries.

## Installation

`graphql-mode` can be installed from MELPA repository at http://melpa.org/.

Once the installation is completed, any file with a *.graphql*
extension will be loaded with this mode.

You can optionally install `json-mode`, and it will be enabled in the
buffer that contains the response from a GraphQL service.

## Org-Babel integration
`graphql-mode` includes an integration with [Babel](https://orgmode.org/worg/org-contrib/babel/intro.html), org-mode's literate programming environment. You can enable it by loading `ob-graphql`:

``` emacs-lisp
(require 'ob-graphql)
```

Once it is loaded, you can evaluate `graphql` source blocks. The URL of the GraphQL server should be given by the `:url` header parameter:

``` org
#+BEGIN_SRC graphql :url https://countries.trevorblades.com/
  query GetContinents {
      continent(code: "AF") {
          name
	  code
      }
  }
#+END_SRC

#+RESULTS:
: {
:   "data": {
:     "continent": {
:       "name": "Africa",
:       "code": "AF"
:     }
:   }
: }
```

Variables can be passed into the source block from other source/example blocks via the `:variables` header parameter. The parameter should be the name of the variables source block, which should evaluate to the JSON body of the variables to pass into the query:

``` org
#+NAME: my-variables
#+begin_example
  {
      "continentCode": "AF"
  }
#+end_example

#+BEGIN_SRC graphql :url https://countries.trevorblades.com/ :variables my-variables
  query GetContinents($continentCode: String!) {
      continent(code: $continentCode) {
          name
	  code
      }
  }
#+END_SRC

#+RESULTS:
: {
:   "data": {
:     "continent": {
:       "name": "Africa",
:       "code": "AF"
:     }
:   }
: }

```

Additional headers, such as an `Authorization` header, can be passed into the query via the `:headers` header parameter. This parameter should be the name of another source block that evaluates to an alist of header names to header values:

``` org
#+NAME: my-headers
#+BEGIN_SRC emacs-lisp
  '(("Authorization" . "Bearer my-secret-token"))
#+END_SRC

#+BEGIN_SRC graphql :url https://countries.trevorblades.com/ :headers my-headers
  query GetContinents {
      continent(code: "AF") {
          name
	  code
      }
  }
#+END_SRC

#+RESULTS:
: {
:   "data": {
:     "continent": {
:       "name": "Africa",
:       "code": "AF"
:     }
:   }
: }
```
