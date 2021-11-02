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

## Querying Endpoints

To send a query to a server, you will first need the
[`request`](https://github.com/tkf/emacs-request) package. Then use
`graphql-send-query` (`C-c C-c`) to send a query.

If you have a [`.graphqlconfig`](https://github.com/jimkyndemeyer/graphql-config-examples) file, you can select an endpoint configuration
with `graphql-select-endpoint` (`C-c C-l`).

To send additional headers for a request, `graphql-extra-headers` must be
set. It is automatically set by `graphql-select-endpoint`, or you can edit its
value using JSON with `graphql-edit-headers` (`C-c e h`).
