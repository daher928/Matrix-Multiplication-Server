# Matrix-Multiplication-Server

Erlang Matrix multiplication server

### Prerequisites

<p> You should have Erlang and OTP installed on your machine. </p>

Visit https://www.erlang.org/downloads to download and install.

### Module Compilation

$ c(matrix).
$ c(matrix_server).
$ c(matrix_server_supervisor).

### Running 

To start the server:
    $ matrix_server:start_server().

### Matrix structure

A tuple of rows.
    e.g. Mat1 = { {1,2,3}, {4,5,6} } %% 2x3 Matrix
