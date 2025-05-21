This repository implements an Erlang-based concurrent server designed for performing matrix multiplication. The server utilizes Erlang's OTP framework for robust and scalable concurrent processing of matrix operations. It defines a specific tuple-based structure for representing matrices and provides functionalities to compile and run the server.

## Module Descriptions:

*   **`matrix.erl`**: This module provides fundamental functions for matrix manipulation. These include capabilities such as generating a matrix initialized with zeros (`getZeroMat/2`), extracting a specific row (`getRow/2`) or column (`getCol/2`) from a matrix, and modifying the value of a particular element within a matrix (`setElementMat/4`).

*   **`matrix_server.erl`**: This module constitutes the core server logic for handling matrix multiplication requests. It exposes an API for clients, primarily through the `mult/2` function, to submit matrices for multiplication. Upon receiving a request, the `server_loop/0` function, which is the main request handler, spawns multiple worker processes. Each worker process, using the `cross/5` function, calculates a single element of the resulting matrix. The server then collects these individual results and assembles the final product matrix. This concurrent approach allows for efficient matrix multiplication.

*   **`matrix_server_supervisor.erl`**: This module acts as a supervisor for the `matrix_server`. Its primary responsibility is to start the `matrix_server` process using `matrix_server_start/0`. Crucially, it also monitors the `matrix_server` and ensures its reliability by automatically restarting it in the event of a crash, thereby maintaining the availability of the matrix multiplication service.

## Concurrency Model:

The `matrix_server.erl` module implements an efficient concurrency model for matrix multiplication. When a client sends a multiplication request (e.g., `Mat1 * Mat2`), the server (`matrix_server`) doesn't perform the entire calculation sequentially within its own process. Instead, it leverages Erlang's lightweight concurrency capabilities:

1.  **Process Spawning**: For each element in the resulting product matrix, the `mult2/4` function spawns a new, short-lived Erlang process.
2.  **Parallel Element Calculation**: Each of these spawned processes is assigned the task of calculating a single element of the final matrix. This is achieved by executing the `cross/5` function, which computes the dot product of the corresponding row from `Mat1` and column from `Mat2`.
3.  **Result Aggregation**: As these individual worker processes complete their calculations, they send their results (the calculated element and its position in the matrix) back to the `mult2/4` function. The `mult_loop/2` function, called by `mult2/4`, then systematically collects these partial results and assembles them into the final product matrix.

This approach allows for a high degree of parallelism, as the calculation of each element of the resulting matrix can occur concurrently, significantly speeding up the multiplication of large matrices. The use of lightweight processes ensures that spawning many of them is efficient and doesn't impose a large overhead.

## Running the Server:

To run the Erlang matrix multiplication server, follow these steps:

1.  **Prerequisites**: Ensure that Erlang and OTP are installed on your machine. The `README.md` file mentions that you can find download and installation instructions at [https://www.erlang.org/downloads](https://www.erlang.org/downloads).
2.  **Compilation**: Open an Erlang shell and navigate to the directory containing the source code. Compile each of the necessary modules by executing the following commands in the Erlang shell, as also detailed in `README.md`:
    *   `c(matrix).`
    *   `c(matrix_server).`
    *   `c(matrix_server_supervisor).`
3.  **Starting the Server**: After successful compilation, you can start the matrix server by executing the following command in the Erlang shell, as per `README.md`:
    *   `matrix_server:start_server().`

Once these steps are completed, the server will be running and ready to accept matrix multiplication requests.
