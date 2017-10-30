## Build & run

First, download the code and enter the project directory:

```bash
git clone https://github.com/dakk/letchain
cd letchain
```

To build the project, first install dependencies from `letchain.opam` file, then run:

```bash
make
./_build/install/default/bin/letchaind -c XTN -p 4
```

Otherwise you can automate the process by using opam:

```bash
opam pin add letchain . -n
opam install letchain
letchaind -c XTN -p 4
```