# letchain

[![Build Status](https://travis-ci.org/dakk/letchain.svg)](https://travis-ci.org/dakk/letchain)


Bitcoin fullnode OCaml implementation. 
Letchain will be usable as a standalone bitcoin node with a REST api interface.

Letchain is directly connected to other 2 projects:
- Bitcoinml: https://github.com/dakk/bitcoinml/
- Secp256k1-ml: https://github.com/dakk/secp256k1-ml 

## Features

- Chain syncronization for Bitcoin and Bitcoin testnet
- Header-first syncronization
- Block storage
- UTXO storage
- Account storage (balance, sent, txs, utxo)
- JSON REST api interface


## Build & run

First, download the code and enter the project directory:

```bash
git clone https://github.com/dakk/letchain
cd letchain
```

To build the project, first install dependencies from `letchain.opam` file, then run:

```bash
make
./_build/install/default/bin/letchain -c XTN -p 4
```

Otherwise you can automate the process by using opam:

```bash
opam pin add letchain . -n
opam install letchain
letchain -c XTN -p 4
```


## License

```
Copyright (c) 2016-2017 Davide Gessa

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
```


## Donations

Feel free to donate bitcoin to the developer: 13TRVwiqLMveg9aPAmZgcAix5ogKVgpe4T