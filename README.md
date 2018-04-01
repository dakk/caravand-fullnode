# caravand

[![Build Status](https://travis-ci.org/dakk/caravand.svg)](https://travis-ci.org/dakk/caravand)
[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/dakk/caravand/blob/master/LICENSE)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://github.com/dakk/caravand/wiki)

Bitcoin fullnode OCaml implementation. 
Letchain will be usable as a standalone bitcoin node with a REST api interface.

Letchain is directly connected to other 3 projects:
- Bitcoinml: https://github.com/dakk/bitcoinml/
- Secp256k1-ml: https://github.com/dakk/secp256k1-ml 
- Letlight: https://github.com/dakk/letlight

## Features

- Chain syncronization for Bitcoin, Bitcoin testnet and Bitcoincash
- Header-first syncronization
- Block storage
- UTXO storage
- Account storage (balance, sent, txs, utxo)
- JSON REST api interface
- Prune-mode for low disk footprint
- Segregated witness
- Letchain-as-a-library for applications who need blockchain data
- Lazy evaluation for block parsing which reduces memory footprint

We are planning to integrate other cool features:
- JSON-RPC api compatible with Bitcoind
- Bloom filtering and compactblock
- Side-chain
- Pluggable script systems

## Documentation
An updated documentation is available [here](https://github.com/dakk/letchain/wiki).

## Supported chains

At the moment letchain supports these chains:
- XTN - Bitcoin testnet
- BTC - Bitcoin mainnet
- BCH - BitcoinCash mainnet

## License

```
Copyright (c) 2016-2018 Davide Gessa

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