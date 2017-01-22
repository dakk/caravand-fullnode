# letchain

Bitcoin fullnode OCaml implementation. 
Letchain will be usable as a standalone bitcoin node or through a library that expose all the features.


## Planned features

- zlib compression for data storage and exchange between nodes
- utxo double index (autxo, utxo of address for instant wallet import)
- full compatibility with bitcoin core nodes
- low memory and disk footprint


## Todo

- Switch to LWT & LWT_Unix for sockets
- Remove threaded code and switch to async; peers should be implemented as LWT threads;
    The only threads should be the modules: network, blockchain, database