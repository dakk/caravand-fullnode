# letchain

Bitcoin fullnode OCaml implementation. 
Letchain will be usable as a standalone bitcoin node or through a library that expose all the features.


## Planned features

- zlib compression for data storage (or flac) and exchange between nodes
- utxo double index (autxo, utxo of address for instant wallet import)
- full compatibility with bitcoin core nodes
- low memory and disk footprint
- pruned node with full utxo, address -> (balance, utxo) capabilities