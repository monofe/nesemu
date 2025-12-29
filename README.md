# About

This is a basic (unfinished) emulator for the Nintendo Entertainment System.

Currently there is no support for audio and only support for Mapper 0 games.


# Usage Guide

## Install

1. Install using Cargo

```sh
cargo install --git https://github.com/monofe/nesemu
```

2. Run and pass file to be interpreted as first argument

```sh
nesemu <file>
```

## Uninstall

Uninstall using Cargo

```sh
cargo uninstall nesemu
```

## Build from Source

1. Clone the repository

```sh
git clone https://github.com/monofe/nesemu
```

2. Build using Cargo

```sh
cargo build --release
```

Executable located in target/release

3. Run and pass file to be interpreted as the first argument

```sh
./<path-to-executable> <file>
```

To build and interpret a file use -

```sh
cargo run --release -- <file>
```