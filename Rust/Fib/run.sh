#!/bin/bash
$(dirname -- "$(readlink -f -- "$BASH_SOURCE")")/target/release/Rust
