FROM rust:latest

RUN rustup default nightly-2022-09-12 && \
    rustup component add --toolchain nightly-2022-09-12 rust-src rustc-dev llvm-tools-preview

WORKDIR /aquascope

COPY crates/aquascope ./aquascope
COPY crates/aquascope_front ./aquascope_front

RUN cargo install --path ./aquascope_front

WORKDIR /app
