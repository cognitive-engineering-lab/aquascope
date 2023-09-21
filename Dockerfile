FROM rustlang/rust:nightly-alpine
ENV CARGO_UNSTABLE_SPARSE_REGISTRY=true

RUN rustup set profile minimal                                  && \
    rustup default nightly-2023-08-25-x86_64-unknown-linux-musl && \
    rustup toolchain uninstall nightly                          && \
    rustup toolchain uninstall stable                           && \
    rustup component add --toolchain nightly-2023-08-25            \
                         --target x86_64-unknown-linux-musl        \
                         rust-src rustc-dev llvm-tools-preview

RUN apk add --no-cache build-base musl-dev libc-dev linux-headers

WORKDIR /aquascope

COPY Cargo.toml rust-toolchain.toml ./
COPY crates ./crates

ENV TARGET_CC=x86_64-linux-musl-gcc
ENV RUSTFLAGS="-C target-feature=-crt-static"

RUN cargo miri setup
ENV MIRI_SYSROOT=/root/.cache/miri

RUN cargo install --path crates/aquascope_front --target x86_64-unknown-linux-musl --locked
RUN cargo clean
RUN rm -rf /aquascope

WORKDIR /app
