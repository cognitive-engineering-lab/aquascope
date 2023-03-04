FROM rustlang/rust:nightly
ENV CARGO_UNSTABLE_SPARSE_REGISTRY=true

RUN rustup default nightly-2022-12-07 && \
    rustup component add --toolchain nightly-2022-12-07 rust-src rustc-dev llvm-tools-preview

WORKDIR /aquascope

COPY Cargo.toml rust-toolchain.toml ./
COPY crates ./crates

RUN cargo miri setup
ENV MIRI_SYSROOT=/root/.cache/miri
RUN cargo install --path crates/aquascope_front

WORKDIR /app
