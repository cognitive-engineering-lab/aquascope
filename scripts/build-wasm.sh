cargo build --release --target wasm32-wasi -p aquascope_front
gzip target/wasm32-wasi/release/aquascope-driver.wasm
mv target/wasm32-wasi/release/aquascope-driver.wasm.gz frontend/packages/system/src/assets/aquascope-driver.wasm.gz