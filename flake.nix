{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
    depot-js.url = "github:cognitive-engineering-lab/depot";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, depot-js }:
  flake-utils.lib.eachDefaultSystem (system:
  let
    overlays = [ (import rust-overlay) ];
    pkgs = import nixpkgs {
      inherit system overlays;
    };

    toolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
    depotjs = depot-js.packages.${system}.default;

    ci-check = pkgs.writeScriptBin "ci-check" ''
      cargo fmt --check &&
      cargo clippy -- -D warnings &&
      cargo insta test &&
      cd crates/mdbook-aquascope/test-book && mdbook build && cd ../../../ &&
      cd frontend && depot test && cd ..
    '';

    ci-install = pkgs.writeScriptBin "ci-install" ''
      cargo make init-bindings
      cd frontend && depot build && cd ..
      cargo install --path crates/aquascope_front --debug --locked
      cargo install --path crates/mdbook-aquascope --debug --locked
    '';

    ci-publish-crates = pkgs.writeScriptBin "ci-publish-crates" ''
      cargo build
      cargo ws publish --from-git --allow-dirty --yes --token "$1"
    '';

    ci-build-standalone = pkgs.writeScriptBin "ci-build-standalone" ''
      cd frontend && depot build
    '';

    ci-publish-full-pages = pkgs.writeScriptBin "ci-update-frontend" ''
      cargo doc --lib
      cd frontend && depot build
      mv ../target/doc ./packages/aquascope-standalone/dist/doc
    '';

    minimalFrontendDeps = [
      depotjs
      pkgs.nodejs_22
      pkgs.nodePackages.pnpm
      ci-build-standalone
    ];

  in {
    devShells = {
      # Used only for building the frontend with
      # depot and publishing the standalone site
      minimal = pkgs.mkShell {
        buildInputs = minimalFrontendDeps;
      };

      fullstack = with pkgs; mkShell {
        buildInputs = minimalFrontendDeps ++ [
          ci-check
          ci-install
          ci-publish-crates
          ci-publish-full-pages

          llvmPackages_latest.llvm
          llvmPackages_latest.lld
          libiconv

          cargo-insta
          cargo-make
          cargo-watch
          rust-analyzer

          mdbook

          toolchain
        ] ++ lib.optionals stdenv.isDarwin [
          darwin.apple_sdk.frameworks.SystemConfiguration
        ];

        shellHook = ''
          export RUSTC_PATH=$(which rustc);
          export SYSROOT=$(rustc --print sysroot)
          export MIRI_SYSROOT=$(cargo miri setup --print-sysroot)
          export DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$(rustc --print target-libdir)"
        '';

        RUSTC_LINKER = "${llvmPackages.clangUseLLVM}/bin/clang";

        # NOTE: the version of playwright-driver must match the version of
        # playwright in the embed and standalone packages.
        PLAYWRIGHT_BROWSERS_PATH="${playwright-driver.browsers}";
      };

      default = self.devShells.${system}.fullstack;
    };
  });
}
