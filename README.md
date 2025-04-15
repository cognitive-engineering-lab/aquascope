# <span>Aquascope: Look Beneath the Surface of Rust</span><img src="https://user-images.githubusercontent.com/663326/219528078-e8792f31-02b3-447f-97ed-f3c0fbb4f557.png" style="float: right; border: 1px solid #555;" height="250" /></p>

[![tests](https://github.com/cognitive-engineering-lab/aquascope/actions/workflows/ci.yml/badge.svg)](https://github.com/cognitive-engineering-lab/aquascope/actions/workflows/ci.yml)
[![crates.io](https://img.shields.io/crates/v/mdbook-aquascope.svg)](https://crates.io/crates/mdbook-aquascope)
[![docs](https://img.shields.io/badge/docs-built-blue)](https://cel.cs.brown.edu//aquascope/doc/aquascope/)

Aquascope is a tool that generates interactive visualizations of Rust programs. These visualizations show how Rust's borrow checker "thinks" about a program, and how a Rust program actually executes. Here is a sample output of Aquascope:

<img alt="Example Aquascope output" src="https://user-images.githubusercontent.com/663326/219532624-a9605540-dac9-4d9b-a5f2-8a3d1ca81f50.png" height="550" />

[Click here for a live demo.](https://cel.cs.brown.edu/aquascope/) Want to learn more about what the diagram means? [Read the new ownership chapter in our Rust Book Experiment.](https://rust-book.cs.brown.edu/ch04-01-what-is-ownership.html)

⚠️🔬 **Aquascope is research software! If you encounter a bug, we welcome contributions!** 🧪⚠️

## Installation

We provide an [mdBook](https://rust-lang.github.io/mdBook/) preprocessor that embeds Aquascope diagrams into an mdBook. To use it, you need to install the `mdbook-aquascope` and `cargo-aquascope` binaries as follows.

```sh
cargo install mdbook-aquascope --locked --version 0.3.5
rustup toolchain install nightly-2024-12-15 -c rust-src -c rustc-dev -c llvm-tools-preview -c miri
cargo +nightly-2024-12-15 install aquascope_front --git https://github.com/cognitive-engineering-lab/aquascope --tag v0.3.5 --locked
cargo +nightly-2024-12-15 miri setup
```

Note that `cargo-aquascope` is installed via `aquascope_front` and must be installed via git and with a specific nightly toolchain. The `miri setup` command is a necessary prerequisite to running the Aquascope interpreter.

### From Source

If you want to install from source, you first need to install [cargo-make](https://github.com/sagiegurari/cargo-make), a Rust build tool, like this:

```
cargo install cargo-make --locked
```

Then you need to install [Depot](https://github.com/cognitive-engineering-lab/depot/), a Javascript build tool, like this:

```
curl https://raw.githubusercontent.com/cognitive-engineering-lab/depot/main/scripts/install.sh | sh
```

Then you can install Aquascope from source like this:

```sh
git clone https://github.com/cognitive-engineering-lab/aquascope.git
cd aquascope
cargo make install-mdbook
```

## Usage

First, enable `mdbook-aquascope` in your mdBook's `book.toml` like so:

```toml
# book.toml
[preprocessor.aquascope]
```

Then add an Aquascope code block to one of your Markdown source files like this:

    ```aquascope,interpreter
    #fn main() {
    let mut s = String::from("hello ");`[]`
    s.push_str("world");`[]`
    #}
    ```

Further documentation on the syntax and configuration of Aquascope blocks will be provided once the interface is more stable.

## Having trouble?

If you want to use Aquascope but are having trouble finding the relevant information, please leave an issue or email us at <wcrichto@brown.edu> and <gavinleroy@brown.edu>.

## Citation

Aquascope was developed as a part of our academic research on [how people learn Rust](https://dl.acm.org/doi/10.1145/3622841). If you use Aquascope as a part of your research, please cite this paper:

```bibtex
@article{cgk:aquascope,
  author = {Crichton, Will and Gray, Gavin and Krishnamurthi, Shriram},
  title = {A Grounded Conceptual Model for Ownership Types in Rust},
  year = {2023},
  issue_date = {October 2023},
  publisher = {Association for Computing Machinery},
  address = {New York, NY, USA},
  volume = {7},
  number = {OOPSLA2},
  url = {https://doi.org/10.1145/3622841},
  doi = {10.1145/3622841},
  journal = {Proc. ACM Program. Lang.},
  month = {oct},
  articleno = {265},
  numpages = {29},
  keywords = {Rust, concept inventory, ownership types, program state visualization}
}
```
