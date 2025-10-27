# 🌐 LanguageComparison

A collection of the same projects implemented across multiple languages.
Goal: To compare syntax, performance, idioms, tooling, and language ergonomics—while having fun and learning.

## Main Inspiration

I love programming languages and all their nuances. I have long wondered about what each language has to offer
in various ways, especially beyond its standard library, package managers, and other bundled features; I was
curious about the languages themselves syntactically and linguistically, and how these deliberate choices 
affected the overall feeling and flow of writing in each one.

I have found that languages are always evolving by building ontop of those that came before; fixing up
little things the authors thought were lacking from previous iterations, bundling in the strengths of
various languages, and throwing in their own little touch of novelty.

Eventually I want to write more languages than I already have, and this repo will serve as a reference of sorts.

---

## 🧰 Languages & Projects

<details>
  <summary><strong>Ada</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Ada/Fib/) – basic recursive fibonacci
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `gnatmake`
  > - **LSP:** `ada-language-server`
</details>
<details>
  <summary><strong>BASIC</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](BASIC/Fib/) – basic recursive fibonacci
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `fbc (freebasic)`
</details>
<details>
  <summary><strong>C</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](C/Fib/) – basic recursive fibonacci
  > - [Brainfuck](C/Brainfuck/) – simple brainfuck interpreter for hello world
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `gcc`
  > - **LSP:** `clangd`
</details>
<details>
  <summary><strong>C++</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Cpp/Lexer/) – basic lexer with static input
  > - [Fib](Cpp/Fib/) – basic recursive fibonacci
  > - [Brainfuck](Cpp/Brainfuck/) – simple brainfuck interpreter for hello world
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `g++`
  > - **LSP:** `clangd`
</details>
<details>
  <summary><strong>C#</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Csharp/Lexer/) – basic lexer with static input
  > - [Fib](Csharp/Fib/) – basic recursive fibonacci
  > - [Brainfuck](Csharp/Brainfuck/) – simple brainfuck interpreter for hello world

  > ### ⚙️ Tooling
  > - **Compiler:** `dotnet`
  > - **LSP:** `csharp-ls`
</details>
<details>
  <summary><strong>Clojure</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Clojure/Lexer/) – basic lexer with static input
  > - [Fib](Clojure/Fib/) – basic recursive fibonacci

  > ### ⚙️ Tooling
  > - **Manager:** `leiningen`
  > - **Compiler:** `lein`
  > - **LSP:** `clojure-lsp`
</details>
<details>
  <summary><strong>Common Lisp</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](CommonLisp/Fib/) – basic recursive fibonacci
  > ### ⚙️ Tooling
  > - **Compiler:** `sbcl`
</details>
<details>
  <summary><strong>Crystal</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Crystal/Fib/) – basic recursive fibonacci

  > ### ⚙️ Tooling
  > - **Compiler:** `crystal`
  > - **LSP:** `crystalline`
</details>
<details>
  <summary><strong>D</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](D/Lexer/) – basic lexer with static input
  > - [Fib](D/Fib/) – basic recursive fibonacci
  > - [Brainfuck](D/Brainfuck/) – simple brainfuck interpreter for hello world
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `dmd`
  > - **LSP:** `serve_d`
</details>
<details>
  <summary><strong>Dart</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Dart/Fib/) – basic recursive fibonacci
  > ### ⚙️ Tooling
  > - **Compiler:** `dart`
  > - **Runner:** `dart`
</details>
<details>
  <summary><strong>Elixir</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Elixir/Lexer/) – basic lexer with static input
  > - [Fib](Elixir/Fib/) – basic recursive fibonacci
  >
  > ### ⚙️ Tooling
  > - **Manager:** `mix`
  > - **Compiler:** `elixirc`
  > - **Runner:** `elixir`
  > - **Shell:** `iex`
  > - **LSP:** `elixir_ls`
</details>
<details>
  <summary><strong>Erlang</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Erlang/Lexer/) – basic lexer with static input
  > - [Fib](Erlang/Fib/) – basic recursive fibonacci
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `erlc`
  > - **Shell:** `erl`
  > - **LSP:** `erlang_ls`
</details>
<details>
  <summary><strong>F#</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Fsharp/Fib/) – basic recursive fibonacci
  > - [Brainfuck](Fsharp/Brainfuck/) – simple brainfuck interpreter for hello world
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `dotnet`
  > - **LSP:** `fsautocomplete`
</details>
<details>
  <summary><strong>Fennel</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Fennel/Fib/) – basic recursive fibonacci
  > ### ⚙️ Tooling
  > - **Runner:** `fennel || lua`
  > - **Compiler:** `fennel || luac`
</details>
<details>
  <summary><strong>Fortran</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Fortran/Fib/) – basic recursive fibonacci
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `gfortran`
  > - **LSP:** `fortls`
</details>
<details>
  <summary><strong>Go</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Go/Lexer/) – basic lexer with static input
  > - [Fib](Go/Fib/) – basic recursive fibonacci
  > - [Brainfuck](Go/Brainfuck/) – simple brainfuck interpreter for hello world
  >
  > ### ⚙️ Tooling
  > - **Runner:** `go`
  > - **LSP:** `gopls`
</details>
<details>
  <summary><strong>Haskell</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Haskell/Lexer/) – basic lexer with static input
  > - [Fib](Haskell/Fib/) – basic recursive fibonacci
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `ghc`
  > - **Shell:** `ghci`
  > - **LSP:** `hls`
</details>
<details>
  <summary><strong>Java</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Java/Lexer/) – basic lexer with static input
  > - [Fib](Java/Fib/) – basic recursive fibonacci
  > - [Brainfuck](Java/Brainfuck/) – simple brainfuck interpreter for hello world
  >
  > ### ⚙️ Tooling
  > - **Manager:** `maven`
  > - **Compiler:** `mvn`
  > - **LSP:** `jdtls`
</details>
<details>
  <summary><strong>Julia</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Julia/Fib/) – basic recursive fibonacci
  > ### ⚙️ Tooling
  >- **Runner:** `julia`
</details>
<details>
  <summary><strong>Koka</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Koka/Fib/) – basic recursive fibonacci
  > ### ⚙️ Tooling
  >- **Compiler:** `koka`
</details>
<details>
  <summary><strong>Kotlin</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Kotlin/Fib/) – basic recursive fibonacci
  > ### ⚙️ Tooling
  >- **Compiler:** `kotlinc`
  >- **Runner:** `java`
  >- **LSP:** `kotlin-language-server`
</details>
<details>
  <summary><strong>Lua</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Lua/Lexer/) – basic lexer with static input
  > - [Fib](Lua/Fib/) – basic recursive fibonacci
  > - [Brainfuck](Lua/Brainfuck/) – simple brainfuck interpreter for hello world
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `luac`
  > - **Runner:** `lua`
  > - **LSP:** `lua_ls`
</details>
<details>
  <summary><strong>Nim</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Nim/Lexer/) – basic lexer with static input
  > - [Fib](Nim/Fib/) – basic recursive fibonacci
  > - [Brainfuck](Nim/Brainfuck/) – simple brainfuck interpreter for hello world
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `nim`
  > - **Runner:** `nim`
  > - **LSP:** `nim_langserver`
</details>
<details>
  <summary><strong>OCaml</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Ocaml/Lexer/) – basic lexer with static input
  > - [Fib](Ocaml/Fib/) – basic recursive fibonacci
  > - [Brainfuck](Ocaml/Brainfuck/) – simple brainfuck interpreter for hello world
  >
  > ### ⚙️ Tooling
  > - **Manager:** `dune`
  > - **Compiler:** `ocamlc`
  > - **Runner:** `ocaml`
  > - **LSP:** `ocamllsp`
</details>
<details>
  <summary><strong>Odin</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Odin/Lexer/) – basic lexer with static input
  > - [Fib](Odin/Fib/) – basic recursive fibonacci
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `odin`
  > - **Runner:** `odin`
  > - **LSP:** `ols`
</details>
<details>
  <summary><strong>Pascal</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Pascal/Fib/) – basic recursive fibonacci
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `fpc`
</details>
<details>
  <summary><strong>Perl</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Perl/Fib/) – basic recursive fibonacci
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `pp`
  > - **LSP:** `perlnavigator`
</details>
<details>
  <summary><strong>PHP</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](PHP/Fib/) – basic recursive fibonacci
  >
  > ### ⚙️ Tooling
  > - **Runner:** `php`
  > - **LSP:** `phpactor`
</details>
<details>
  <summary><strong>Python</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Python/Lexer/) – basic lexer with static input
  > - [Fib](Python/Fib/) – basic recursive fibonacci
  > - [Brainfuck](Python/Brainfuck/) – simple brainfuck interpreter for hello world
  >
  > ### ⚙️ Tooling
  > - **Runner:** `python`
  > - **LSP:** `based_pyright`
</details>
<details>
  <summary><strong>R</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](R/Fib/) – basic recursive fibonacci
  >
  > ### ⚙️ Tooling
  > - **Runner:** `R`
  > - **Compiler:** `R`
</details>
<details>
  <summary><strong>Raku</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Raku/Fib/) – basic recursive fibonacci
  > ### ⚙️ Tooling
  > - **Runner:** `raku`
  > - **Compiler:** `rakudo`
</details>
<details>
  <summary><strong>Red/Rebol</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Red/Fib/) – basic recursive fibonacci
  > ### ⚙️ Tooling
  > - **Runner:** `red`
</details>
<details>
  <summary><strong>ReScript</strong></summary>

  > ### 🗂️ Projects
  > ### ⚙️ Tooling
</details>
<details>
  <summary><strong>Ruby</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Ruby/Fib/) – basic recursive fibonacci
  >
  > ### ⚙️ Tooling
  > - **Runner:** `ruby`
  > - **LSP:** `ruby-lsp`
</details>
<details>
  <summary><strong>Rust</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Rust/Lexer/) – basic lexer with static input
  > - [Brainfuck](Rust/Brainfuck/) – simple brainfuck interpreter for hello world
  > - [ShuntingYard](Rust/ShuntingYard/) – shunting yard implementation
  > - [Conway's Game Of Life](Rust/ConwayGameOfLife/) – conway's game of life with ncurses
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `cargo`
  > - **Runner:** `cargo`
  > - **LSP:** `rust-analyzer`
  > - **Extra** `rust-script` - allows shebang for running single files
</details>
<details>
  <summary><strong>Scala</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Scala/Fib/) – basic recursive fibonacci
  >
  > ### ⚙️ Tooling
  > - **Runner:** `scala`
  > - **Compiler:** `scalac`
</details>
<details>
  <summary><strong>StandardML</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](StandardML/Fib/) – basic recursive fibonacci
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `mlton`
  > - **LSP:** `millet`
</details>
<details>
  <summary><strong>Swift</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Swift/Fib/) – basic recursive fibonacci
  > ### ⚙️ Tooling
  > - **Compiler:** `swift`
  > - **Runner:** `swift`
</details>
<details>
  <summary><strong>Tcl</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Tcl/Fib/) – basic recursive fibonacci
  > ### ⚙️ Tooling
  > - **Runner:** `tclsh`
</details>
<details>
  <summary><strong>Typescript</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Typescript/Lexer/) – basic lexer with static input
  > - [Brainfuck](Typescript/Brainfuck/) – simple brainfuck interpreter for hello world
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `npx tsc`
  > - **Runner:** `npx tsx`
  > - **LSP:** `typescript-language-server`
</details>
<details>
  <summary><strong>V</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](V/Fib/) – basic recursive fibonacci
  > ### ⚙️ Tooling
  > - **Compiler:** `v`
</details>
<details>
  <summary><strong>Vale</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Vale/Fib/) – basic recursive fibonacci
  > ### ⚙️ Tooling
  > - **Compiler:** `valec`
</details>
<details>
  <summary><strong>Wren</strong></summary>

  > ### 🗂️ Projects
  > - [Fib](Wren/Fib/) – basic recursive fibonacci
  > ### ⚙️ Tooling
  > - **Runner:** `wren`
</details>
<details>
  <summary><strong>Zig</strong></summary>

  > ### 🗂️ Projects
  > - [Lexer](Zig/Lexer/) – basic lexer with static input
  > - [Brainfuck](Zig/Brainfuck/) – simple brainfuck interpreter for hello world
  >
  > ### ⚙️ Tooling
  > - **Compiler:** `zig`
  > - **Runner:** `zig`
  > - **LSP:** `zls`
</details>
