Welcome, stranger!

These are the libraries I extracted from my last game project. They might be of interest to
you. Read at your own risk.

If you do find a bug, I'll happilly talk to you, but otherwise, this
project is open source in the sense that you can read the code, not in the sense that I plan to
engage with people on the internet about it much.

This repository does not contain the live versions of the libraries - rather I will from time to
time manually sync this code with my projects.

Notable libraries:

- `wfprof`: An intrusive profiler with very low overhead. Does not do call attribution. Strongly
  influenced by the profiler Casey built at [Computer,
  Enhance!](https://www.computerenhance.com/). Video below.

- `wfarena`: An arena style allocator. Supports both the block based approach as well as virtual
  memory. Supports resetting to a previous checkpoint. Current version is somewhat influenced by the
  arena in [RADDebugger](https://github.com/EpicGamesExt/raddebugger).

- `wfgui`: An immediate-style GUI. Currently not very high quality. Definitely needs more
  work. Video below.

- `wfslab`: A slab array (sometimes called a slab allocator or a bucket array). An arena-friendly
  (does not reallocate) array-like datastructure with stable pointers (not that Rust appreciates
  them).

- `wfhashtrie`: An arena-friendly (does not reallocate) hash table.

- `wfflags`: A proc-macro (a piece of Rust you execute at compile time to generate code) that turns
  an enum declaration into a typesafe flags type.

- `wfserialize`: A serialization system that supports per-struct versioning. Currently only
  serializes to text, but a binary backend is on the roadmap.

- `wfinlinevec`: Just like the [arrayvec library](https://github.com/bluss/arrayvec), but you can
  reinterpret the bytes with `mem::transmute` (or a safe wrapper like `bytemuck`) without fear of LLVM.

And others.


---

The profiler (and GUI, I guess).

![](./profiler.gif)
