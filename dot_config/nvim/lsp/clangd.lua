return {
  cmd = { "clangd", "--background-index", "--clang-tidy" },
  filetypes = { "c", "cpp", "objc", "objcpp", "cuda", "proto" },
  root_markers = {
    "compile_commands.json",
    "compile_flags.txt",
    "CMakeLists.txt",
    "Makefile",
    "meson.build",
    ".git",
  },
}
