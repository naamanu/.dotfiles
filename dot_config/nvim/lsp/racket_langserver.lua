-- Racket (racket-langserver, installed with `raco pkg install racket-langserver`;
-- Emacs uses racket-mode's own back end). core/lsp.lua gates on the package.
return {
  cmd = { "racket", "--lib", "racket-langserver" },
  filetypes = { "racket", "scheme" },
  root_markers = { "info.rkt", ".git" },
}
