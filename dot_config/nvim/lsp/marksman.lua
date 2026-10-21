-- Markdown: heading/anchor completion, document symbols, link checks for
-- relative links. Denote `denote:ID` links are URLs to it, so they are left
-- alone (core/notes.lua resolves them).
return {
  cmd = { "marksman", "server" },
  filetypes = { "markdown" },
  root_markers = { ".marksman.toml", ".git" },
}
