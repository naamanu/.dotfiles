-- Terminal-agent helpers. Notes and citations live in core/notes.lua.
local M = {}

local function project_root()
  local marker = vim.fs.root(
    0,
    { ".git", "package.json", "pyproject.toml", "Cargo.toml", "go.mod", "dune-project" }
  )
  return marker or vim.fn.getcwd()
end

local function choose_agent()
  if vim.env.DEV_AGENT and vim.env.DEV_AGENT ~= "" then
    return vim.env.DEV_AGENT
  end
  for _, candidate in ipairs({ "codex", "claude" }) do
    if vim.fn.executable(candidate) == 1 then
      return candidate
    end
  end
end

function M.agent_open()
  local agent = choose_agent()
  if not agent then
    vim.notify(
      "No terminal agent found; install codex or claude, or set DEV_AGENT",
      vim.log.levels.ERROR
    )
    return
  end
  Snacks.terminal.toggle(
    agent,
    { cwd = project_root(), win = { position = "bottom", height = 0.35 } }
  )
end

function M.agent_context(line1, line2)
  local path = vim.fn.expand("%:p")
  if path == "" then
    return vim.notify("Save the buffer before sharing context", vim.log.levels.WARN)
  end
  local relative = vim.fs.relpath(project_root(), path) or path
  local reference = string.format("Please inspect @%s:%d-%d", relative, line1, line2)
  vim.fn.setreg(vim.fn.has("clipboard") == 1 and "+" or '"', reference)
  M.agent_open()
  vim.notify("Context reference copied; paste it into the agent terminal")
end

-- Documentation for the word under the cursor (or the selection), by
-- filetype. K is LSP hover; this is the "read the real docs" step. Haskell
-- prefers a local hoogle (`cabal install hoogle && hoogle generate`).
local doc_urls = {
  haskell = "https://hoogle.haskell.org/?hoogle=%s",
  ocaml = "https://ocaml.org/packages/search?q=%s",
  python = "https://devdocs.io/#q=python %s",
  rust = "https://docs.rs/releases/search?query=%s",
  go = "https://pkg.go.dev/search?q=%s",
  typescript = "https://devdocs.io/#q=typescript %s",
  javascript = "https://devdocs.io/#q=javascript %s",
  c = "https://devdocs.io/#q=c %s",
  cpp = "https://devdocs.io/#q=cpp %s",
  sml = "https://smlfamily.github.io/Basis/manpages.html#%s",
  lua = "https://neovim.io/doc/user/lua.html#%s",
}

function M.docs_lookup(word)
  word = word or vim.fn.expand("<cword>")
  local ft = vim.bo.filetype
  if ft == "haskell" and vim.fn.executable("hoogle") == 1 then
    return Snacks.terminal.open({ "hoogle", "search", "--count=30", word }, {
      interactive = false,
      win = { position = "float", width = 0.7, height = 0.6 },
    })
  elseif ft == "racket" or ft == "scheme" then
    return vim.system({ "raco", "docs", word })
  end
  local url = doc_urls[ft] or "https://devdocs.io/#q=%s"
  vim.ui.open(url:format(vim.uri_encode and vim.uri_encode(word) or word))
end

vim.keymap.set("n", "<leader>fD", M.docs_lookup, { desc = "Docs for word (by filetype)" })
vim.keymap.set("x", "<leader>fD", function()
  local lines = vim.fn.getregion(vim.fn.getpos("v"), vim.fn.getpos("."))
  M.docs_lookup(vim.trim(table.concat(lines, " ")))
end, { desc = "Docs for selection" })

vim.keymap.set("n", "<leader>aa", M.agent_open, { desc = "Open terminal agent" })
vim.keymap.set("n", "<leader>ac", function()
  M.agent_context(vim.fn.line("."), vim.fn.line("."))
end, { desc = "Copy agent context" })
vim.keymap.set("x", "<leader>ac", function()
  M.agent_context(vim.fn.line("'<"), vim.fn.line("'>"))
end, { desc = "Copy agent context" })

return M
