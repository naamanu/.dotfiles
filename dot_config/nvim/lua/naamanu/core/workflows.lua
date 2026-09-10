local M = {}

local function project_root()
  local marker = vim.fs.root(
    0,
    { ".git", "package.json", "pyproject.toml", "Cargo.toml", "go.mod", "dune-project" }
  )
  return marker or vim.fn.getcwd()
end

local function notes_dir()
  local configured = vim.env.NOTES_DIR
  return vim.fs.normalize(configured and configured ~= "" and configured or "~/notes")
end

local function bibliography()
  local configured = vim.env.BIBLIOGRAPHY
  return vim.fs.normalize(
    configured and configured ~= "" and configured or (notes_dir() .. "/references.bib")
  )
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

function M.note_new()
  vim.ui.input({ prompt = "Note title: " }, function(title)
    if not title or title == "" then
      return
    end
    local slug = title:lower():gsub("[^%w]+", "-"):gsub("^-", ""):gsub("-$", "")
    local id = os.date("%Y%m%dT%H%M%S")
    local dir = notes_dir()
    vim.fn.mkdir(dir, "p")
    local path = string.format("%s/%s--%s.md", dir, id, slug)
    vim.cmd.edit(vim.fn.fnameescape(path))
    vim.api.nvim_buf_set_lines(0, 0, -1, false, {
      "---",
      'title: "' .. title:gsub('"', '\\"') .. '"',
      "date: " .. os.date("%Y-%m-%d"),
      "identifier: " .. id,
      "tags: []",
      "---",
      "",
      "# " .. title,
      "",
    })
  end)
end

function M.note_find()
  vim.fn.mkdir(notes_dir(), "p")
  Snacks.picker.files({ cwd = notes_dir(), hidden = false })
end

function M.note_search()
  vim.fn.mkdir(notes_dir(), "p")
  Snacks.picker.grep({ cwd = notes_dir() })
end

function M.note_link()
  local files = vim.fn.globpath(notes_dir(), "*.md", false, true)
  vim.ui.select(files, {
    prompt = "Link note:",
    format_item = function(path)
      return vim.fn.fnamemodify(path, ":t")
    end,
  }, function(path)
    if not path then
      return
    end
    local name = vim.fn.fnamemodify(path, ":t:r")
    local id, title = name:match("^(%d+)%-%-(.+)$")
    if not id then
      return
    end
    title = title:gsub("-", " ")
    vim.api.nvim_put(
      { string.format("[%s](%s)", title, vim.fn.fnamemodify(path, ":t")) },
      "c",
      true,
      true
    )
  end)
end

function M.note_backlinks()
  local id = vim.fn.expand("%:t"):match("^(%d+)%-%-")
  if not id then
    return vim.notify("Current file has no Denote identifier", vim.log.levels.WARN)
  end
  Snacks.picker.grep({ cwd = notes_dir(), search = id })
end

function M.citation_insert()
  local file = bibliography()
  if vim.fn.filereadable(file) == 0 then
    return vim.notify("Bibliography not found: " .. file, vim.log.levels.WARN)
  end
  local keys = {}
  for _, line in ipairs(vim.fn.readfile(file)) do
    local key = line:match("^@[%w_%-]+%s*{%s*([^,]+)")
    if key then
      table.insert(keys, key)
    end
  end
  vim.ui.select(keys, { prompt = "Citation:" }, function(key)
    if key then
      vim.api.nvim_put({ "[@" .. key .. "]" }, "c", true, true)
    end
  end)
end

vim.keymap.set("n", "<leader>aa", M.agent_open, { desc = "Open terminal agent" })
vim.keymap.set("n", "<leader>ac", function()
  M.agent_context(vim.fn.line("."), vim.fn.line("."))
end, { desc = "Copy agent context" })
vim.keymap.set("x", "<leader>ac", function()
  M.agent_context(vim.fn.line("'<"), vim.fn.line("'>"))
end, { desc = "Copy agent context" })
vim.keymap.set("n", "<leader>nn", M.note_new, { desc = "New note" })
vim.keymap.set("n", "<leader>nf", M.note_find, { desc = "Find note" })
vim.keymap.set("n", "<leader>ns", M.note_search, { desc = "Search notes" })
vim.keymap.set("n", "<leader>nl", M.note_link, { desc = "Link note" })
vim.keymap.set("n", "<leader>nb", M.note_backlinks, { desc = "Note backlinks" })
vim.keymap.set("n", "<leader>nc", M.citation_insert, { desc = "Insert citation" })

return M
