-- UI plugins (installed by core/plugins.lua): icons, the floating cmdline,
-- statusline, per-window file tags, small highlights, oil and snacks.

local map = function(mode, lhs, rhs, desc)
  vim.keymap.set(mode, lhs, rhs, { desc = desc })
end

-- oil replaces netrw as the directory browser.
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- --- Icons ------------------------------------------------------------------

-- Set up before the plugins that draw icons (snacks picker, oil). The mock
-- answers require("nvim-web-devicons") with mini.icons, so plugins that only
-- know devicons get the same icons without installing it. Glyphs come from
-- Symbols Nerd Font Mono, Ghostty's fallback font.
require("mini.icons").setup()
MiniIcons.mock_nvim_web_devicons()

-- --- tiny-cmdline -----------------------------------------------------------

-- The ui2 cmdline (options.lua: cmdheight=0, ui2) as a floating box,
-- centered left to right and 20% from the top. / and ? stay at the bottom,
-- so the search pattern and count read in the cmdline.
require("tiny-cmdline").setup({
  position = { x = "50%", y = "20%" },
})

-- --- Lualine ----------------------------------------------------------------

-- One global bar (laststatus=3), theme derived from the colorscheme.
--   a mode | b branch | c project root (when not the cwd), icon, path
--   x macro register, diagnostics, git diff | y progress | z line:column

-- Project root: nearest ancestor holding any of these.
local root_markers =
  { ".git", "lua", "package.json", "Makefile", "Cargo.toml", "go.mod", "pyproject.toml" }

local function project_root()
  return vim.fs.root(0, root_markers)
end

-- Root folder name, only when it is not the cwd (cwd is already in the title
-- and the shell prompt).
local function root_name()
  local root = project_root()
  if not root or vim.fs.normalize(root) == vim.fs.normalize(vim.fn.getcwd()) then
    return ""
  end
  return vim.fs.basename(root)
end

-- File path relative to the project root (or cwd), ~ for home, with
-- modified/readonly marks. Special buffers show their name or filetype.
local function pretty_path()
  local buf = vim.api.nvim_get_current_buf()
  local name = vim.api.nvim_buf_get_name(buf)
  if vim.bo[buf].buftype ~= "" or name == "" then
    return name ~= "" and vim.fs.basename((name:gsub("/+$", ""))) or "[No Name]"
  end
  local base = project_root() or vim.fn.getcwd()
  local path = vim.fs.relpath(base, name) or vim.fn.fnamemodify(name, ":~")
  if vim.bo[buf].modified then
    path = path .. " ●"
  end
  if vim.bo[buf].readonly or not vim.bo[buf].modifiable then
    path = path .. " "
  end
  return path
end

local function macro()
  local reg = vim.fn.reg_recording()
  return reg ~= "" and ("recording @" .. reg) or ""
end

-- Diff counts from gitsigns (already computed for the sign column).
local function git_diff()
  local d = vim.b.gitsigns_status_dict
  if d then
    return { added = d.added, modified = d.changed, removed = d.removed }
  end
end

require("lualine").setup({
  options = {
    theme = "auto",
    globalstatus = true,
    component_separators = "",
    section_separators = { left = "\u{e0bc}", right = "\u{e0ba}" }, -- the tabline's slants
    -- Redraw on cursor movement as well as the usual buffer/window events.
    refresh = {
      events = {
        "WinEnter",
        "BufEnter",
        "BufWritePost",
        "SessionLoadPost",
        "FileChangedShellPost",
        "VimResized",
        "Filetype",
        "CursorMoved",
        "CursorMovedI",
        "ModeChanged",
        "RecordingEnter",
      },
    },
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch" },
    lualine_c = {
      { root_name, color = { gui = "bold" } },
      { "filetype", icon_only = true, padding = { left = 1, right = 0 } },
      { pretty_path, padding = { left = 0, right = 1 } },
    },
    lualine_x = {
      { macro, color = "WarningMsg" },
      "diagnostics",
      { "diff", source = git_diff },
    },
    lualine_y = { "progress" },
    lualine_z = {
      function()
        local pos = vim.api.nvim_win_get_cursor(0)
        return ("%d:%d"):format(pos[1], pos[2] + 1)
      end,
    },
  },
})

-- RecordingLeave fires while reg_recording() still names the register, so
-- refresh just after it, or "recording @q" would linger until the next move.
vim.api.nvim_create_autocmd("RecordingLeave", {
  group = vim.api.nvim_create_augroup("lualine-macro", { clear = true }),
  callback = function()
    vim.defer_fn(function()
      require("lualine").refresh({ place = { "statusline" } })
    end, 50)
  end,
  desc = "Clear the macro indicator from lualine",
})

-- --- Incline ----------------------------------------------------------------

-- With one global statusline each split still needs to say which file it
-- holds: a floating tag (icon, name, "[+]" when unsaved) in every window's
-- top-right corner, hidden while the cursor is on that line.
require("incline").setup({
  window = {
    placement = { vertical = "top", horizontal = "right" },
    margin = { vertical = 0, horizontal = 1 },
    padding = 1,
  },
  hide = { cursorline = true },
  render = function(props)
    local name = vim.api.nvim_buf_get_name(props.buf)
    name = name ~= "" and vim.fs.basename((name:gsub("/+$", ""))) or "[No Name]"
    local icon, icon_hl = "", nil
    if _G.MiniIcons then
      icon, icon_hl = MiniIcons.get("file", name)
    end
    return {
      { icon, group = props.focused and icon_hl or "InclineNormalNC" },
      " ",
      { name, gui = props.focused and "bold" or nil },
      vim.bo[props.buf].modified and { " [+]", group = "InclineModified" } or "",
    }
  end,
})

-- --- Small highlights ---------------------------------------------------------

-- TODO, FIXME, HACK, WARN, PERF, NOTE, TEST in comments, with the defaults.
require("todo-comments").setup({})

-- Colors in the text painted as their own background: hex, rgb(), hsl(),
-- ANSI escapes and CSS var(--x) usages. Tailwind classes are left alone.
require("nvim-highlight-colors").setup({
  render = "background",
  enable_hex = true,
  enable_short_hex = true,
  enable_rgb = true,
  enable_hsl = true,
  enable_ansi = true,
  enable_var_usage = true,
  enable_named_colors = false,
  enable_tailwind = false,
})

-- --- Oil --------------------------------------------------------------------

-- Replaces netrw for directories (`nvim .`, :e dir, `-`), dotfiles shown.
require("oil").setup({
  default_file_explorer = true,
  columns = { "icon", "permissions", "size", "mtime" },
  view_options = { show_hidden = true },
  -- Renames/moves ask the LSP (workspace/willRenameFiles) to update imports;
  -- the buffers it edits are saved automatically. 3s: gopls can be slow.
  lsp_file_methods = { enabled = true, timeout_ms = 3000, autosave_changes = true },
  -- Always a centered float, with a live preview of the entry under the
  -- cursor on the right (preview_win.update_on_cursor_moved is on by default).
  float = { padding = 2, max_width = 0.85, max_height = 0.8, preview_split = "right" },
  keymaps = { q = { "actions.close", mode = "n" } }, -- also <C-c>
})

local function oil_float(dir)
  require("oil").open_float(dir, { preview = {} })
end
map("n", "-", function()
  oil_float()
end, "File explorer (parent directory)")

-- `nvim .`, `:e dir` and a bare `:Oil` put oil in a normal window. Move it into
-- the float, leaving the previous buffer (or an empty one) behind.
vim.api.nvim_create_autocmd("BufWinEnter", {
  group = vim.api.nvim_create_augroup("oil-always-float", { clear = true }),
  callback = function(ev)
    if vim.bo[ev.buf].filetype ~= "oil" then
      return
    end
    local win = vim.api.nvim_get_current_win()
    if vim.api.nvim_win_get_config(win).relative ~= "" then
      return -- already the float
    end
    vim.schedule(function()
      if not vim.api.nvim_win_is_valid(win) or vim.api.nvim_win_get_buf(win) ~= ev.buf then
        return
      end
      local dir = require("oil").get_current_dir(ev.buf)
      vim.api.nvim_win_call(win, function()
        local alt = vim.fn.bufnr("#")
        if alt > 0 and alt ~= ev.buf and vim.bo[alt].buflisted then
          vim.cmd.buffer(alt)
        else
          vim.cmd.enew()
        end
      end)
      oil_float(dir)
    end)
  end,
  desc = "Open oil as a centered float",
})

-- autosave_changes only covers servers that answer willRenameFiles. Others
-- (vtsls) update imports after the move, from didRenameFiles, by sending a
-- workspace/applyEdit -- which would leave those buffers edited but unsaved.
-- For 5s after an oil move/rename, save the buffers such an edit touches,
-- but only ones that had no unsaved changes of their own.
local rename_until = 0
vim.api.nvim_create_autocmd("User", {
  pattern = "OilActionsPost",
  group = vim.api.nvim_create_augroup("oil-lsp-rename-save", { clear = true }),
  callback = function(ev)
    for _, action in ipairs(ev.data and ev.data.actions or {}) do
      if action.type == "move" then
        rename_until = vim.uv.now() + 5000
        return
      end
    end
  end,
  desc = "Open a window for saving LSP import edits after an oil rename",
})
local apply_edit = vim.lsp.handlers["workspace/applyEdit"]
vim.lsp.handlers["workspace/applyEdit"] = function(err, params, ctx, config)
  if vim.uv.now() > rename_until or not (params and params.edit) then
    return apply_edit(err, params, ctx, config)
  end
  local uris = vim.tbl_keys(params.edit.changes or {})
  for _, change in ipairs(params.edit.documentChanges or {}) do
    if change.textDocument then
      uris[#uris + 1] = change.textDocument.uri
    end
  end
  local clean = {}
  for _, uri in ipairs(uris) do
    local buf = vim.uri_to_bufnr(uri)
    clean[buf] = not (vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].modified)
  end
  local result = apply_edit(err, params, ctx, config)
  for buf, was_clean in pairs(clean) do
    if was_clean and vim.api.nvim_buf_is_loaded(buf) and vim.bo[buf].modified then
      vim.api.nvim_buf_call(buf, function()
        vim.cmd("silent noautocmd update")
      end)
    end
  end
  return result
end

-- --- Snacks ---------------------------------------

require("snacks").setup({
  picker = {
    enabled = true,
    ui_select = true, -- code actions and other vim.ui.select prompts
    -- Dotfiles are real project files: files and grep include hidden paths.
    -- .git/ itself stays out (snacks excludes it), and so do gitignored files.
    sources = {
      files = { hidden = true },
      grep = { hidden = true },
      grep_word = { hidden = true },
    },
  },
  terminal = { enabled = true },
  -- vim.ui.input (LSP rename, prompts) as a small float like the pickers.
  input = { enabled = true },
  -- Files over 1.5MB (or with very long lines) open without treesitter,
  -- LSP, folds and the like, so a minified bundle or a huge log stays fast.
  bigfile = { enabled = true },
  -- Indent scope as a text object (ii / ai) and jumps to its edges ([i / ]i).
  scope = { enabled = true },
  notifier = { enabled = true }, -- vim.notify as popups (top right)
  indent = { enabled = true }, -- indent guides + current scope
  statuscolumn = { enabled = true }, -- marks/signs left, folds/git right
  -- Every occurrence of the word under the cursor, from LSP document
  -- highlights (so only in buffers with a language server).
  words = { enabled = true },
  scroll = { enabled = false }, -- no smooth scrolling
})

-- ]] / [[ jump between those occurrences. The go, python, lua, c and markdown
-- ftplugins map ]] / [[ buffer-locally (section motions), which would win over
-- a global map, so they are also set per buffer once an LSP attaches (after
-- the ftplugin ran).
local function words_jump_keys(buf)
  local opts = function(desc)
    return { buffer = buf, desc = desc }
  end
  vim.keymap.set({ "n", "x", "o" }, "]]", function()
    Snacks.words.jump(vim.v.count1)
  end, opts("Next reference"))
  vim.keymap.set({ "n", "x", "o" }, "[[", function()
    Snacks.words.jump(-vim.v.count1)
  end, opts("Previous reference"))
end
words_jump_keys(nil)
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("snacks-words-keys", { clear = true }),
  callback = function(ev)
    words_jump_keys(ev.buf)
  end,
  desc = "]] / [[ jump between LSP references in this buffer",
})

local P = function(name, opts)
  return function()
    Snacks.picker[name](opts)
  end
end
-- The everyday pickers on short keys:
map("n", "<leader>/", P("grep"), "Live grep (incl. hidden)")
map("n", "<leader>,", P("buffers"), "Open buffers")
map("n", "<leader>'", P("resume"), "Reopen last picker")
map("n", "<leader>xx", P("diagnostics"), "Diagnostics (all buffers)")
map("n", "<leader>xb", P("diagnostics_buffer"), "Diagnostics (this buffer)")
map("n", "<leader>n", P("notifications"), "Notification history")
map("n", "<leader>gh", P("git_diff"), "Git hunks")

map("n", "<leader>ff", P("files"), "Find files")
map("n", "<leader>fg", P("grep"), "Grep (incl. hidden)")
map({ "n", "x" }, "<leader>fc", P("grep_word"), "Grep word/selection")
map("n", "<leader>fb", P("buffers"), "Buffers")
map("n", "<leader>fr", P("recent"), "Recent files")
map("n", "<leader>f/", P("lines"), "Search buffer")
map("n", "<leader>f.", P("resume"), "Resume last picker")
map("n", "<leader>fh", P("help"), "Help tags")
map("n", "<leader>fk", P("keymaps"), "Keymaps")
map("n", "<leader>fd", P("diagnostics"), "Diagnostics")
map("n", "<leader>fs", P("lsp_symbols"), "Document symbols")
map("n", "<leader>fS", P("lsp_workspace_symbols"), "Workspace symbols")
map("n", "<leader>gi", P("gh_issue"), "GitHub issues")
map("n", "<leader>gP", P("gh_pr"), "GitHub pull requests")
map({ "n", "x" }, "<leader>gB", function()
  Snacks.gitbrowse()
end, "Open file/selection on GitHub")
map("n", "<leader>gg", function()
  Snacks.lazygit()
end, "LazyGit")
map({ "n", "t" }, "<C-/>", function()
  Snacks.terminal.toggle(nil, { win = { position = "right", width = 0.4 } })
end, "Terminal (toggle)")
