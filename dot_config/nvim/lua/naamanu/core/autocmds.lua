local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

-- Highlight when yanking text
autocmd("TextYankPost", {
  group = augroup("highlight-yank", { clear = true }),
  callback = function()
    vim.hl.on_yank({ higroup = "Visual", timeout = 200 })
  end,
  desc = "Highlight when yanking text",
})

-- Close some filetypes with <q>
autocmd("FileType", {
  group = augroup("close-with-q", { clear = true }),
  pattern = {
    "help",
    "lspinfo",
    "man",
    "qf",
    "query",
    "checkhealth",
  },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set("n", "q", "<cmd>close<CR>", { buffer = event.buf, silent = true })
  end,
  desc = "Close certain filetypes with q",
})

-- Auto-create parent directories when saving
autocmd("BufWritePre", {
  group = augroup("auto-create-dir", { clear = true }),
  callback = function(event)
    if event.match:match("^%w%w+://") then
      return
    end
    local file = vim.uv.fs_realpath(event.match) or event.match
    vim.fn.mkdir(vim.fn.fnamemodify(file, ":p:h"), "p")
  end,
  desc = "Auto-create parent directories when saving",
})

-- Reopen a file where it was left (the '"' mark), except in commit messages
autocmd("BufReadPost", {
  group = augroup("restore-cursor", { clear = true }),
  callback = function(event)
    local buf = event.buf
    local ft = vim.bo[buf].filetype
    if ft == "gitcommit" or ft == "gitrebase" or vim.b[buf].restored_cursor then
      return
    end
    vim.b[buf].restored_cursor = true
    local mark = vim.api.nvim_buf_get_mark(buf, '"')
    if mark[1] > 0 and mark[1] <= vim.api.nvim_buf_line_count(buf) then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
  desc = "Restore cursor position",
})

-- Reload buffers changed on disk (pairs with 'autoread' in options.lua)
autocmd({ "FocusGained", "TermClose", "TermLeave" }, {
  group = augroup("checktime", { clear = true }),
  callback = function()
    if vim.o.buftype ~= "nofile" then
      vim.cmd.checktime()
    end
  end,
  desc = "Check for files changed outside Neovim",
})

-- Per-filetype indentation lives in after/ftplugin/<ft>.lua.
