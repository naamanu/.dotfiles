-- Tabline: shown only with 2+ tabs. Each tab is the file in its active window,
-- with its mini.icons icon, inside slanted separators:
--
--     main.go    tasks.lua
--
-- Colors come from the colorscheme: TabLineSel (active), TabLine (inactive),
-- TabLineFill (the bar, transparent). Tabs are clickable.

local M = {}

local SEP_LEFT, SEP_RIGHT = "\u{e0ba}", "\u{e0bc}" -- lower-right / upper-left slants
local MAX, KEEP = 20, 17 -- names longer than MAX are cut to KEEP + "…"

--- Full tail name of BUF (for the icon lookup) and the label to display.
local function name_of(buf)
  if vim.bo[buf].buftype == "terminal" then
    return "terminal", "terminal"
  end
  -- Oil buffers are oil:///dir/ -- show the directory's name.
  local full = vim.fn.fnamemodify((vim.api.nvim_buf_get_name(buf):gsub("/+$", "")), ":t")
  if full == "" then
    return full, "[No Name]"
  end
  local label = full
  if vim.fn.strchars(full) > MAX then
    label = vim.fn.strcharpart(full, 0, KEEP) .. "…"
  end
  return full, label
end

local function icon_of(name)
  if not _G.MiniIcons then
    return "", nil
  end
  local ok, icon, hl = pcall(MiniIcons.get, "file", name)
  if not ok then
    return "", nil
  end
  return icon, hl
end

-- Icon highlight on the inactive tab background: the icon's own color on
-- TabLine's bg, created once per icon group and reset on :colorscheme.
local icon_hls = {}
local function inactive_icon_hl(hl)
  if not hl then
    return "TabLine"
  end
  if not icon_hls[hl] then
    local fg = vim.api.nvim_get_hl(0, { name = hl, link = false }).fg
    local bg = vim.api.nvim_get_hl(0, { name = "TabLine", link = false }).bg
    local group = "TabLineIcon" .. hl
    vim.api.nvim_set_hl(0, group, { fg = fg, bg = bg })
    icon_hls[hl] = group
  end
  return icon_hls[hl]
end

function M.render()
  local parts = {}
  local current = vim.api.nvim_get_current_tabpage()
  for i, tab in ipairs(vim.api.nvim_list_tabpages()) do
    local buf = vim.api.nvim_win_get_buf(vim.api.nvim_tabpage_get_win(tab))
    local full, name = name_of(buf)
    local icon, icon_hl = icon_of(full)
    local active = tab == current
    local body = active and "TabLineSel" or "TabLine"
    local sep = active and "TabLineSelSep" or "TabLineSep"
    -- Active: the icon takes the tab's text color (readable on the bright bg).
    local ihl = active and "TabLineSel" or inactive_icon_hl(icon_hl)
    parts[#parts + 1] = ("%%%dT%%#%s#%s%%#%s# %%#%s#%s %%#%s#%s %%#%s#%s%%T"):format(
      i,
      sep,
      SEP_LEFT,
      body,
      ihl,
      icon,
      body,
      name:gsub("%%", "%%%%"),
      sep,
      SEP_RIGHT
    )
  end
  return table.concat(parts, "%#TabLineFill# ") .. "%#TabLineFill#"
end

vim.o.showtabline = 1 -- only when there are at least two tabs
vim.o.tabline = "%!v:lua.require'naamanu.core.tabline'.render()"

vim.api.nvim_create_autocmd("ColorScheme", {
  group = vim.api.nvim_create_augroup("tabline-icons", { clear = true }),
  callback = function()
    icon_hls = {}
  end,
  desc = "Rebuild tabline icon highlights for the new colorscheme",
})

return M
