-- Denote-compatible notes, shared with Emacs (~/.emacs.d/elisp/notes.el:
-- denote, denote-file-type 'markdown-yaml, denote-journal, citar-denote).
-- Files written here must be indistinguishable from ones Emacs creates:
--   ID--title-slug__tag1_tag2.md
-- with Denote's YAML front matter and `[title](denote:ID)` links, so either
-- editor can rename, link and backlink them. Slug rules follow
-- denote-sluggify-title / denote-sluggify-keyword (denote 4.x).
local M = {}

function M.dir()
  local configured = vim.env.NOTES_DIR
  return vim.fs.normalize(configured and configured ~= "" and configured or "~/notes")
end

function M.bibliography()
  local configured = vim.env.BIBLIOGRAPHY
  return vim.fs.normalize(
    configured and configured ~= "" and configured or (M.dir() .. "/references.bib")
  )
end

-- denote-journal-directory default: a `journal` subdirectory.
local function journal_dir()
  return M.dir() .. "/journal"
end

-- Mirrors denote-known-keywords in ~/.emacs.d/elisp/notes.el; tags already
-- used in file names are offered too.
local KNOWN_KEYWORDS = {
  "emacs",
  "typescript",
  "rust",
  "ocaml",
  "haskell",
  "racket",
  "sml",
  "python",
  "ml",
  "c",
  "sql",
  "systems",
  "research",
  "project",
  "reading",
}

local function strip_curly_quotes(str)
  return (str:gsub("‘", ""):gsub("’", ""):gsub("“", ""):gsub("”", ""))
end

--- denote-sluggify-title: drop punctuation, hyphenate spaces/underscores,
--- collapse and trim hyphens, downcase.
function M.slug_title(str)
  str = strip_curly_quotes(str):gsub("[%[%]{}!@#%$%%%^&%*%(%)%+'\"%?,%.|;:~`/=]", "")
  str = str:gsub("[_%s]+", "-"):gsub("%-%-+", "-"):gsub("^%-", ""):gsub("%-$", "")
  return (str:lower():gsub("[=@_]+$", ""))
end

--- denote-sluggify-keyword: one word, no separators at all.
function M.slug_keyword(str)
  return (strip_curly_quotes(str):gsub("[%[%]{}!@#%$%%%^&%*%(%)%+'\"%?,%.|;:~`/_ =%-]", ""):lower())
end

-- Emacs `%S` of a string: double quotes, with `"` and `\` escaped.
local function elisp_string(str)
  return '"' .. str:gsub("\\", "\\\\"):gsub('"', '\\"') .. '"'
end

-- ISO 8601 with a colon in the offset (+02:00), as Denote writes `date:`.
local function iso_date(time)
  local offset = os.date("%z", time)
  return os.date("%Y-%m-%dT%H:%M:%S", time) .. offset:sub(1, 3) .. ":" .. offset:sub(4, 5)
end

local function all_notes()
  return vim.fn.globpath(M.dir(), "**/*.md", false, true)
end

--- Parse a Denote file name into { id, slug, tags }.
function M.parse_name(path)
  local name = vim.fn.fnamemodify(path, ":t:r")
  local id = name:match("^(%d%d%d%d%d%d%d%dT%d%d%d%d%d%d)")
  if not id then
    return nil
  end
  local tags = {}
  local tag_part = name:match("__([%w_]+)$")
  if tag_part then
    for tag in tag_part:gmatch("[^_]+") do
      tags[#tags + 1] = tag
    end
  end
  return { id = id, slug = name:match("%-%-([^_]+)") or "", tags = tags }
end

local function front_matter_value(lines, key)
  for i, line in ipairs(lines) do
    if line == "---" and i > 1 then
      break
    end
    local value = line:match("^" .. key .. ":%s*(.-)%s*$")
    if value then
      return value
    end
  end
end

--- Title of a note: the front matter `title:` if readable, else the slug.
function M.title(path)
  local ok, lines = pcall(vim.fn.readfile, path, "", 8)
  local value = ok and front_matter_value(lines, "title")
  if value and value ~= "" then
    return (value:gsub('^"(.*)"$', "%1"):gsub('\\"', '"'))
  end
  local parsed = M.parse_name(path)
  return parsed and parsed.slug:gsub("-", " ") or vim.fn.fnamemodify(path, ":t:r")
end

local function known_tags()
  local seen, tags = {}, {}
  local function add(tag)
    if not seen[tag] then
      seen[tag] = true
      tags[#tags + 1] = tag
    end
  end
  for _, tag in ipairs(KNOWN_KEYWORDS) do
    add(tag)
  end
  for _, path in ipairs(all_notes()) do
    local parsed = M.parse_name(path)
    for _, tag in ipairs(parsed and parsed.tags or {}) do
      add(tag)
    end
  end
  table.sort(tags)
  return tags
end

--- customlist completion for the tag prompt (completes the last word).
function M.complete_tags(lead)
  return vim.tbl_filter(function(tag)
    return vim.startswith(tag, lead)
  end, known_tags())
end

-- Space- or comma-separated tags, sluggified, de-duplicated and sorted
-- (denote-sort-keywords defaults to t).
local function parse_tags(input)
  local seen, tags = {}, {}
  for word in (input or ""):gmatch("[^,%s]+") do
    local tag = M.slug_keyword(word)
    if tag ~= "" and not seen[tag] then
      seen[tag] = true
      tags[#tags + 1] = tag
    end
  end
  table.sort(tags)
  return tags
end

local function prompt_tags(default, callback)
  vim.ui.input({
    prompt = "Tags (space separated, <Tab> completes): ",
    default = default,
    completion = "customlist,v:lua.require'naamanu.core.notes'.complete_tags",
  }, function(input)
    if input == nil then
      return
    end
    callback(parse_tags(input))
  end)
end

function M.file_name(id, title, tags)
  local slug = M.slug_title(title)
  return id
    .. (slug ~= "" and ("--" .. slug) or "")
    .. (#tags > 0 and ("__" .. table.concat(tags, "_")) or "")
    .. ".md"
end

function M.front_matter(title, time, tags, id)
  local quoted = vim.tbl_map(elisp_string, tags)
  return {
    "---",
    "title:      " .. elisp_string(title),
    "date:       " .. iso_date(time),
    "tags:       [" .. table.concat(quoted, ", ") .. "]",
    "identifier: " .. elisp_string(id),
    "---",
    "",
  }
end

-- A fresh identifier; like Denote, step forward a second on collision.
local function new_id()
  local time = os.time()
  while
    #vim.fn.globpath(M.dir(), "**/" .. os.date("%Y%m%dT%H%M%S", time) .. "*", false, true) > 0
  do
    time = time + 1
  end
  return os.date("%Y%m%dT%H%M%S", time), time
end

local function create(dir, title, tags, body)
  local id, time = new_id()
  vim.fn.mkdir(dir, "p")
  local path = dir .. "/" .. M.file_name(id, title, tags)
  vim.cmd.edit(vim.fn.fnameescape(path))
  local lines = M.front_matter(title, time, tags, id)
  vim.list_extend(lines, body or { "" })
  vim.api.nvim_buf_set_lines(0, 0, -1, false, lines)
  vim.api.nvim_win_set_cursor(0, { #lines, 0 })
  return path
end

function M.note_new()
  vim.ui.input({ prompt = "Note title: " }, function(title)
    if not title or vim.trim(title) == "" then
      return
    end
    -- A typed file extension is never part of the title.
    title = vim.trim(title):gsub("%.md$", "")
    prompt_tags("", function(tags)
      create(M.dir(), title, tags)
    end)
  end)
end

--- Today's journal entry (denote-journal-new-or-existing-entry): reuse the
--- first `journal`-tagged note whose identifier is today, else create one.
function M.journal()
  local today = os.date("%Y%m%d")
  for _, path in ipairs(vim.fn.globpath(journal_dir(), today .. "T*__*journal*.md", false, true)) do
    return vim.cmd.edit(vim.fn.fnameescape(path))
  end
  -- denote-journal-title-format 'day-date-month-year-24h
  local title = os.date("%A ") .. tonumber(os.date("%d")) .. os.date(" %B %Y %H:%M")
  create(journal_dir(), title, { "journal" })
end

local function pick_note(prompt, on_choice, tag)
  local items = {}
  for _, path in ipairs(all_notes()) do
    local parsed = M.parse_name(path)
    if parsed and (not tag or vim.list_contains(parsed.tags, tag)) then
      local title = M.title(path)
      items[#items + 1] = {
        text = title .. " " .. table.concat(parsed.tags, " "),
        file = path,
        id = parsed.id,
        title = title,
        tags = parsed.tags,
      }
    end
  end
  table.sort(items, function(a, b)
    return a.id > b.id
  end)
  Snacks.picker.pick({
    title = prompt,
    items = items,
    format = function(item)
      return {
        { item.title, "SnacksPickerFile" },
        { "  " .. table.concat(item.tags, " "), "SnacksPickerComment" },
      }
    end,
    confirm = function(picker, item)
      picker:close()
      if item then
        vim.schedule(function()
          on_choice(item)
        end)
      end
    end,
  })
end

function M.note_find()
  vim.fn.mkdir(M.dir(), "p")
  pick_note("Notes", function(item)
    vim.cmd.edit(vim.fn.fnameescape(item.file))
  end)
end

function M.note_search()
  vim.fn.mkdir(M.dir(), "p")
  Snacks.picker.grep({ cwd = M.dir() })
end

--- Notes carrying a tag: tag prompt, then the note picker narrowed to it.
function M.note_by_tag()
  vim.ui.select(known_tags(), { prompt = "Tag:" }, function(tag)
    if tag then
      pick_note("Notes: " .. tag, function(item)
        vim.cmd.edit(vim.fn.fnameescape(item.file))
      end, tag)
    end
  end)
end

--- Insert `[title](denote:ID)` (denote-md-link-format).
function M.note_link()
  pick_note("Link note", function(item)
    vim.api.nvim_put({ string.format("[%s](denote:%s)", item.title, item.id) }, "c", true, true)
  end)
end

--- Resolve a `denote:ID` target to a file path.
function M.resolve(id)
  return vim.fn.globpath(M.dir(), "**/" .. id .. "*", false, true)[1]
end

--- gf/<CR> on a `denote:ID` link opens the note; anything else falls back.
function M.follow()
  local line, col = vim.api.nvim_get_current_line(), vim.fn.col(".")
  for start, id, stop in line:gmatch("()denote:(%d+T%d+)()") do
    if col >= start - 1 and col <= stop then
      local path = M.resolve(id)
      if path then
        return vim.cmd.edit(vim.fn.fnameescape(path))
      end
      return vim.notify("No note with identifier " .. id, vim.log.levels.WARN)
    end
  end
  -- Not on a denote link: the cursor may be inside `[...](denote:...)` text.
  local id = line:match("%]%(denote:(%d+T%d+)%)")
  if id and M.resolve(id) then
    return vim.cmd.edit(vim.fn.fnameescape(M.resolve(id)))
  end
  vim.cmd("normal! gf")
end

--- Backlinks: every note mentioning this note's identifier (denote: links,
--- and old-style links to the file name, which also contain the ID).
function M.note_backlinks()
  local parsed = M.parse_name(vim.api.nvim_buf_get_name(0))
  if not parsed then
    return vim.notify("Current file has no Denote identifier", vim.log.levels.WARN)
  end
  Snacks.picker.grep({
    title = "Backlinks",
    cwd = M.dir(),
    search = parsed.id,
    -- The note's own front matter is not a backlink.
    exclude = { "*" .. parsed.id .. "*" },
  })
end

--- Rename the file from its front matter, keeping the identifier
--- (denote-rename-file-using-front-matter). Edit title:/tags: first.
function M.note_rename()
  local path = vim.api.nvim_buf_get_name(0)
  local parsed = M.parse_name(path)
  if not parsed then
    return vim.notify("Current file has no Denote identifier", vim.log.levels.WARN)
  end
  local lines = vim.api.nvim_buf_get_lines(0, 0, 10, false)
  local title = M.title(path)
  local raw_title = front_matter_value(lines, "title")
  if raw_title then
    title = raw_title:gsub('^"(.*)"$', "%1"):gsub('\\"', '"')
  end
  vim.ui.input({ prompt = "Title: ", default = title }, function(new_title)
    if not new_title then
      return
    end
    prompt_tags(table.concat(parsed.tags, " "), function(tags)
      -- Rewrite title:/tags: in place so front matter and name agree.
      for i, line in ipairs(vim.api.nvim_buf_get_lines(0, 0, 10, false)) do
        if line:match("^title:") then
          vim.api.nvim_buf_set_lines(
            0,
            i - 1,
            i,
            false,
            { "title:      " .. elisp_string(new_title) }
          )
        elseif line:match("^tags:") then
          local quoted = table.concat(vim.tbl_map(elisp_string, tags), ", ")
          vim.api.nvim_buf_set_lines(0, i - 1, i, false, { "tags:       [" .. quoted .. "]" })
        end
      end
      local to = vim.fs.dirname(path) .. "/" .. M.file_name(parsed.id, new_title, tags)
      if to == path then
        return vim.cmd.write()
      end
      vim.cmd.write()
      Snacks.rename.rename_file({ from = path, to = to })
    end)
  end)
end

-- BibTeX ----------------------------------------------------------------

-- Minimal BibTeX reader: key, entry type and the brace- or quote-delimited
-- fields this picker shows. Good enough for Zotero/Better BibTeX exports.
local function read_bibliography(file)
  local entries, current = {}, nil
  for _, line in ipairs(vim.fn.readfile(file)) do
    local kind, key = line:match("^%s*@([%w_%-]+)%s*{%s*([^,%s]+)")
    if kind and kind:lower() ~= "comment" and kind:lower() ~= "string" then
      current = { key = key, kind = kind:lower() }
      entries[#entries + 1] = current
    elseif current then
      local field, value = line:match('^%s*([%w_%-]+)%s*=%s*[{"](.*)[}"],?%s*$')
      if field then
        current[field:lower()] = value:gsub("[{}]", "")
      end
    end
  end
  return entries
end

local function pick_citation(prompt, on_choice)
  local file = M.bibliography()
  if vim.fn.filereadable(file) == 0 then
    return vim.notify("Bibliography not found: " .. file, vim.log.levels.WARN)
  end
  local items = {}
  for _, entry in ipairs(read_bibliography(file)) do
    local year = entry.year or (entry.date and entry.date:match("%d%d%d%d")) or ""
    local author = (entry.author or entry.editor or ""):match("^([^,]+)") or ""
    items[#items + 1] = {
      text = table.concat({ entry.key, author, year, entry.title or "" }, " "),
      entry = entry,
      author = author,
      year = year,
    }
  end
  Snacks.picker.pick({
    title = prompt,
    items = items,
    format = function(item)
      return {
        { item.entry.key, "SnacksPickerLabel" },
        { "  " .. item.author .. " " .. item.year, "SnacksPickerComment" },
        { "  " .. (item.entry.title or "") },
      }
    end,
    preview = function(ctx)
      local lines = {}
      for field, value in pairs(ctx.item.entry) do
        lines[#lines + 1] = string.format("%-10s %s", field, value)
      end
      table.sort(lines)
      ctx.preview:set_lines(lines)
    end,
    confirm = function(picker, item)
      picker:close()
      if item then
        vim.schedule(function()
          on_choice(item.entry)
        end)
      end
    end,
  })
end

--- Insert a Pandoc-style `[@key]` citation.
function M.citation_insert()
  pick_citation("Citation", function(entry)
    vim.api.nvim_put({ "[@" .. entry.key .. "]" }, "c", true, true)
  end)
end

--- Open the entry's PDF (Better BibTeX `file` field), else its DOI or URL.
function M.citation_open()
  pick_citation("Open paper", function(entry)
    local pdf = entry.file and entry.file:match("([^;]-%.pdf)")
    if pdf then
      -- Zotero's `file` may be `Description:/path.pdf:application/pdf`.
      pdf = pdf:gsub("^[^/~]*:", "")
      if vim.fn.filereadable(vim.fs.normalize(pdf)) == 1 then
        return vim.ui.open(vim.fs.normalize(pdf))
      end
    end
    local target = (entry.doi and ("https://doi.org/" .. entry.doi)) or entry.url
    if target then
      return vim.ui.open(target)
    end
    vim.notify("No file, DOI or URL for " .. entry.key, vim.log.levels.WARN)
  end)
end

local map = function(lhs, rhs, desc)
  vim.keymap.set("n", lhs, rhs, { desc = desc })
end
map("<leader>nn", M.note_new, "New note")
map("<leader>nj", M.journal, "Journal (today)")
map("<leader>nf", M.note_find, "Find note")
map("<leader>ns", M.note_search, "Search notes")
map("<leader>nt", M.note_by_tag, "Notes by tag")
map("<leader>nl", M.note_link, "Insert denote: link")
map("<leader>nb", M.note_backlinks, "Note backlinks")
map("<leader>nR", M.note_rename, "Rename note from front matter")
map("<leader>nc", M.citation_insert, "Insert citation")
map("<leader>nP", M.citation_open, "Open paper (PDF/DOI)")

-- Inside the notes directory, gf/<CR> follow denote: links.
vim.api.nvim_create_autocmd("FileType", {
  group = vim.api.nvim_create_augroup("denote-links", { clear = true }),
  pattern = "markdown",
  callback = function(ev)
    local name = vim.api.nvim_buf_get_name(ev.buf)
    if vim.startswith(name, M.dir() .. "/") then
      vim.keymap.set("n", "gf", M.follow, { buffer = ev.buf, desc = "Follow denote: link" })
      vim.keymap.set("n", "<CR>", M.follow, { buffer = ev.buf, desc = "Follow denote: link" })
    end
  end,
})

return M
