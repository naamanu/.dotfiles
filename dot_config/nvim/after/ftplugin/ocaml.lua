-- ocaml-lsp extensions beyond the LSP spec (ocaml-lsp docs/ocamllsp/*-spec.md).
local function ocamllsp()
  return vim.lsp.get_clients({ bufnr = 0, name = "ocamllsp" })[1]
end

-- .ml <-> .mli (creating the interface if it does not exist yet).
vim.keymap.set("n", "<localleader>s", function()
  local client = ocamllsp()
  if not client then
    return vim.notify("ocamllsp is not attached", vim.log.levels.WARN)
  end
  -- Params are a one-element array holding the DocumentUri: a { uri } table
  -- is rejected as invalid and a bare string is never answered.
  client:request("ocamllsp/switchImplIntf", { vim.uri_from_bufnr(0) }, function(err, uris)
    if err or not uris or #uris == 0 then
      return vim.notify("No matching implementation/interface", vim.log.levels.WARN)
    end
    vim.cmd.edit(vim.fn.fnameescape(vim.uri_to_fname(uris[1])))
  end, 0)
end, { buffer = true, desc = "Switch .ml/.mli" })

-- Typed holes (`_`): jump to the next/previous one, then `<leader>la` offers
-- Construct to fill it from its type.
local function hole(direction)
  return function()
    local client = ocamllsp()
    if not client then
      return
    end
    client:request("ocamllsp/typedHoles", { uri = vim.uri_from_bufnr(0) }, function(err, holes)
      if err or not holes or #holes == 0 then
        return vim.notify("No typed holes", vim.log.levels.INFO)
      end
      local row, col = unpack(vim.api.nvim_win_get_cursor(0))
      row = row - 1
      table.sort(holes, function(a, b)
        return a.start.line < b.start.line
          or (a.start.line == b.start.line and a.start.character < b.start.character)
      end)
      local pick
      if direction > 0 then
        for _, h in ipairs(holes) do
          if h.start.line > row or (h.start.line == row and h.start.character > col) then
            pick = h
            break
          end
        end
        pick = pick or holes[1]
      else
        for i = #holes, 1, -1 do
          local h = holes[i]
          if h.start.line < row or (h.start.line == row and h.start.character < col) then
            pick = h
            break
          end
        end
        pick = pick or holes[#holes]
      end
      vim.api.nvim_win_set_cursor(0, { pick.start.line + 1, pick.start.character })
    end, 0)
  end
end
vim.keymap.set("n", "]H", hole(1), { buffer = true, desc = "Next typed hole" })
vim.keymap.set("n", "[H", hole(-1), { buffer = true, desc = "Previous typed hole" })
