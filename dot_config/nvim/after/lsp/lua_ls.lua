-- lua_ls for this config: LuaJIT and Neovim's runtime as the library, so
-- `vim`, `vim.uv` and friends resolve instead of "undefined global".
-- Projects with their own .luarc.json keep their settings.
return {
  on_init = function(client)
    local root = client.workspace_folders and client.workspace_folders[1].name
    if
      root and (vim.uv.fs_stat(root .. "/.luarc.json") or vim.uv.fs_stat(root .. "/.luarc.jsonc"))
    then
      return
    end
    client.config.settings.Lua = vim.tbl_deep_extend("force", client.config.settings.Lua or {}, {
      runtime = { version = "LuaJIT", path = { "lua/?.lua", "lua/?/init.lua" } },
      workspace = {
        checkThirdParty = false,
        library = { vim.env.VIMRUNTIME, "${3rd}/luv/library" },
      },
      diagnostics = { globals = { "Snacks", "MiniIcons" } },
    })
  end,
  settings = {
    Lua = {
      format = { enable = false }, -- stylua formats (core/format.lua)
      hint = { enable = true },
    },
  },
}
