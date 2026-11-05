-- gopls formats with gofumpt (built in, no binary needed) and organizes
-- imports on save via format.lua. Emacs's Eglot sends the same settings.
return {
  settings = {
    gopls = {
      gofumpt = true,
      staticcheck = true,
      usePlaceholders = true,
      completeUnimported = true,
      analyses = { unusedparams = true, unusedwrite = true, nilness = true, shadow = true },
      codelenses = { test = true, tidy = true, generate = true, upgrade_dependency = true },
      hints = {
        assignVariableTypes = true,
        compositeLiteralFields = true,
        constantValues = true,
        functionTypeParameters = true,
        parameterNames = true,
        rangeVariableTypes = true,
      },
    },
  },
}
