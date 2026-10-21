-- Each project and editor gets its own JDT LS workspace (no shared locks).
return {
  cmd = function(dispatchers, config)
    local root = config.root_dir or vim.fn.getcwd()
    local workspace = vim.fn.stdpath("cache") .. "/jdtls/" .. vim.fn.sha256(root)
    return vim.lsp.rpc.start({ "jdtls", "-data", workspace }, dispatchers, { cwd = root })
  end,
  filetypes = { "java" },
  root_markers = {
    { "mvnw", "gradlew", "settings.gradle", "settings.gradle.kts" },
    "pom.xml",
    "build.gradle",
    "build.gradle.kts",
    ".git",
  },
  settings = { java = { format = { enabled = true } } },
}
