return {
  "chentoast/marks.nvim",
  event = "VeryLazy",
  opts = {
    default_mappings = true,
    signs = true,
    mappings = {
      set_next = "m,",
      toggle = "m;",
      preview = "m:",
      next = "m]",
      prev = "m[",
      delete = "dm",
      delete_line = "dm-",
      delete_buf = "dm<space>",
    },
  },
}
