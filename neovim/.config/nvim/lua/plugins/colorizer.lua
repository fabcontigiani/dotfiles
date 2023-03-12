return {
  "NvChad/nvim-colorizer.lua",
  keys = {
    { "<leader>tc", "<cmd>ColorizerToggle<cr>", desc = "Toggle Colorizer" },
  },
  config = true,
  opts = {
    user_default_options = {
      RGB = false,
      names = false,
    },
  },
  lazy = false,
}
