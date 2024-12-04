-- Define a function to open a scratch buffer, tile it, and set up agenda
function OpenScratchAgenda()

   -- Get the current date information
  local year = os.date("%Y")
  local month = os.date("%m")
  local day = os.date("%d")
  
  -- Define the base path for the agenda
  local agenda_dir = vim.fn.expand("~")
  local year_dir = agenda_dir .. "/" .. year
  local month_dir = year_dir .. "/" .. month

  -- Ensure the agenda directory exists, and create it if necessary
  if not vim.fn.isdirectory(agenda_dir) then
    print("Creating agenda directory: " .. agenda_dir)
    vim.fn.mkdir(agenda_dir, "p")
  end

  -- Ensure the year folder exists, and create it if necessary
  if not vim.fn.isdirectory(year_dir) then
    vim.fn.mkdir(year_dir, "p")
  end

  -- Ensure the month folder exists, and create it if necessary
  if not vim.fn.isdirectory(month_dir) then
    vim.fn.mkdir(month_dir, "p")
  end

  -- Create the file for the agenda (inside the year/month folder)
  local agenda_file = month_dir .. "/" .. "agenda_" .. year .. "_" .. month .. "_" .. day .. ".md"

    -- Check if the agenda file already exists
  if vim.fn.filereadable(agenda_file) == 1 then
    -- If the file exists, open it in the current buffer
    vim.cmd("edit " .. agenda_file)
  else
    -- If the file doesn"t exist, create a new scratch buffer and open it
    -- Get the total width of the terminal (number of columns)
    local screen_width = vim.o.columns
  
    -- Calculate 20% of the screen width
    local split_width = math.floor(screen_width * 0.25)
  
    -- Open a new vertical split and create a scratch buffer
    vim.cmd("vsplit")  -- Create a vertical split
    vim.cmd("enew")    -- Open an empty buffer in the new split
 
    -- Move the new split to the left side (wincmd H)
    vim.cmd("wincmd H")
  
    -- Set the filetype to markdown for checkbox syntax highlighting
    vim.bo.filetype = "markdown"
  
    -- Resize the window to 20% of screen width
    vim.cmd("vertical resize " .. split_width)
  
    -- Optionally, add a title to the scratch agenda
    vim.api.nvim_buf_set_lines(0, 0, 1, false, { "# Agenda for " .. os.date("%Y-%m-%d") })
    vim.api.nvim_buf_set_lines(0, 1, 2, false, { "" }) -- Add space
  
    -- Highlighting for unchecked and checked checkboxes
    vim.cmd("highlight CheckedCheckbox guifg=#00FF00 gui=bold")
    vim.cmd("highlight UncheckedCheckbox guifg=#FF0000")
  
    -- Show example agenda items with checkboxes
    vim.api.nvim_buf_set_lines(0, 2, 3, false, { "[ ] Task 1" })
    vim.api.nvim_buf_set_lines(0, 3, 4, false, { "[ ] Task 2" })
    vim.api.nvim_buf_set_lines(0, 4, 5, false, { "[ ] Task 3" })

    -- Automatically save and quit when you close the scratch buffer
    -- Set the filename to the full path of the agenda file
    vim.api.nvim_buf_set_name(0, agenda_file)  -- Set the buffer name to the agenda file path
  
    -- Automatically save when the buffer is unloaded (e.g., on quit)
    vim.cmd("autocmd BufDelete <buffer> silent! write " .. agenda_file)
  end
end

-- Map a shortcut (e.g., <leader>a) to open the scratch agenda
vim.keymap.set("n", "<leader>today", ":lua OpenScratchAgenda()<CR>", { noremap = true, silent = true })

