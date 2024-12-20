-- Adopted from https://github.com/baptiste/quarto-flowfram
-- Originally from the the latex-environment quarto extension

-- MIT License

-- Copyright (c) 2023 Posit Software, PBC

-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

-- environment.lua
-- Copyright (C) 2020 by RStudio, PBC

local classEnvironments = pandoc.MetaMap({})
local classCommands = pandoc.MetaMap({})

-- helper that identifies arrays
local function tisarray(t)
  local i = 0
  for _ in pairs(t) do
    i = i + 1
    if t[i] == nil then return false end
  end
  return true
end

-- reads the environments
local function readEnvironments(meta)
  local env = meta['environments']
  if env ~= nil then
    if tisarray(env) then
      -- read an array of strings
      for i, v in ipairs(env) do
        local value = pandoc.utils.stringify(v)
        classEnvironments[value] = value
      end
    else
      -- read key value pairs
      for k, v in pairs(env) do
        local key = pandoc.utils.stringify(k)
        local value = pandoc.utils.stringify(v)
        classEnvironments[key] = value
      end
    end
  end
end

local function readCommands(meta)
  local env = meta['commands']
  if env ~= nil then
    if tisarray(env) then
      -- read an array of strings
      for i, v in ipairs(env) do
        local value = pandoc.utils.stringify(v)
        classCommands[value] = value
      end
    else
      -- read key value pairs
      for k, v in pairs(env) do
        local key = pandoc.utils.stringify(k)
        local value = pandoc.utils.stringify(v)
        classCommands[key] = value
      end
    end
  end
end

local function readEnvsAndCommands(meta)
  readEnvironments(meta)
  readCommands(meta)
end

-- use the environments from metadata to
-- emit a custom environment for latex
local function writeEnvironments(divEl)
  if quarto.doc.isFormat("latex") then
    for k, v in pairs(classEnvironments) do
      if divEl.attr.classes:includes(k) then
        -- process this into a latex environment
        local beginEnv = '\\begin{staticcontents*}{' .. v .. '}\n \\' .. v .. 'specs\n'
        local endEnv = '\n\\end{staticcontents*} % end-' .. v

        -- if the first and last div blocks are paragraphs then we can
        -- bring the environment begin/end closer to the content
        if divEl.content[1].t == "Para" and divEl.content[#divEl.content].t == "Para" then
          table.insert(divEl.content[1].content, 1, pandoc.RawInline('tex', beginEnv .. "\n"))
          table.insert(divEl.content[#divEl.content].content, pandoc.RawInline('tex', "\n" .. endEnv))
        else
          table.insert(divEl.content, 1, pandoc.RawBlock('tex', beginEnv))
          table.insert(divEl.content, pandoc.RawBlock('tex', endEnv))
        end
        return divEl
      end
    end
  end
end



-- Run in two passes so we process metadata
-- and then process the divs
return {
  { Meta = readEnvsAndCommands },
  { Div = writeEnvironments }
}
