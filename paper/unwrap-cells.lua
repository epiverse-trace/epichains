-- Unwrap Quarto's code cell containers so that paper.md holds plain code
-- blocks and figures rather than <div> wrappers.
function Div(el)
  for _, class in ipairs(el.classes) do
    if class == "cell" or class:match("^cell%-output") then
      return el.content
    end
  end
end
