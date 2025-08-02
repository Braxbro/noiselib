local lib = {}

--[[
    To preface - the following code generally avoids using character range shorthand (ex. %s for whitespace) for readability.
]]

lib.token_type_expressions = {}
local tte = lib.token_type_expressions
-- A capture for contiguous whitespace recognized by the noise system.
tte.whitespace = "([ \n\r\t]*)"
-- A capture for valid identifiers (e.g. variable names) recognized by the noise system.
tte.identifier = "([a-zA-Z_][a-zA-Z0-9_:]*)"
--[[
    Captures for the different number formats recognized by the noise system.
    Because there is no alternation, it's impossible to write all the supported number forms 
    as a single expression. So the three formats are written independently.

    While 1.e1 is (according to the API docs) valid scientific notation
    (and 1. is valid decimal notation) for the noise system, I find this to be sloppy.
    As a result, this library expects the following stricter notations:
    Scientific Notation: 1e1, .1e1 OR 1.0e1 - not 1.e1.
    Decimal Notation: 1, .1 OR 1.0 - not 1.

    fix_string(str) will automatically convert noise-valid notation to this stricter notation.
    When doing so, 1. becomes 1.0 and 1.e1 becomes 1.0e1.
]]
tte.number_hex = "(0x[0-9a-f]+)"
tte.number_decimal = "([0-9]*%.?[0-9]+)"
tte.number_scientific = "([0-9]*%.?[0-9]+e%-?[0-9]+)"
--[[
    Captures for strings using both quote types.
    Similarly to numbers, can't put both quote types in a single capture.
]]
tte.string_single = "(%b'')"
tte.string_double = '(%b"")'
--[[
    Captures for functions. Also catches variables in var() form as positional functions.
    Named and positional arguments may need to be handled differently, so they are separated.
    They also can't be unified in a single capture, like the two different types of quotes for strings.
]]
tte.func_named = "([a-zA-Z_][a-zA-Z0-9_:]*%b{})"
tte.func_positional = "([a-zA-Z_][a-zA-Z0-9_:]*%b())"

--[[ 
    A pair of captures for noise-valid, but not library-valid, notation.
    The first capture matches library-invalid scientific notation anywhere 
    and decimal notation not at the end of the string.
    The second capture matches library-invalid decimal notation at the end of a string.
]]
local bad_number = "([0-9]+%.)([^0-9]+)"
local bad_number_end = "([0-9]+%.)$"
--[[ 
    Catches and fixes formats that are noise-system compatible but against this library's standard.
]]
function lib.fix_string(str)
    --[[
        backup string literals and replace them with empty strings of their quote type
        
        replacing other value types is unnecessary, as no noise-valid but not library-valid numbers
        can occur in identifiers, function names, or whitespace,  because identifiers/function names
        do not permit '.' characters, and a '.' character isn't whitespace
    ]]
    local string_single_literals = {}
    local string_double_literals = {}
    -- captures to split the string by the first literal of either type and capture all three pieces
    local single_match = "^(.-)(%b'')(.*)$"
    local double_match = '^(.-)(%b"")(.*)$'
    local rawstr = str
    local to_fix = ""
    while (string.len(rawstr) > 0) do
        -- find the first possible string literal of each type, to be able to find which occurs first
        local _, _, sslc_prefix, str_single_lit_candidate, sslc_leftovers =
            string.find(rawstr, single_match)
        local _, _, sdlc_prefix, str_double_lit_candidate, sdlc_leftovers =
            string.find(rawstr, double_match)
        -- if either type is not present, tiebreak against it
        local sslc_start, sdlc_start =
            sslc_prefix and string.len(sslc_prefix) or math.huge,
            sdlc_prefix and string.len(sdlc_prefix) or math.huge
        -- equal case is impossible, unless there are no more strings
        if sslc_start == sdlc_start then
            to_fix = to_fix .. rawstr
            rawstr = ""
        --[[
            replace found literals with empty string literals, which
            prevents their contents from interfering with detection

            preserves quote type, so that the literals can be reinserted in order later
        ]]
        elseif sslc_start < sdlc_start then
            table.insert(string_single_literals, str_single_lit_candidate)
            to_fix = to_fix .. sslc_prefix .. "''"
            rawstr = sslc_leftovers
        else
            table.insert(string_double_literals, str_double_lit_candidate)
            to_fix = to_fix .. sdlc_prefix .. '""'
            rawstr = sdlc_leftovers
        end
    end
    --[[
        Match and fix both bad number cases. The first gsub's second return value is implicitly 
        discarded by the second :gsub. the second gsub's second return value is explicitly discarded.
    ]]
    local fixedstr, _ = to_fix:gsub(bad_number, "%10%2"):gsub(bad_number_end, "%10")
    local newstr = ""
    while (string.len(fixedstr) > 0) do
        local _, _, sslc_prefix, _, sslc_leftovers =
            string.find(fixedstr, single_match)
        local _, _, sdlc_prefix, _, sdlc_leftovers =
            string.find(fixedstr, double_match)
        -- if either type is not present, tiebreak against it
        local sslc_start, sdlc_start =
            sslc_prefix and string.len(sslc_prefix) or math.huge,
            sdlc_prefix and string.len(sdlc_prefix) or math.huge
        -- equal case is impossible, unless there are no more strings
        if sslc_start == sdlc_start then
            newstr = newstr .. fixedstr
            fixedstr = ""
        --[[
            restores string literals based on order of occurrence
        ]]
        elseif sslc_start < sdlc_start then
            newstr = newstr .. sslc_prefix .. table.remove(string_single_literals, 1)
            fixedstr = sslc_leftovers
        else
            newstr = newstr .. sdlc_prefix .. table.remove(string_double_literals, 1)
            fixedstr = sdlc_leftovers
        end
    end
    return newstr
end

--[[
    Removes whitespace at the beginning and end of the string and shrinks other whitespace into single spaces.
]]
function lib.trim_string(str)
    local newstr, _ = str:gsub("^[ \n\r\t]*(.-)[ \n\r\t]*$", "%1")
    local newstr, _ = newstr:gsub("[ \n\r\t]+", " ")
    return newstr
end

local noise_object_template = {
    --[[
        The type of the noise object. Should always be one of the following:

        "operator" - An operator with one or more operands.

        "function" - A NamedNoiseFunction, invoked by identifier with possible arguments.
        
        "expression" - A NamedNoiseExpression, referenced by identifier.

        "number" - A number literal in either hexadecimal, decimal, or scientific notation.

        "string" - A string literal denoted either by single or double quotes.
    ]]
    type = nil,
    -- If operatior, the operator; otherwise nil.
    operator = nil,
    -- If function or expression, the name of the function or expression; otherwise nil.
    name = nil,
    --[[
        If operator, is "explicit" or "implicit", depending on if the operator is grouped by parentheses.
        Changing operator always changes the format to explicit.

        If function, is "named" or "positional", depending on how arguments are specified.
        
        If number, is "hexadecimal", "decimal", or "scientific".

        If string, is "single" or "double", depending on type of quote used.
    ]]
    format = nil,
    -- If explicit or implicit operation, contains operands from left to right; otherwise nil.
    operands = nil,
    --[[
        If function, contains arguments by name; otherwise nil.
        Can be indexed by name or position.
    ]]
    arguments = nil,
    --[[
        If number or string, the literal value of the number or string; otherwise nil.
    ]]
    literal_value = nil,
}
-- Parses the noise string given and returns a noise object.
function lib.parse_noise(str)
    --[[
        To start, remove all the values so that all that's left are
        standardized references and operators, so that functions don't
        mess with the detection of operators grouped by parentheses and 
        strings don't mess with the detection of operators included in the string.
    ]]
    local values = {}

    -- Remaining string to remove values from
    local remstr = lib.trim_string(str)
    -- String with only references and operators
    local modstr = ""

    while(string.len(remstr) > 0) do
        local prefixes, suffixes, noise_values = {}, {}, {}
        local next_value_type, min_loc = "", math.huge
        --[[ 
            I need a for loop to find the first value if I don't want to write a giant pile of hardcoded
            spaghetti, so might as well also use it to do the value parsing.
        ]]
        for index, pattern in pairs(tte) do
            if index ~= "whitespace" then
                _, _, prefixes[index], noise_values[index], suffixes[index] = 
                    string.find(remstr, "^(.-)" .. pattern .. "(.*)$")
                if prefixes[index] then -- check that string.find found something
                    local prefix_length = string.len(prefixes[index])
                    -- Figure out the next value while collecting them. 
                    if prefix_length < min_loc or
                        ((next_value_type == "identifier" and prefix_length == min_loc) and
                            (index == "func_named" or index == "func_positional")
                        )
                        then
                        next_value_type, min_loc = index, prefix_length
                    end
                    
                    -- Check for number of characters that could start a recognized value type
                    local _, nstartchars = string.gsub(prefixes[index], "(['\"0-9a-zA-Z_])", "%1")
                    --[[
                        Only identifiers can false-positive at the same position as the first possible value.
                        This is because functions begin with an identifier - every other value type
                        begins with a different character that isn't allowed at the start of an identifier.
                        In other cases, it's safe to not check other value types. 
                    ]]
                    if index ~= "identifier" and nstartchars == 0 then
                        break
                    end
                end
            end
        end
        modstr = modstr .. prefixes[next_value_type] .. "[" .. #values+1 .. "]"
        remstr = suffixes[next_value_type]
        table.insert(values, noise_values[next_value_type])
    end

    --[[
        Now that all the values have been separated out, remove all explicit operators so that
        all that's left are value references, explicit operator references, and implicit operators.
        (Explicit operators are grouped by parentheses.)
    ]]
    local subexpressions = {}
    remstr = modstr
    modstr = ""
    -- use a custom replacement function to retrieve the operators and properly replace them with references
    modstr = string.gsub(remstr, "%b()", function(value)
        table.insert(subexpressions, string.sub(value, 2, #value - 1))
        return "(" .. #subexpressions .. ")"
    end)

    --[[
        Now that top-level values are separated out (recursion will still be needed for function args
        and explicit operators) operator precedence can be safely processed.
    ]]
    
    local operator_list = {} -- establish precedence-indexed list of operator lists

    --[[
        These lists are indexed by operator symbols, because operator symbols must be nonambiguous.
        As a result, for a given precedence level, each symbol is unique, but the operator may not be.
        This is because Factorio supports both C++ and Lua syntax for not equals in noise expressions.
    ]]
    operator_list[9] = {} 
    operator_list[9]["^"] = {operator = "exponentiation", binary = true}

    operator_list[8] = {}
    operator_list[8]["+"] = {operator = "unary_plus", unary = true}
    operator_list[8]["-"] = {operator = "unary_minus", unary = true}
    operator_list[8]["~"] = {operator = "unary_not", unary = true}

    operator_list[7] = {}
    operator_list[7]["*"] = {operator = "multiplication", binary = true}
    operator_list[7]["/"] = {operator = "division", binary = true}
    operator_list[7]["%"] = {operator = "modulo", binary = true}
    operator_list[7]["%%"] = {operator = "remainder", binary = true}

    operator_list[6] = {}
    operator_list[6]["+"] = {operator = "addition", binary = true}
    operator_list[6]["-"] = {operator = "subtraction", binary = true}

    operator_list[5] = {}
    operator_list[5]["<"] = {operator = "less_than", binary = true}
    operator_list[5]["<="] = {operator = "less_or_equal", binary = true}
    operator_list[5][">"] = {operator = "greater_than", binary = true}
    operator_list[5][">="] = {operator = "greater_or_equal", binary = true}

    operator_list[4] = {}
    operator_list[4]["=="] = {operator = "equal", binary = true}
    operator_list[4]["~="] = {operator = "not_equal", binary = true} -- lua syntax
    operator_list[4]["!="] = {operator = "not_equal", binary = true} -- c++ syntax

    operator_list[3] = {}
    operator_list[3]["&"] = {operator = "bitwise_and", binary = true}

    operator_list[2] = {}
    operator_list[2]["~"] = {operator = "bitwise_xor", binary = true}

    operator_list[1] = {}
    operator_list[1]["|"] = {operator = "bitwise_or", binary = true}
    
    -- This is unnecessary - operator_list could just contain escaped operators, but that'd be less readable.
    local function escape_magic_characters(pattern)
        local escaped_pattern, _ = string.gsub(pattern, "([().%+-*?[^$])", "%%%1")
        return escaped_pattern
    end

    --[[
        Accepts a pre-processed string of implicit operators.
        Returns an iterator that returns two values: next_op and lookahead_op
    ]]
    local function next_op(operators)
        local refpattern = "[%[%(][0-9]+[%]%)]"
        local unarygroup = "[+-~]?"
        local ows = "%s*" -- Short for 'optional whitespace'
        local index = 1
        local function get_next_operator(start, lookahead)
            local found
            for i = 9, 1, -1 do
                for symbol, opdata in pairs(operator_list[i]) do
                    if opdata.binary then
                        local pattern = "^".. ows .. "(" .. refpattern .. ows .. escape_magic_characters(symbol)
                                        .. ows .. unarygroup .. ows .. refpattern .. ")"
                        -- attempt to find operator at the start of the search window
                        local raw = string.match(operators, pattern, start)
                        if raw then
                            found = {raw = raw, symbol = symbol, precedence = i, operator = opdata.operator}
                        end
                    elseif opdata.unary then
                        local pattern = "^" .. ows .. "(" .. escape_magic_characters(symbol) .. ows .. refpattern .. ")"
                        local raw = string.match(operators, pattern, start)
                        if raw then
                            found = {raw = raw, symbol = symbol, precedence = i, operator = opdata.operator}
                        end
                    else
                        --[[
                            If this debug error shows up, something went horribly wrong
                            operator_list is local and relatively protected from tampering by other mods.
                            Its definition also should never trigger this error.
                            However, I would rather it error out if something does happen.
                        ]]
                        error("Unrecognized operator definition for operator " .. opdata.operator .. 
                        ".\nExpected either operator_list[" .. i .. "][".. symbol .."].binary or \z
                        operator_list[" .. i .. "][".. symbol .."].unary to be true, but neither were.")
                    end
                    if found then
                        break
                    end
                end
                if found then
                    break
                end
            end
            if (not lookahead) and found then
                local _, next_start = string.find(operators, found.symbol, start, true)
                if next_start then
                    next_start = next_start + 1 -- increment so it doesn't pick up the last character of the operator later
                    return found, get_next_operator(next_start, true), next_start
                end
            end
            -- this is technically synonymous with return nil I think?
            return found
        end
        return function()
            if index then
                local next_op, lookahead_op, next_start = get_next_operator(index, false)
                index = next_start
                return next_op, lookahead_op
            else
                error("Lost index in next_op iterator before finishing the loop")
            end
        end
    end

    local function parse_precedence(operators)
        
    end
end

return lib