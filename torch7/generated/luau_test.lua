---- TestSuite.lua

local test_out_print = print

local TestSuite = {}
TestSuite.__index = TestSuite

function TestSuite:new()
   local obj = {
      __tests = {},
      __isTestSuite = true
   }

   local metatable = {}

   function metatable:__index(key)
      return self.__tests[key]
   end

   function metatable:__newindex(key, value)
      if self.__tests[key] ~= nil then
         error("Test " .. tostring(key) .. " is already defined.")
      end
      if type(value) ~= "function" then
         if type(value) == "table" then
            error("Nested tables of tests are not supported")
         else
            error("Only functions are supported as members of a TestSuite")
         end
      end
      self.__tests[key] = value
   end

   setmetatable(obj, metatable)

   return obj
end

---- Tester.lua

local check = {} -- helper functions, defined at the bottom of the file

local Tester = {}
Tester.__index = Tester

function Tester:new()
   local self = setmetatable({}, Tester)
   self:__init()
   return self
end

function Tester:__init()
   self.errors = {}
   self.tests = {}
   self.warnings = {}
   self._warningCount = {}
   self.disabledTests = {}
   self._currentTestName = ''

   -- To maintain backwards compatibility (at least for a short while),
   -- disable exact dimension checking of tensors when :assertTensorEq is
   -- called. Thus {{1}} == {1} when this flag is true.
   --
   -- Note that other methods that suppose tensor checking (such as
   -- :assertGeneralEq) ignore this flag, since previously they didn't
   -- exist or support tensor equality checks at all, so there is no
   -- old code that uses these functions and relies on the behaviour.
   --
   -- Note also that if the dimension check fails with this flag is true, then
   -- will show a warning.
   self._assertTensorEqIgnoresDims = true
end

function Tester:setEarlyAbort(earlyAbort)
   self.earlyAbort = earlyAbort
end

function Tester:setRethrowErrors(rethrow)
   self.rethrow = rethrow
end

function Tester:setSummaryOnly(summaryOnly)
   self.summaryOnly = summaryOnly
end

-- Add a success to the test.
function Tester:_success()
   local name = self._currentTestName
   self.assertionPass[name] = self.assertionPass[name] + 1
   return true
end

function Tester:_addDebugInfo(message)
   local ss = debug.traceback('tester', 3) or ''
   ss = ss:match('.-\n([^\n]+\n[^\n]+)\n[^\n]+xpcall') or ''
   local name = self._currentTestName
   return (name ~= '' and name .. '\n' or '') .. message .. '\n' .. ss
end

-- Add a failure to the test.
function Tester:_failure(message)
   if self.rethrow then error(message, 2) end
   local name = self._currentTestName
   self.assertionFail[name] = self.assertionFail[name] + 1
   self.errors[#self.errors + 1] = self:_addDebugInfo(message)
   return false
end

-- Add a warning to the test
function Tester:_warning(message)
   local name = self._currentTestName
   self._warningCount[name] = (self._warningCount[name] or 0) + 1
   self.warnings[#self.warnings + 1] = self:_addDebugInfo(message)
end

-- Call this during a test run with `condition = true` to log a success, or with
-- `condition = false` to log a failure (using `message`).
function Tester:_assert_sub(condition, message)
   if condition then
      return self:_success()
   else
      return self:_failure(message)
   end
end

local function getMessage(message, ...)
   assert(next{...} == nil, "Unexpected arguments passed to test function")
   if message then
      assert(type(message) == 'string', 'message parameter must be a string')
      if message ~= '' then
         return message .. '\n'
      end
   end
   return ''
end

--[[ Historically, some test functions have accepted both a message and a
tolerance, and some just a message (e.g., assertTableEq). Now assertTableEq
accepts both a tolerance and a message, so allow the two arguments to be passed
in either order to maintain backwards compatibility (and more generally,
for convenience). (We still document the ordering as "tolerance, message" for
clarity.) This function also sanitizes them (ensures they are non-nil, etc).
]]
local function getToleranceAndMessage(defaultTolerance, ...)
   local args = {...}
   local message = nil
   local tolerance = nil
   for _, a in ipairs(args) do
      if type(a) == 'string' then
         if message then
            error("Unexpected string argument; already have message", a)
         end
         message = a .. '\n'
      elseif type(a) == 'number' then
         if tolerance then
            error("Unexpected number argument; already have tolerance", a)
         end
         tolerance = a
         assert(tolerance >= 0, "tolerance cannot be negative")
      else
         error("Unrecognized argument; should be a tolerance or message", a)
      end
   end
   message = message or ''
   tolerance = tolerance or defaultTolerance
   return tolerance, message
end

function Tester:assert(condition, ...)
   local message = getMessage(...)
   if type(condition) ~= 'boolean' then
      self:_warning(" :assert should only be used for boolean conditions. "
                    .. "To check for non-nil variables, do this explicitly: "
                    .. "Tester:assert(var ~= nil).")
   end
   return self:_assert_sub(condition,
                           string.format('%sBOOL violation condition=%s',
                                         message, tostring(condition)))
end

function Tester:assertGeneralEq(got, expected, ...)
   return self:_eqOrNeq(got, expected, false, ...)
end

function Tester:eq(got, expected, ...)
   return self:assertGeneralEq(got, expected, ...)
end

function Tester:assertGeneralNe(got, unexpected, ...)
   return self:_eqOrNeq(got, unexpected, true, ...)
end

function Tester:ne(got, unexpected, ...)
   return self:assertGeneralNe(got, unexpected, ...)
end

function Tester:_eqOrNeq(got, expected, negate, ...)
   local tolerance, message = getToleranceAndMessage(0, ...)
   local success, subMessage = check.areEq(got, expected, tolerance, negate)
   subMessage = subMessage or ''
   return self:_assert_sub(success, message .. subMessage)
end

function Tester:assertlt(a, b, ...)
   local message = getMessage(...)
   return self:_assert_sub(a < b,
                           string.format('%sLT failed: %s >= %s',
                                         message, tostring(a), tostring(b)))
end

function Tester:assertgt(a, b, ...)
   local message = getMessage(...)
   return self:_assert_sub(a > b,
                           string.format('%sGT failed: %s <= %s',
                                         message, tostring(a), tostring(b)))
end

function Tester:assertle(a, b, ...)
   local message = getMessage(...)
   return self:_assert_sub(a <= b,
                           string.format('%sLE failed: %s > %s',
                                         message, tostring(a), tostring(b)))
end

function Tester:assertge(a, b, ...)
   local message = getMessage(...)
   return self:_assert_sub(a >= b,
                           string.format('%sGE failed: %s < %s',
                                         message, tostring(a), tostring(b)))
end

function Tester:assertalmosteq(a, b, ...)
   local tolerance, message = getToleranceAndMessage(1e-16, ...)
   local diff = math.abs(a - b)
   return self:_assert_sub(
         diff <= tolerance,
         string.format(
               '%sALMOST_EQ failed: %s ~= %s with tolerance=%s',
               message, tostring(a), tostring(b), tostring(tolerance)))
end

function Tester:asserteq(a, b, ...)
   local message = getMessage(...)
   return self:_assert_sub(a == b,
                           string.format('%sEQ failed: %s ~= %s',
                                         message, tostring(a), tostring(b)))
end

function Tester:assertne(a, b, ...)
   local message = getMessage(...)
   if type(a) == type(b) and type(a) == 'table' or type(a) == 'userdata' then
      self:_warning(" :assertne should only be used to compare basic lua "
                    .. "objects (numbers, booleans, etc). Consider using "
                    .. "either :assertGeneralNe or :assert(a ~= b).")
   end
   return self:_assert_sub(a ~= b,
                           string.format('%sNE failed: %s == %s',
                                         message, tostring(a), tostring(b)))
end

function Tester:assertTensorEq(ta, tb, ...)
  return self:_assertTensorEqOrNeq(ta, tb, false, ...)
end

function Tester:assertTensorNe(ta, tb, ...)
  return self:_assertTensorEqOrNeq(ta, tb, true, ...)
end

function Tester:_assertTensorEqOrNeq(ta, tb, negate, ...)
   assert(torch.isTensor(ta), "First argument should be a Tensor")
   assert(torch.isTensor(tb), "Second argument should be a Tensor")

   local tolerance, message = getToleranceAndMessage(0, ...)
   local success, subMessage =
         check.areTensorsEq(ta, tb, tolerance, negate,
                            self._assertTensorEqIgnoresDims)
   subMessage = subMessage or ''

   if self._assertTensorEqIgnoresDims and (not negate) and success
         and not ta:isSameSizeAs(tb) then
     self:_warning("Tensors have the same content but different dimensions. "
                   .. "For backwards compatibility, they are considered equal, "
                   .. "but this may change in the future. Consider using :eq "
                   .. "to check for equality instead.")
   end

   return self:_assert_sub(success, message .. subMessage)
end

function Tester:assertTableEq(ta, tb, ...)
   return self:_assertTableEqOrNeq(ta, tb, false, ...)
end

function Tester:assertTableNe(ta, tb, ...)
   return self:_assertTableEqOrNeq(ta, tb, true, ...)
end

function Tester:_assertTableEqOrNeq(ta, tb, negate, ...)
   assert(type(ta) == 'table', "First argument should be a Table")
   assert(type(tb) == 'table', "Second argument should be a Table")
   return self:_eqOrNeq(ta, tb, negate, ...)
end

function Tester:assertError(f, ...)
   return self:assertErrorObj(f, function() return true end, ...)
end

function Tester:assertNoError(f, ...)
   local message = getMessage(...)
   local status, err = pcall(f)
   return self:_assert_sub(status,
                           string.format('%sERROR violation: err=%s', message,
                                         tostring(err)))
end

function Tester:assertErrorMsg(f, errmsg, ...)
   return self:assertErrorObj(f, function(err) return err == errmsg end, ...)
end

function Tester:assertErrorPattern(f, errPattern, ...)
   local function errcomp(err)
      return string.find(err, errPattern) ~= nil
   end
   return self:assertErrorObj(f, errcomp, ...)
end

function Tester:assertErrorObj(f, errcomp, ...)
   local message = getMessage(...)
   local status, err = pcall(f)
   return self:_assert_sub((not status) and errcomp(err),
                           string.format('%sERROR violation: err=%s', message,
                                         tostring(err)))
end

function Tester:add(f, name)
   if type(f) == "table" then
      assert(name == nil, "Name parameter is forbidden for a table of tests, "
                          .. "since its use is ambiguous")
      if f.__isTestSuite then
         f = f.__tests
      else
         self:_warning("Should use TestSuite rather than plain lua table")
      end
      for i, v in pairs(f) do
         -- We forbid nested tests because the "expected" behaviour when a named
         -- test is run in the case that the named test is in fact a table of
         -- tests is not supported. Similar issue with _setUp and _tearDown
         -- functions inside nested tests.
         assert(type(v) ~= 'table', "Nested sets of tests are not supported")
         self:add(v, i)
      end
      return self
   end

   assert(type(f) == 'function',
          "Only tables of functions and functions supported")

   if name == '_setUp' then
      assert(not self._setUp, "Only one set-up function allowed")
      self._setUp = f
   elseif name == '_tearDown' then
      assert(not self._tearDown, "Only one tear-down function allowed")
      self._tearDown = f
   else
      name = name or 'unknown'
      if self.tests[name] ~= nil then
         error('Test with name ' .. name .. ' already exists!')
      end
      self.tests[name] = f
   end
   return self
end

function Tester:disable(testNames)
   if type(testNames) == 'string' then
      testNames = {testNames}
   end
   assert(type(testNames) == 'table', "Expecting name or list for disable")
   for _, name in ipairs(testNames) do
      assert(self.tests[name], "Unrecognized test '" .. name .. "'")
      self.disabledTests[name] = true
   end
   return self
end

function Tester:run(testNames)
   local tests = self:_getTests(testNames)
   self.assertionPass = {}
   self.assertionFail = {}
   self.haveWarning = {}
   self.testError = {}
   for name in pairs(tests) do
      self.assertionPass[name] = 0
      self.assertionFail[name] = 0
      self.testError[name] = 0
      self._warningCount[name] = 0
   end
   self:_run(tests)
   self:_report(tests)

   -- Throws an error on test failure/error, so that test script returns
   -- with nonzero return value.
   for name in pairs(tests) do
      assert(self.assertionFail[name] == 0,
             'An error was found while running tests!')
      assert(self.testError[name] == 0,
             'An error was found while running tests!')
   end

   return 0
end

local function pluralize(num, str)
   local stem = num .. ' ' .. str
   if num == 1 then
      return stem
   else
      return stem .. 's'
   end
end

local NCOLS = 80
local coloured
local enable_colors, c = pcall(require, 'sys.colors')
if arg and enable_colors then  -- have we been invoked from the commandline?
   coloured = function(str, colour)
      return colour .. str .. c.none
   end
else
   c = {}
   coloured = function(str)
      return str
   end
end

function Tester:_run(tests)
   local ntests = 0
   for _ in pairs(tests) do
      ntests = ntests + 1
   end

   local ntestsAsString = string.format('%u', ntests)
   local cfmt = string.format('%%%uu/%u ', ntestsAsString:len(), ntestsAsString)
   local cfmtlen = ntestsAsString:len() * 2 + 2

   local function bracket(str)
      return '[' .. str .. ']'
   end

   test_out_print('Running ' .. pluralize(ntests, 'test') .. '\n')
   local i = 1
   for name, fn in pairs(tests) do
      self._currentTestName = name

      -- TODO: compute max length of name and cut it down to size if needed
      local strinit = coloured(string.format(cfmt, i), c.cyan)
                      .. self._currentTestName .. ' '
                      .. string.rep('.',
                                    NCOLS - 6 - 2 -
                                    cfmtlen - self._currentTestName:len())
                      .. ' '
      test_out_print(strinit .. bracket(coloured('WAIT', c.cyan)))

      local status, message, pass, skip
      if self.disabledTests[name] then
         skip = true
      else
         skip = false
         if self._setUp then
            self._setUp(name)
         end
         if self.rethrow then
            status = true
            local nerr = #self.errors
            message = fn()
            pass = nerr == #self.errors
         else
            status, message, pass = self:_pcall(fn)
         end
         if self._tearDown then
            self._tearDown(name)
         end
      end

      test_out_print('\r')
      test_out_print(strinit)

      if skip then
         test_out_print(bracket(coloured('SKIP', c.yellow)))
      elseif not status then
         self.testError[name] = 1
         test_out_print(bracket(coloured('ERROR', c.magenta)))
      elseif not pass then
         test_out_print(bracket(coloured('FAIL', c.red)))
      else
         test_out_print(bracket(coloured('PASS', c.green)))
         if self._warningCount[name] > 0 then
            test_out_print('\n' .. string.rep(' ', NCOLS - 10))
            test_out_print(bracket(coloured('+warning', c.yellow)))
         end
      end
      test_out_print('\n')

      if self.earlyAbort and (i < ntests) and (not status or not pass)
            and (not skip) then
         test_out_print('Aborting on first error, not all tests have been executed\n')
         break
      end

      i = i + 1
   end
end

function Tester:_pcall(f)
   local nerr = #self.errors
   local stat, result = xpcall(f, debug.traceback)
   if not stat then
      self.errors[#self.errors + 1] =
         self._currentTestName .. '\n Function call failed\n' .. result .. '\n'
   end
   return stat, result, stat and (nerr == #self.errors)
end

function Tester:_getTests(testNames)
   if testNames == nil then
      return self.tests
   end
   if type(testNames) == 'string' then
      testNames = {testNames}
   end
   assert(type(testNames) == 'table',
          "Only accept a name or table of test names (or nil for all tests)")

   local function getMatchingNames(pattern)
      local matchingNames = {}
      for name in pairs(self.tests) do
         if string.match(name, pattern) then
            table.insert(matchingNames, name)
         end
      end
      return matchingNames
   end

   local tests = {}
   for _, pattern in ipairs(testNames) do
      local matchingNames = getMatchingNames(pattern)
      assert(#matchingNames > 0, "Couldn't find test '" .. pattern .. "'")
      for _, name in ipairs(matchingNames) do
         tests[name] = self.tests[name]
      end
   end
   return tests
end

function Tester:_report(tests)
   local ntests = 0
   local nfailures = 0
   local nerrors = 0
   local nskipped = 0
   local nwarnings = 0
   self.countasserts = 0
   for name in pairs(tests) do
      ntests = ntests + 1
      self.countasserts = self.countasserts + self.assertionFail[name]
                          + self.assertionPass[name]
      if self.assertionFail[name] > 0 then
         nfailures = nfailures + 1
      end
      if self.testError[name] > 0 then
         nerrors = nerrors + 1
      end
      if self._warningCount[name] > 0 then
         nwarnings = nwarnings + 1
      end
      if self.disabledTests[name] then
         nskipped = nskipped + 1
      end
   end
   if self._warningCount[''] then
      nwarnings = nwarnings + self._warningCount['']
   end

   test_out_print('Completed ' .. pluralize(self.countasserts, 'assert'))
   test_out_print(' in ' .. pluralize(ntests, 'test') .. ' with ')
   test_out_print(coloured(pluralize(nfailures, 'failure'),
                     nfailures == 0 and c.green or c.red))
   test_out_print(' and ')
   test_out_print(coloured(pluralize(nerrors, 'error'),
                     nerrors == 0 and c.green or c.magenta))
   if nwarnings > 0 then
      test_out_print(' and ')
      test_out_print(coloured(pluralize(nwarnings, 'warning'), c.yellow))
   end
   if nskipped > 0 then
      test_out_print(' and ')
      test_out_print(coloured(nskipped .. ' disabled', c.yellow))
   end
   test_out_print('\n')

   -- Prints off a message separated by -----
   local haveSection = false
   local function addSection(text)
      local function printDashes()
         test_out_print(string.rep('-', NCOLS) .. '\n')
      end
      if not haveSection then
         printDashes()
         haveSection = true
      end
      test_out_print(text .. '\n')
      printDashes()
   end

   if not self.summaryOnly then
      for _, v in ipairs(self.errors) do
         addSection(v)
      end
      for _, v in ipairs(self.warnings) do
         addSection(v)
      end
   end
end


--[[ Tests for tensor equality between two tensors of matching sizes and types.

Tests whether the maximum element-wise difference between `ta` and `tb` is less
than or equal to `tolerance`.

Arguments:
* `ta` (tensor)
* `tb` (tensor)
* `tolerance` (number) maximum elementwise difference between `ta` and `tb`.
* `negate` (boolean) if true, we invert success and failure.
* `storage` (boolean) if true, we test_out_print an error message referring to Storages
    rather than Tensors.

Returns:
1. success, boolean that indicates success
2. failure_message, string or nil
]]
function check.areSameFormatTensorsEq(ta, tb, tolerance, negate, storage)
   local function ensureHasAbs(t)
      -- Byte, Char and Short Tensors don't have abs
      return t.abs and t or t:double()
   end

   ta = ensureHasAbs(ta)
   tb = ensureHasAbs(tb)

   local diff = ta:clone():add(-1, tb):abs()
   local err = diff:max()
   local success = err <= tolerance
   if negate then
      success = not success
   end

   local errMessage
   if not success then
      local prefix = storage and 'Storage' or 'Tensor'
      local violation = negate and 'NE(==)' or 'EQ(==)'
      errMessage = string.format('%s%s violation: max diff=%s, tolerance=%s',
                                 prefix,
                                 violation,
                                 tostring(err),
                                 tostring(tolerance))
   end

   return success, errMessage
end

--[[ Tests for tensor equality.

Tests whether the maximum element-wise difference between `ta` and `tb` is less
than or equal to `tolerance`.

Arguments:
* `ta` (tensor)
* `tb` (tensor)
* `tolerance` (number) maximum elementwise difference between `ta` and `tb`.
* `negate` (boolean) if negate is true, we invert success and failure.
* `ignoreTensorDims` (boolean, default false) if true, then tensors of the same
    size but different dimensions can still be considered equal, e.g.,
    {{1}} == {1}. For backwards compatibility.

Returns:
1. success, boolean that indicates success
2. failure_message, string or nil
]]
function check.areTensorsEq(ta, tb, tolerance, negate, ignoreTensorDims)
   ignoreTensorDims = ignoreTensorDims or false

   if not ignoreTensorDims and ta:dim() ~= tb:dim() then
      return negate, 'The tensors have different dimensions'
   end

   if ta:type() ~= tb:type() then
      return negate, 'The tensors have different types'
   end

   -- If we are comparing two empty tensors, return true.
   -- This is needed because some functions below cannot be applied to tensors
   -- of dimension 0.
   if ta:dim() == 0 and tb:dim() == 0 then
      return not negate, 'Both tensors are empty'
   end

   local sameSize
   if ignoreTensorDims then
      sameSize = ta:nElement() == tb:nElement()
   else
      sameSize = ta:isSameSizeAs(tb)
   end
   if not sameSize then
      return negate, 'The tensors have different sizes'
   end

   return check.areSameFormatTensorsEq(ta, tb, tolerance, negate, false)
end

local typesMatching = {
      ['torch.ByteStorage'] = torch.ByteTensor,
      ['torch.CharStorage'] = torch.CharTensor,
      ['torch.ShortStorage'] = torch.ShortTensor,
      ['torch.IntStorage'] = torch.IntTensor,
      ['torch.LongStorage'] = torch.LongTensor,
      ['torch.FloatStorage'] = torch.FloatTensor,
      ['torch.DoubleStorage'] = torch.DoubleTensor,
      ['torch.HalfStorage'] = torch.HalfTensor,
}

--[[ Tests for storage equality.

Tests whether the maximum element-wise difference between `sa` and `sb` is less
than or equal to `tolerance`.

Arguments:
* `sa` (storage)
* `sb` (storage)
* `tolerance` (number) maximum elementwise difference between `a` and `b`.
* `negate` (boolean) if negate is true, we invert success and failure.

Returns:
1. success, boolean that indicates success
2. failure_message, string or nil
]]
function check.areStoragesEq(sa, sb, tolerance, negate)
   if sa:size() ~= sb:size() then
      return negate, 'The storages have different sizes'
   end

   local typeOfsa = torch.type(sa)
   local typeOfsb = torch.type(sb)

   if typeOfsa ~= typeOfsb then
      return negate, 'The storages have different types'
   end

   local ta = typesMatching[typeOfsa](sa)
   local tb = typesMatching[typeOfsb](sb)

   return check.areSameFormatTensorsEq(ta, tb, tolerance, negate, true)
end

--[[ Tests for general (deep) equality.

The types of `got` and `expected` must match.
Tables are compared recursively. Keys and types of the associated values must
match, recursively. Numbers are compared with the given tolerance.
Torch tensors and storages are compared with the given tolerance on their
elementwise difference. Other types are compared for strict equality with the
regular Lua == operator.

Arguments:
* `got`
* `expected`
* `tolerance` (number) maximum elementwise difference between `a` and `b`.
* `negate` (boolean) if negate is true, we invert success and failure.

Returns:
1. success, boolean that indicates success
2. failure_message, string or nil
]]
function check.areEq(got, expected, tolerance, negate)
   local errMessage
   if type(got) ~= type(expected) then
      if not negate then
         errMessage = 'EQ failed: values have different types (first: '
                      .. type(got) .. ', second: ' .. type(expected) .. ')'
      end
      return negate, errMessage
   elseif type(got) == 'number' then
      local diff = math.abs(got - expected)
      local ok = (diff <= tolerance)
      if negate then
         ok = not ok
      end
      if not ok then
         if negate then
            errMessage = string.format("NE failed: %s == %s",
                                       tostring(got), tostring(expected))
         else
            errMessage = string.format("EQ failed: %s ~= %s",
                                       tostring(got), tostring(expected))
         end
         if tolerance > 0 then
            errMessage = errMessage .. " with tolerance=" .. tostring(tolerance)
         end
      end
      return ok, errMessage
   elseif type(expected) == "table" then
     return check.areTablesEq(got, expected, tolerance, negate)
   elseif torch.isTensor(got) then
     return check.areTensorsEq(got, expected, tolerance, negate)
   elseif torch.isStorage(got) then
     return check.areStoragesEq(got, expected, tolerance, negate)
   else
     -- Below: we have the same type which is either userdata or a lua type
     -- which is not a number.
     local ok = (got == expected)
     if negate then
        ok = not ok
     end
     if not ok then
        if negate then
           errMessage = string.format("NE failed: %s (%s) == %s (%s)",
                                      tostring(got), type(got),
                                      tostring(expected), type(expected))
        else
           errMessage = string.format("EQ failed: %s (%s) ~= %s (%s)",
                                      tostring(got), type(got),
                                      tostring(expected), type(expected))
        end
     end
     return ok, errMessage
   end
end

--[[ Tests for (deep) table equality.

Tables are compared recursively. Keys and types of the associated values must
match, recursively. Numbers are compared with the given tolerance.
Torch tensors and storages are compared with the given tolerance on their
elementwise difference. Other types are compared for strict equality with the
regular Lua == operator.

Arguments:
* `t1` (table)
* `t2` (table)
* `tolerance` (number) maximum elementwise difference between `a` and `b`.
* `negate` (boolean) if negate is true, we invert success and failure.

Returns:
1. success, boolean that indicates success
2. failure_message, string or nil
]]
function check.areTablesEq(t1, t2, tolerance, negate)
   -- Implementation detail: Instead of doing a depth-first table comparison
   -- check (for example, using recursion), let's do a breadth-first search
   -- using a queue. Why? Because if we have two tables that are quite deep
   -- (e.g., a gModule from nngraph), then if they are different then it's
   -- more useful to the user to show how they differ at as-shallow-a-depth
   -- as possible.
   local queue = {}
   queue._head = 1
   queue._tail = 1
   function queue.isEmpty()
      return queue._tail == queue._head
   end
   function queue.pop()
      queue._head = queue._head + 1
      return queue[queue._head - 1]
   end
   function queue.push(value)
      queue[queue._tail] = value
      queue._tail = queue._tail + 1
   end

   queue.push({t1, t2})
   while not queue.isEmpty() do
      local location
      t1, t2, location = unpack(queue.pop())

      local function toSublocation(key)
         local keyAsString = tostring(key)
         return (location and location .. "." .. keyAsString) or keyAsString
      end

      for key, value1 in pairs(t1) do
         local sublocation = toSublocation(key)
         if t2[key] == nil then
            return negate, string.format(
                  "Entry %s missing in second table (is %s in first)",
                  sublocation, tostring(value1))
         end
         local value2 = t2[key]
         if type(value1) == 'table' and type(value2) == 'table' then
            queue.push({value1, value2, sublocation})
         else
            local ok, message = check.areEq(value1, value2, tolerance, false)
            if not ok then
               message = 'At table location ' .. sublocation .. ': ' .. message
               return negate, message
            end
         end
      end

      for key, value2 in pairs(t2) do
         local sublocation = toSublocation(key)
         if t1[key] == nil then
             return negate, string.format(
                   "Entry %s missing in first table (is %s in second)",
                   sublocation, tostring(value2))
         end
      end
   end
   return not negate, 'The tables are equal'
end

---- test_Tester.lua

local test_tester = Tester:new()

local MESSAGE = "a really useful informative error message"

local test_subtester = Tester:new()
-- The message only interests us in case of failure
test_subtester._success = function(self) return true, MESSAGE end
test_subtester._failure = function(self, message) return false, message end

local test_testsuite = TestSuite:new()

local test_name_passed_to_setUp
local calls_to_setUp = 0
local calls_to_tearDown = 0


local originalIoWrite = test_out_print
local function disableIoWrite()
   test_out_print = function() end
end
local function enableIoWrite()
   test_out_print = originalIoWrite
end

local function meta_assert_success(success, message)
   test_tester:assert(success == true, "assert wasn't successful")
   test_tester:assert(string.find(message, MESSAGE) ~= nil, "message doesn't match")
end
local function meta_assert_failure(success, message)
   test_tester:assert(success == false, "assert didn't fail")
   test_tester:assert(string.find(message, MESSAGE) ~= nil, "message doesn't match")
end

function test_testsuite.really_test_assert()
   assert((test_subtester:assert(true, MESSAGE)),
          "test_subtester:assert doesn't actually work!")
   assert(not (test_subtester:assert(false, MESSAGE)),
          "test_subtester:assert doesn't actually work!")
end

function test_testsuite.setEarlyAbort()
   disableIoWrite()

   for _, earlyAbort in ipairs{false, true} do
      local myTester = Tester:new()

      local invokedCount = 0
      local myTests = {}
      function myTests.t1()
         invokedCount = invokedCount + 1
         myTester:assert(false)
      end
      myTests.t2 = myTests.t1

      myTester:setEarlyAbort(earlyAbort)
      myTester:add(myTests)
      pcall(myTester.run, myTester)

      test_tester:assert(invokedCount == (earlyAbort and 1 or 2),
                    "wrong number of test_testsuite invoked for use with earlyAbort")
   end

   enableIoWrite()
end

function test_testsuite.setRethrowErrors()
   disableIoWrite()

   local myTester = Tester:new()
   myTester:setRethrowErrors(true)
   myTester:add(function() error("a throw") end)

   test_tester:assertErrorPattern(function() myTester:run() end,
                             "a throw",
                             "error should be rethrown")

   enableIoWrite()
end

function test_testsuite.disable()
   disableIoWrite()

   for disableCount = 1, 2 do
      local myTester = Tester:new()
      local test_testsuite = {}
      local test1Invoked = false
      local test2Invoked = false
      function test_testsuite.test1()
         test1Invoked = true
      end
      function test_testsuite.test2()
         test2Invoked = true
      end
      myTester:add(test_testsuite)

      if disableCount == 1 then
         myTester:disable('test1'):run()
         test_tester:assert((not test1Invoked) and test2Invoked,
                       "disabled test shouldn't have been invoked")
      else
         myTester:disable({'test1', 'test2'}):run()
         test_tester:assert((not test1Invoked) and (not test2Invoked),
                       "disabled test_testsuite shouldn't have been invoked")
      end
   end

   enableIoWrite()
end

function test_testsuite.assert()
   meta_assert_success(test_subtester:assert(true, MESSAGE))
   meta_assert_failure(test_subtester:assert(false, MESSAGE))
end

local function testEqNe(eqExpected, ...)
   if eqExpected then
      meta_assert_success(test_subtester:eq(...))
      meta_assert_failure(test_subtester:ne(...))
   else
      meta_assert_failure(test_subtester:eq(...))
      meta_assert_success(test_subtester:ne(...))
   end
end

--[[ Test :assertGeneralEq and :assertGeneralNe (also known as :eq and :ne).

Note that in-depth testing of testing of many specific types of data (such as
Tensor) is covered below, when we test specific functions (such as
:assertTensorEq). This just does a general check, as well as testing of testing
of mixed datatypes.
]]
function test_testsuite.assertGeneral()
   local one = torch.Tensor{1}

   testEqNe(true, one, one, MESSAGE)
   testEqNe(false, one, 1, MESSAGE)
   testEqNe(true, "hi", "hi", MESSAGE)
   testEqNe(true, {one, 1}, {one, 1}, MESSAGE)
   testEqNe(true, {{{one}}}, {{{one}}}, MESSAGE)
   testEqNe(false, {{{one}}}, {{one}}, MESSAGE)
   testEqNe(true, torch.Storage{1}, torch.Storage{1}, MESSAGE)
   testEqNe(false, torch.FloatStorage{1}, torch.LongStorage{1}, MESSAGE)
   testEqNe(false, torch.Storage{1}, torch.Storage{1, 2}, MESSAGE)
   testEqNe(false, "one", 1, MESSAGE)
   testEqNe(false, {one}, {one + torch.Tensor{1e-10}}, MESSAGE)
   testEqNe(true, {one}, {one + torch.Tensor{1e-10}}, 1e-9, MESSAGE)
end

function test_testsuite.assertlt()
   meta_assert_success(test_subtester:assertlt(1, 2, MESSAGE))
   meta_assert_failure(test_subtester:assertlt(2, 1, MESSAGE))
   meta_assert_failure(test_subtester:assertlt(1, 1, MESSAGE))
end

function test_testsuite.assertgt()
   meta_assert_success(test_subtester:assertgt(2, 1, MESSAGE))
   meta_assert_failure(test_subtester:assertgt(1, 2, MESSAGE))
   meta_assert_failure(test_subtester:assertgt(1, 1, MESSAGE))
end

function test_testsuite.assertle()
   meta_assert_success(test_subtester:assertle(1, 2, MESSAGE))
   meta_assert_failure(test_subtester:assertle(2, 1, MESSAGE))
   meta_assert_success(test_subtester:assertle(1, 1, MESSAGE))
end

function test_testsuite.assertge()
   meta_assert_success(test_subtester:assertge(2, 1, MESSAGE))
   meta_assert_failure(test_subtester:assertge(1, 2, MESSAGE))
   meta_assert_success(test_subtester:assertge(1, 1, MESSAGE))
end

function test_testsuite.asserteq()
   meta_assert_success(test_subtester:asserteq(1, 1, MESSAGE))
   meta_assert_failure(test_subtester:asserteq(1, 2, MESSAGE))
end

function test_testsuite.assertalmosteq()
   meta_assert_success(test_subtester:assertalmosteq(1, 1, MESSAGE))
   meta_assert_success(test_subtester:assertalmosteq(1, 1 + 1e-17, MESSAGE))
   meta_assert_success(test_subtester:assertalmosteq(1, 2, 2, MESSAGE))
   meta_assert_failure(test_subtester:assertalmosteq(1, 2, MESSAGE))
   meta_assert_failure(test_subtester:assertalmosteq(1, 3, 1, MESSAGE))
end

function test_testsuite.assertne()
   meta_assert_success(test_subtester:assertne(1, 2, MESSAGE))
   meta_assert_failure(test_subtester:assertne(1, 1, MESSAGE))
end

-- The `alsoTestEq` flag is provided to test :eq in addition to :assertTensorEq.
-- The behaviour of the two isn't always the same due to handling of tensors of
-- different dimensions but the same number of elements.
local function testTensorEqNe(eqExpected, alsoTestEq, ...)
   if eqExpected then
      meta_assert_success(test_subtester:assertTensorEq(...))
      meta_assert_failure(test_subtester:assertTensorNe(...))
      if alsoTestEq then
         meta_assert_success(test_subtester:eq(...))
         meta_assert_failure(test_subtester:ne(...))
      end
   else
      meta_assert_failure(test_subtester:assertTensorEq(...))
      meta_assert_success(test_subtester:assertTensorNe(...))
      if alsoTestEq then
         meta_assert_failure(test_subtester:eq(...))
         meta_assert_success(test_subtester:ne(...))
      end
   end
end

function test_testsuite.assertTensor_types()
   local allTypes = {
         torch.ByteTensor,
         torch.CharTensor,
         torch.ShortTensor,
         torch.IntTensor,
         torch.LongTensor,
         torch.FloatTensor,
         torch.DoubleTensor,
   }
   for _, tensor1 in ipairs(allTypes) do
      for _, tensor2 in ipairs(allTypes) do
         local t1 = tensor1():ones(10)
         local t2 = tensor2():ones(10)
         testTensorEqNe(tensor1 == tensor2, true, t1, t2, 1e-6, MESSAGE)
      end
   end

   testTensorEqNe(false, true, torch.FloatTensor(), torch.LongTensor(), MESSAGE)
end

function test_testsuite.assertTensor_sizes()
   local t = torch.Tensor() -- no dimensions
   local t2 = torch.ones(2)
   local t3 = torch.ones(3)
   local t12 = torch.ones(1, 2)
   assert(test_subtester._assertTensorEqIgnoresDims == true) -- default state
   testTensorEqNe(false, false, t, t2, 1e-6, MESSAGE)
   testTensorEqNe(false, false, t, t3, 1e-6, MESSAGE)
   testTensorEqNe(false, false, t, t12, 1e-6, MESSAGE)
   testTensorEqNe(false, false, t2, t3, 1e-6, MESSAGE)
   testTensorEqNe(true, false, t2, t12, 1e-6, MESSAGE)
   testTensorEqNe(false, false, t3, t12, 1e-6, MESSAGE)
   test_subtester._assertTensorEqIgnoresDims = false
   testTensorEqNe(false, true, t, t2, 1e-6, MESSAGE)
   testTensorEqNe(false, true, t, t3, 1e-6, MESSAGE)
   testTensorEqNe(false, true, t, t12, 1e-6, MESSAGE)
   testTensorEqNe(false, true, t2, t3, 1e-6, MESSAGE)
   testTensorEqNe(false, true, t2, t12, 1e-6, MESSAGE)
   testTensorEqNe(false, true, t3, t12, 1e-6, MESSAGE)
   test_subtester._assertTensorEqIgnoresDims = true -- reset back
end

function test_testsuite.assertTensor_epsilon()
   local t1 = torch.rand(100, 100)
   local t2 = torch.rand(100, 100) * 1e-5
   local t3 = t1 + t2
   testTensorEqNe(true, true, t1, t3, 1e-4, MESSAGE)
   testTensorEqNe(false, true, t1, t3, 1e-6, MESSAGE)
end

function test_testsuite.assertTensor_arg()
   local one = torch.Tensor{1}

   test_tester:assertErrorPattern(
         function() test_subtester:assertTensorEq(one, 2) end,
         "Second argument should be a Tensor")

   -- Test that assertTensorEq support message and tolerance in either ordering
   test_tester:assertNoError(
         function() test_subtester:assertTensorEq(one, one, 0.1, MESSAGE) end)
   test_tester:assertNoError(
         function() test_subtester:assertTensorEq(one, one, MESSAGE, 0.1) end)
end

function test_testsuite.assertTensor()
   local t1 = torch.randn(100, 100)
   local t2 = t1:clone()
   local t3 = torch.randn(100, 100)
   testTensorEqNe(true, true, t1, t2, 1e-6, MESSAGE)
   testTensorEqNe(false, true, t1, t3, 1e-6, MESSAGE)
   testTensorEqNe(true, true, torch.Tensor(), torch.Tensor(), MESSAGE)
end

-- Check that calling assertTensorEq with two tensors with the same content but
-- different dimensions gives a warning.
function test_testsuite.assertTensorDimWarning()
   local myTester = Tester:new()
   myTester:add(
       function()
          myTester:assertTensorEq(torch.Tensor{{1}}, torch.Tensor{1})
       end)

   local warningGiven = false
   test_out_print = function(s)
      if string.match(s, 'but different dimensions') then
         warningGiven = true
      end
   end

   myTester:run()
   enableIoWrite()

   test_tester:assert(warningGiven,
                 "Calling :assertTensorEq({{1}}, {1}) should give a warning")
end

local function testTableEqNe(eqExpected, ...)
   if eqExpected then
      meta_assert_success(test_subtester:assertTableEq(...))
      meta_assert_failure(test_subtester:assertTableNe(...))
      meta_assert_success(test_subtester:eq(...))
      meta_assert_failure(test_subtester:ne(...))
   else
      meta_assert_failure(test_subtester:assertTableEq(...))
      meta_assert_success(test_subtester:assertTableNe(...))
      meta_assert_failure(test_subtester:eq(...))
      meta_assert_success(test_subtester:ne(...))
   end
end

function test_testsuite.assertTable()
   testTableEqNe(true, {1, 2, 3}, {1, 2, 3}, MESSAGE)
   testTableEqNe(false, {1, 2, 3}, {3, 2, 1}, MESSAGE)
   testTableEqNe(true, {1, 2, {4, 5}}, {1, 2, {4, 5}}, MESSAGE)
   testTableEqNe(false, {1, 2, 3}, {1,2}, MESSAGE)
   testTableEqNe(false, {1, 2, 3}, {1, 2, 3, 4}, MESSAGE)
   testTableEqNe(true, {{1}}, {{1}}, MESSAGE)
   testTableEqNe(false, {{1}}, {{{1}}}, MESSAGE)
   testTableEqNe(true, {false}, {false}, MESSAGE)
   testTableEqNe(false, {true}, {false}, MESSAGE)
   testTableEqNe(false, {false}, {true}, MESSAGE)

   local tensor = torch.rand(100, 100)
   local t1 = {1, "a", key = "value", tensor = tensor, subtable = {"nested"}}
   local t2 = {1, "a", key = "value", tensor = tensor, subtable = {"nested"}}
   testTableEqNe(true, t1, t2, MESSAGE)
   for k, v in pairs(t1) do
      local x = "something else"
      t2[k] = nil
      t2[x] = v
      testTableEqNe(false, t1, t2, MESSAGE)
      t2[x] = nil
      t2[k] = x
      testTableEqNe(false, t1, t2, MESSAGE)
      t2[k] = v
      testTableEqNe(true, t1, t2, MESSAGE)
   end
end

local function good_fn() end
local function bad_fn() error("muahaha!") end

function test_testsuite.assertError()
   meta_assert_success(test_subtester:assertError(bad_fn, MESSAGE))
   meta_assert_failure(test_subtester:assertError(good_fn, MESSAGE))
end

function test_testsuite.assertNoError()
   meta_assert_success(test_subtester:assertNoError(good_fn, MESSAGE))
   meta_assert_failure(test_subtester:assertNoError(bad_fn, MESSAGE))
end

function test_testsuite.assertErrorPattern()
   meta_assert_success(test_subtester:assertErrorPattern(bad_fn, "haha", MESSAGE))
   meta_assert_failure(test_subtester:assertErrorPattern(bad_fn, "hehe", MESSAGE))
end

function test_testsuite.testSuite_duplicateTests()
   local function createDuplicateTests()
      local test_testsuite = TestSuite:new()
      function test_testsuite.testThis() end
      function test_testsuite.testThis() end
   end
   test_tester:assertErrorPattern(createDuplicateTests,
                             "Test testThis is already defined.")
end

--[[ Returns a Tester with `numSuccess` success cases, `numFailure` failure
  cases, and with an error if `hasError` is true.
  Success and fail test_testsuite are evaluated with test_tester:eq
]]
local function genDummyTest(numSuccess, numFailure, hasError)
   hasError = hasError or false

   local dummyTester = Tester:new()
   local dummyTests = TestSuite:new()

   if numSuccess > 0 then
      function dummyTests.testDummySuccess()
         for i = 1, numSuccess do
           dummyTester:eq({1}, {1}, '', 0)
         end
      end
   end

   if numFailure > 0 then
      function dummyTests.testDummyFailure()
         for i = 1, numFailure do
            dummyTester:eq({1}, {2}, '', 0)
         end
      end
   end

   if hasError then
      function dummyTests.testDummyError()
         error('dummy error')
      end
   end

   return dummyTester:add(dummyTests)
end

function test_testsuite.runStatusAndAssertCounts()
   local emptyTest      = genDummyTest(0, 0, false)
   local sucTest        = genDummyTest(1, 0, false)
   local multSucTest    = genDummyTest(4, 0, false)
   local failTest       = genDummyTest(0, 1, false)
   local errTest        = genDummyTest(0, 0, true)
   local errFailTest    = genDummyTest(0, 1, true)
   local errSucTest     = genDummyTest(1, 0, true)
   local failSucTest    = genDummyTest(1, 1, false)
   local failSucErrTest = genDummyTest(1, 1, true)

   disableIoWrite()

   local success, msg = pcall(emptyTest.run, emptyTest)
   test_tester:asserteq(success, true, "pcall should succeed for empty test_testsuite")

   local success, msg = pcall(sucTest.run, sucTest)
   test_tester:asserteq(success, true, "pcall should succeed for 1 successful test")

   local success, msg = pcall(multSucTest.run, multSucTest)
   test_tester:asserteq(success, true,
                   "pcall should succeed for 2+ successful test_testsuite")

   local success, msg = pcall(failTest.run, failTest)
   test_tester:asserteq(success, false, "pcall should fail for test_testsuite with failure")

   local success, msg = pcall(errTest.run, errTest)
   test_tester:asserteq(success, false, "pcall should fail for test_testsuite with error")

   local success, msg = pcall(errFailTest.run, errFailTest)
   test_tester:asserteq(success, false, "pcall should fail for error+fail test_testsuite")

   local success, msg = pcall(errSucTest.run, errSucTest)
   test_tester:asserteq(success, false, "pcall should fail for error+success test_testsuite")

   local success, msg = pcall(failSucTest.run, failSucTest)
   test_tester:asserteq(success, false, "pcall should fail for fail+success test_testsuite")

   local success, msg = pcall(failSucErrTest.run, failSucErrTest)
   test_tester:asserteq(success, false,
                   "pcall should fail for fail+success+err test")

   enableIoWrite()

   test_tester:asserteq(emptyTest.countasserts, 0,
                   "emptyTest should have 0 asserts")
   test_tester:asserteq(sucTest.countasserts, 1, "sucTest should have 1 assert")
   test_tester:asserteq(multSucTest.countasserts, 4,
                   "multSucTest should have 4 asserts")
   test_tester:asserteq(failTest.countasserts, 1, "failTest should have 1 assert")
   test_tester:asserteq(errTest.countasserts, 0, "errTest should have 0 asserts")
   test_tester:asserteq(errFailTest.countasserts, 1,
                   "errFailTest should have 1 assert")
   test_tester:asserteq(errSucTest.countasserts, 1,
                   "errSucTest should have 0 asserts")
   test_tester:asserteq(failSucTest.countasserts, 2,
                   "failSucTest should have 2 asserts")
end

function test_testsuite.checkNestedTestsForbidden()
   disableIoWrite()

   local myTester = Tester:new()
   local myTests = {{function() end}}
   test_tester:assertErrorPattern(function() myTester:add(myTests) end,
                             "Nested sets",
                             "test_tester should forbid adding nested test sets")

   enableIoWrite()
end

function test_testsuite.checkWarningOnAssertObject()
   -- This test checks that calling assert with an object generates a warning
   local myTester = Tester:new()
   local myTests = {}
   function myTests.assertAbuse()
      myTester:assert({})
   end
   myTester:add(myTests)

   local warningGiven = false
   test_out_print = function(s)
      if string.match(s, 'should only be used for boolean') then
         warningGiven = true
      end
   end

   myTester:run()
   enableIoWrite()

   test_tester:assert(warningGiven, "Should warn on calling :assert(object)")
end

function test_testsuite.checkWarningOnAssertNeObject()
   -- This test checks that calling assertne with two objects generates warning
   local myTester = Tester:new()
   local myTests = {}
   function myTests.assertAbuse()
      myTester:assertne({}, {})
   end
   myTester:add(myTests)

   local warningGiven = false
   test_out_print = function(s)
      if string.match(s, 'assertne should only be used to compare basic') then
         warningGiven = true
      end
   end

   myTester:run()
   enableIoWrite()

   test_tester:assert(warningGiven, "Should warn on calling :assertne(obj, obj)")
end

function test_testsuite.checkWarningOnExtraAssertArguments()
   -- This test checks that calling assert with extra args gives a lua error
   local myTester = Tester:new()
   local myTests = {}
   function myTests.assertAbuse()
      myTester:assert(true, "some message", "extra argument")
   end
   myTester:add(myTests)

   local errorGiven = false
   test_out_print = function(s)
      if string.match(s, 'Unexpected arguments') then
         errorGiven = true
      end
   end
   test_tester:assertError(function() myTester:run() end)
   enableIoWrite()

   test_tester:assert(errorGiven, ":assert should fail on extra arguments")
end

function test_testsuite.checkWarningOnUsingTable()
   -- Checks that if we don't use a TestSuite then gives a warning
   local myTester = Tester:new()
   local myTests = {}
   myTester:add(myTests)

   local errorGiven = false
   test_out_print = function(s)
      if string.match(s, 'use TestSuite rather than plain lua table') then
         errorGiven = true
      end
   end
   myTester:run()

   enableIoWrite()
   test_tester:assert(errorGiven, "Using a plain lua table for testsuite should warn")
end

function test_testsuite.checkMaxAllowedSetUpAndTearDown()
   -- Checks can have at most 1 set-up and at most 1 tear-down function
   local function f() end
   local myTester = Tester:new()

   for _, name in ipairs({'_setUp', '_tearDown'}) do
      test_tester:assertNoError(function() myTester:add(f, name) end,
                           "Adding 1 set-up / tear-down should be fine")
      test_tester:assertErrorPattern(function() myTester:add(f, name) end,
                                "Only one",
                                "Adding second set-up / tear-down should fail")
   end
end

function test_testsuite.test_setUp()
   test_tester:asserteq(test_name_passed_to_setUp, 'test_setUp')
   for key, value in pairs(test_tester.tests) do
      test_tester:assertne(key, '_setUp')
   end
end

function test_testsuite.test_tearDown()
   for key, value in pairs(test_tester.tests) do
      test_tester:assertne(key, '_tearDown')
   end
end

function test_testsuite._setUp(name)
   test_name_passed_to_setUp = name
   calls_to_setUp = calls_to_setUp + 1
end

function test_testsuite._tearDown(name)
   calls_to_tearDown = calls_to_tearDown + 1
end

test_tester:add(test_testsuite):run()

-- Additional test_testsuite to check that _setUp and _tearDown were called.
local test_count = 0
for _ in pairs(test_tester.tests) do
   test_count = test_count + 1
end
local postTests = TestSuite:new()
local postTester = Tester:new()

function postTests.test_setUp(test_tester)
   postTester:asserteq(calls_to_setUp, test_count,
                       "Expected " .. test_count .. " calls to _setUp")
end

function postTests.test_tearDown()
   postTester:asserteq(calls_to_tearDown, test_count,
                      "Expected " .. test_count .. " calls to _tearDown")
end

postTester:add(postTests):run()

---- for not self-testing

local tester = Tester:new()

---- test_aliasMultinormal.lua

local function aliasMultinomial()
   local n_class = 10000
   local probs = torch.Tensor(n_class):uniform(0,1)
   probs:div(probs:sum())
   local a = torch.Timer()
   local state = torch.multinomialAliasSetup(probs)
   test_out_print("AliasMultinomial setup in "..a:time().real.." seconds(hot)")
   a:reset()
   state = torch.multinomialAliasSetup(probs, state)
   test_out_print("AliasMultinomial setup in "..a:time().real.." seconds(cold)")
   a:reset()
   
   tester:assert(state[1]:min() >= 0, "Index ="..state[1]:min().."alias indices has an index below or equal to 0")
   tester:assert(state[1]:max() <= n_class, state[1]:max().." alias indices has an index exceeding num_class")
   local output = torch.LongTensor(1000000)
   torch.multinomialAlias(output, state)
   local n_samples = output:nElement()
   test_out_print("AliasMultinomial draw "..n_samples.." elements from "..n_class.." classes ".."in "..a:time().real.." seconds")
   local counts = torch.Tensor(n_class):zero()
   local mult_output = torch.multinomial(probs, n_samples, true)
   test_out_print("Multinomial draw "..n_samples.." elements from "..n_class.." classes ".." in "..a:time().real.." seconds")
   tester:assert(output:min() > 0, "sampled indices has an index below or equal to 0")
   tester:assert(output:max() <= n_class, "indices has an index exceeding num_class")
   output:apply(function(x)
         counts[x] = counts[x] + 1
   end)
   a:reset()
   
   counts:div(counts:sum())
   
   tester:assert(state[1]:min() >= 0, "Index ="..state[1]:min().."alias indices has an index below or equal to 0")
   tester:assert(state[1]:max() <= n_class, state[1]:max().." alias indices has an index exceeding num_class")
   tester:eq(probs, counts, 0.001, "probs and counts should be approximately equal")
end

tester:add(aliasMultinomial, "aliasMultinomial")

---- test_half.lua

local half_test = TestSuite:new()

function half_test.half_test_easy()
   local x=torch.randn(5, 6):half()
   tester:assert(x:isContiguous(), 'x should be contiguous')
   tester:assert(x:dim() == 2, 'x should have dim of 2')
   tester:assert(x:nDimension() == 2, 'x should have nDimension of 2')
   tester:assert(x:nElement() == 5 * 6, 'x should have 30 elements')
   local stride = x:stride()
   local expectedStride = torch.LongStorage{6,1}
   for i=1,stride:size() do
      tester:assert(stride[i] == expectedStride[i], "stride is wrong")
   end

   x=x:t()
   tester:assert(not x:isContiguous(), 'x transpose should not be contiguous')
   x=x:transpose(1,2)
   tester:assert(x:isContiguous(), 'x should be contiguous after 2 transposes')

   local y=torch.HalfTensor()
   y:resizeAs(x:t()):copy(x:t())
   tester:assert(x:isContiguous(), 'after resize and copy, x should be contiguous')
   tester:assertTensorEq(y, x:t(), 0.001, 'copy broken after resizeAs')
   local z=torch.HalfTensor()
   z:resize(6, 5):copy(x:t())
   tester:assertTensorEq(y, x:t(), 0.001, 'copy broken after resize')
end

function half_test.half_test_narrowSub()
   local x = torch.randn(5, 6):half()
   local narrow = x:narrow(1, 2, 3)
   local sub = x:sub(2, 4)
   tester:assertTensorEq(narrow, sub, 0.001, 'narrow not equal to sub')
end

function half_test.half_test_selectClone()
   local x = torch.zeros(5, 6)
   x:select(1,2):fill(2)
   x=x:half()
   local y=x:clone()
   tester:assertTensorEq(x, y, 0.001, 'not equal after select and clone')
   x:select(1,1):fill(3)
   tester:assert(y[1][1] == 0, 'clone broken')
end

tester:add(half_test)

---- test_qr.lua

local qr_test = TestSuite:new()

-- torch.qr() with result tensors given.
local function qrInPlace(tensorFunc)
  return function(x)
    local q, r = tensorFunc(), tensorFunc()
    torch.qr(q, r, x:clone())
    return q, r
  end
end

-- torch.qr() without result tensors given.
local function qrReturned(tensorFunc)
  return function(x)
    return torch.qr(x:clone())
  end
end

-- torch.geqrf() with result tensors given.
local function geqrfInPlace(tensorFunc)
  return function(x)
    local result = tensorFunc()
    local tau = tensorFunc()
    local result_, tau_ = torch.geqrf(result, tau, x)
    assert(torch.pointer(result) == torch.pointer(result_),
           'expected result, result_ same tensor')
    assert(torch.pointer(tau) == torch.pointer(tau_),
           'expected tau, tau_ same tensor')
    return result_, tau_
  end
end

-- torch.orgqr() with result tensors given.
local function orgqrInPlace(tensorFunc)
  return function(result, tau)
    local q = tensorFunc()
    local q_ = torch.orgqr(q, result, tau)
    assert(torch.pointer(q) == torch.pointer(q_), 'expected q, q_ same tensor')
    return q
  end
end

-- Test a custom QR routine that calls the LAPACK functions manually.
local function qrManual(geqrfFunc, orgqrFunc)
  return function(x)
    local m = x:size(1)
    local n = x:size(2)
    local k = math.min(m, n)
    local result, tau = geqrfFunc(x)
    assert(result:size(1) == m)
    assert(result:size(2) == n)
    assert(tau:size(1) == k)
    local r = torch.triu(result:narrow(1, 1, k))
    local q = orgqrFunc(result, tau)
    return q:narrow(2, 1, k), r
  end
end

-- Check that Q multiplied with a matrix with ormqr gives the correct result
local function checkQM(testOpts, mat1, mat2)
  local q, r = torch.qr(mat1)
  local m, tau = torch.geqrf(mat1)
  local requiredPrecision = 1e-5
  tester:assertTensorEq(torch.mm(q, mat2), torch.ormqr(m, tau, mat2),
                        requiredPrecision)
  tester:assertTensorEq(torch.mm(mat2, q), torch.ormqr(m, tau, mat2, 'R'),
                        requiredPrecision)
  tester:assertTensorEq(torch.mm(q:t(), mat2),
                        torch.ormqr(m, tau, mat2, 'L', 'T'), requiredPrecision)
  tester:assertTensorEq(torch.mm(mat2, q:t()),
                        torch.ormqr(m, tau, mat2, 'R', 'T'), requiredPrecision)
end

-- Check that the given `q`, `r` matrices are a valid QR decomposition of `a`.
local function checkQR(testOpts, a, q, r)
  local qrFunc = testOpts.qr
  if not q then
    q, r = qrFunc(a)
  end
  local k = math.min(a:size(1), a:size(2))
  tester:asserteq(q:size(1), a:size(1), "Bad size for q first dimension.")
  tester:asserteq(q:size(2), k, "Bad size for q second dimension.")
  tester:asserteq(r:size(1), k, "Bad size for r first dimension.")
  tester:asserteq(r:size(2), a:size(2), "Bad size for r second dimension.")
  tester:assertTensorEq(q:t() * q,
                        torch.eye(q:size(2)):typeAs(testOpts.tensorFunc()),
                        testOpts.precision,
                        "Q was not orthogonal")
  tester:assertTensorEq(r, r:triu(), testOpts.precision,
                        "R was not upper triangular")
  tester:assertTensorEq(q * r, a, testOpts.precision, "QR = A")
end

-- Do a QR decomposition of `a` and check that the result is valid and matches
-- the given expected `q` and `r`.
local function checkQRWithExpected(testOpts, a, expected_q, expected_r)
  local qrFunc = testOpts.qr
  -- Since the QR decomposition is unique only up to the signs of the rows of
  -- R, we must ensure these are positive before doing the comparison.
  local function canonicalize(q, r)
      local d = r:diag():sign():diag()
      return q * d, d * r
  end
  local q, r = qrFunc(a)
  local q_canon, r_canon = canonicalize(q, r)
  local expected_q_canon, expected_r_canon
      = canonicalize(expected_q, expected_r)
  tester:assertTensorEq(q_canon, expected_q_canon, testOpts.precision,
                        "Q did not match expected")
  tester:assertTensorEq(r_canon, expected_r_canon, testOpts.precision,
                        "R did not match expected")
  checkQR(testOpts, a, q, r)
end

-- Generate a separate test based on `func` for each of the possible
-- combinations of tensor type (double or float) and QR function (torch.qr
-- in-place, torch.qr, and manually calling the geqrf and orgqr from Lua
-- (both in-place and not).
--
-- The tests are added to the given `tests` table, with names generated by
-- appending a unique string for the specific combination to `name`.
--
-- If opts.doubleTensorOnly is true, then the FloatTensor versions of the test
-- will be skipped.
local function addTestVariations(tests, name, func, opts)
  opts = opts or {}
  local tensorTypes = {
      [torch.DoubleTensor] = 1e-12,
      [torch.FloatTensor] = 1e-5,
  }
  for tensorFunc, requiredPrecision in pairs(tensorTypes) do
    local qrFuncs = {
        ['inPlace'] = qrInPlace(tensorFunc),
        ['returned'] = qrReturned(tensorFunc),
        ['manualInPlace'] = qrManual(geqrfInPlace(tensorFunc),
                                     orgqrInPlace(tensorFunc)),
        ['manualReturned'] = qrManual(torch.geqrf, torch.orgqr)
    }
    for qrName, qrFunc in pairs(qrFuncs) do
      local testOpts = {
          tensorFunc=tensorFunc,
          precision=requiredPrecision,
          qr=qrFunc,
      }
      local tensorType = tensorFunc():type()
      local fullName = name .. "_" .. qrName .. "_" .. tensorType
      assert(not tests[fullName])
      if tensorType == 'torch.DoubleTensor' or not opts.doubleTensorOnly then
        tests[fullName] = function()
          local state = torch.getRNGState()
          torch.manualSeed(1)
          func(testOpts)
          torch.setRNGState(state)
        end
      end
    end
  end
end

-- Decomposing a specific square matrix.
addTestVariations(qr_test, 'qrSquare', function(testOpts)
  return function(testOpts)
    local tensorFunc = testOpts.tensorFunc
    local a = tensorFunc{{1, 2, 3}, {4, 5, 6}, {7, 8, 9}}
    local expected_q = tensorFunc{
        {-1.230914909793328e-01,  9.045340337332914e-01,
         4.082482904638621e-01},
        {-4.923659639173310e-01,  3.015113445777629e-01,
         -8.164965809277264e-01},
        {-8.616404368553292e-01, -3.015113445777631e-01,
         4.082482904638634e-01},
    }
    local expected_r = tensorFunc{
        {-8.124038404635959e+00, -9.601136296387955e+00,
         -1.107823418813995e+01},
        { 0.000000000000000e+00,  9.045340337332926e-01,
         1.809068067466585e+00},
        { 0.000000000000000e+00,  0.000000000000000e+00,
         -8.881784197001252e-16},
    }
    checkQRWithExpected(testOpts, a,  expected_q, expected_r)
  end
end, {doubleTensorOnly=true})

-- Decomposing a specific (wide) rectangular matrix.
addTestVariations(qr_test, 'qrRectFat', function(testOpts)
  -- The matrix is chosen to be full-rank.
  local a = testOpts.tensorFunc{
      {1,  2,  3,  4},
      {5,  6,  7,  8},
      {9, 10, 11, 13}
  }
  local expected_q = testOpts.tensorFunc{
      {-0.0966736489045663,  0.907737593658436 ,  0.4082482904638653},
      {-0.4833682445228317,  0.3157348151855452, -0.8164965809277254},
      {-0.870062840141097 , -0.2762679632873518,  0.4082482904638621}
  }
  local expected_r = testOpts.tensorFunc{
      { -1.0344080432788603e+01,  -1.1794185166357092e+01,
        -1.3244289899925587e+01,  -1.5564457473635180e+01},
      {  0.0000000000000000e+00,   9.4720444555662542e-01,
         1.8944088911132546e+00,   2.5653453733825331e+00},
      {  0.0000000000000000e+00,   0.0000000000000000e+00,
         1.5543122344752192e-15,   4.0824829046386757e-01}
  }
  checkQRWithExpected(testOpts, a, expected_q, expected_r)
end, {doubleTensorOnly=true})

-- Decomposing a specific (thin) rectangular matrix.
addTestVariations(qr_test, 'qrRectThin', function(testOpts)
  -- The matrix is chosen to be full-rank.
  local a = testOpts.tensorFunc{
      { 1,  2,  3},
      { 4,  5,  6},
      { 7,  8,  9},
      {10, 11, 13},
  }
  local expected_q = testOpts.tensorFunc{
      {-0.0776150525706334, -0.833052161400748 ,  0.3651483716701106},
      {-0.3104602102825332, -0.4512365874254053, -0.1825741858350556},
      {-0.5433053679944331, -0.0694210134500621, -0.7302967433402217},
      {-0.7761505257063329,  0.3123945605252804,  0.5477225575051663}
  }
  local expected_r = testOpts.tensorFunc{
      {-12.8840987267251261, -14.5916298832790581, -17.0753115655393231},
      {  0,                  -1.0413152017509357,  -1.770235842976589 },
      {  0,                   0,                    0.5477225575051664}
  }
  checkQRWithExpected(testOpts, a, expected_q, expected_r)
end, {doubleTensorOnly=true})

-- Decomposing a sequence of medium-sized random matrices.
addTestVariations(qr_test, 'randomMediumQR', function(testOpts)
  for x = 0, 10 do
    for y = 0, 10 do
      local m = math.pow(2, x)
      local n = math.pow(2, y)
      local x = torch.rand(m, n)
      checkQR(testOpts, x:typeAs(testOpts.tensorFunc()))
    end
  end
end)

-- Decomposing a sequence of small random matrices.
addTestVariations(qr_test, 'randomSmallQR', function(testOpts)
  for m = 1, 40 do
    for n = 1, 40 do
      checkQR(testOpts, torch.rand(m, n):typeAs(testOpts.tensorFunc()))
    end
  end
end)

-- Decomposing a sequence of small matrices that are not contiguous in memory.
addTestVariations(qr_test, 'randomNonContiguous', function(testOpts)
  for m = 2, 40 do
    for n = 2, 40 do
      local x = torch.rand(m, n):t()
      tester:assert(not x:isContiguous(), "x should not be contiguous")
      checkQR(testOpts, x:typeAs(testOpts.tensorFunc()))
    end
  end
end)

function qr_test.testQM()
  checkQM({}, torch.randn(10, 10), torch.randn(10, 10))
  -- checkQM({}, torch.randn(20, 10), torch.randn(20, 20))
end

tester:add(qr_test)

---- test_timer.lua

local timer_tests = TestSuite:new()

function timer_tests.timerTime()
  local timer = torch.Timer()

  local function wait(seconds)
   luau_sleep(seconds)
  end

  timer:reset()
  wait(1)
  local passed_time = timer:time().real
  tester:assert(passed_time < 1.1,
               ("Too long time passed: %.1f sec >= 1.1 sec"):format(passed_time))
  tester:assert(passed_time > 0.9,
               ("Too short time passed:  %.1f sec <= 0.9 sec"):format(passed_time))

  timer:stop()
  wait(1)
  passed_time = timer:time().real
  tester:assert(passed_time < 1.1,
               ("Too long time passed: %.1f sec >= 1.1 sec"):format(passed_time))
  tester:assert(passed_time > 0.9,
               ("Too short time passed:  %.1f sec <= 0.9 sec"):format(passed_time))

  timer:resume()
  wait(1)
  passed_time = timer:time().real
  tester:assert(passed_time < 2.2,
               ("Too long time passed: %.1f sec >= 2.2 sec"):format(passed_time))
  tester:assert(passed_time > 1.8,
               ("Too short time passed:  %.1f sec <= 1.8 sec"):format(passed_time))

  timer:reset()
  wait(1)
  passed_time = timer:time().real
  tester:assert(passed_time < 1.1,
               ("Too long time passed: %.1f sec >= 1.1 sec"):format(passed_time))
  tester:assert(passed_time > 0.9,
               ("Too short time passed:  %.1f sec <= 0.9 sec"):format(passed_time))
end

tester:add(timer_tests)

---- test.lua

local torch_main_tests = TestSuite:new()
local msize = 100
local precision

local function maxdiff(x,y)
   local d = x-y
   if x:type() == 'torch.DoubleTensor' or x:type() == 'torch.FloatTensor' then
      return d:abs():max()
   else
      local dd = torch.Tensor():resize(d:size()):copy(d)
      return dd:abs():max()
   end
end

function torch_main_tests.dot()
   local types = {
      ['torch.DoubleTensor'] = 1e-8, -- for ddot
      ['torch.FloatTensor']  = 1e-4, -- for sdot
   }
   for tname, prec in pairs(types) do
      local v1 = torch.randn(100):type(tname)
      local v2 = torch.randn(100):type(tname)

      local res1 = torch.dot(v1,v2)

      local res2 = 0
      for i = 1,v1:size(1) do
         res2 = res2 + v1[i] * v2[i]
      end

      local err = math.abs(res1-res2)

      tester:assertlt(err, prec, 'error in torch.dot (' .. tname .. ')')
   end
end

local function genericSingleOpTest(torch_fun, ref_fun)
   -- [res] torch.functionname([res,] x)
   -- contiguous
   local m1 = torch.randn(100,100)
   local res1 = torch_fun(m1[{ 4,{} }])
   local res2 = res1:clone():zero()
   for i = 1,res1:size(1) do
      res2[i] = ref_fun(m1[4][i])
   end
   local err = res1:clone():zero()
   -- find absolute error
   for i = 1, res1:size(1) do
      err[i] = math.abs(res1[i] - res2[i])
   end
   -- find maximum element of error
   local maxerrc = 0
   for i = 1, err:size(1) do
      if err[i] > maxerrc then
         maxerrc = err[i]
      end
   end

   -- non-contiguous
   local m1 = torch.randn(100,100)
   local res1 = torch_fun(m1[{ {}, 4 }])
   local res2 = res1:clone():zero()
   for i = 1,res1:size(1) do
      res2[i] = ref_fun(m1[i][4])
   end
   local err = res1:clone():zero()
   -- find absolute error
   for i = 1, res1:size(1) do
      err[i] = math.abs(res1[i] - res2[i])
   end
   -- find maximum element of error
   local maxerrnc = 0
   for i = 1, err:size(1) do
      if err[i] > maxerrnc then
         maxerrnc = err[i]
      end
   end
   return maxerrc, maxerrnc
end

function torch_main_tests.sin()
   --local f = loadstring(string.gsub(genericSingleOpTest, 'functionname', 'sin'))
   local maxerrc, maxerrnc = genericSingleOpTest(torch.sin, math.sin)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.sinh()
   --local f = loadstring(string.gsub(genericSingleOpTest, 'functionname', 'sinh'))
   local maxerrc, maxerrnc = genericSingleOpTest(torch.sinh, math.sinh)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.asin()
   --local f = loadstring(string.gsub(genericSingleOpTest, 'functionname', 'asin'))
   local maxerrc, maxerrnc = genericSingleOpTest(torch.asin, math.asin)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.cos()
   --local f = loadstring(string.gsub(genericSingleOpTest, 'functionname', 'cos'))
   local maxerrc, maxerrnc = genericSingleOpTest(torch.cos, math.cos)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.cosh()
   --local f = loadstring(string.gsub(genericSingleOpTest, 'functionname', 'cosh'))
   local maxerrc, maxerrnc = genericSingleOpTest(torch.cosh, math.cosh)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.acos()
   --local f = loadstring(string.gsub(genericSingleOpTest, 'functionname', 'acos'))
   local maxerrc, maxerrnc = genericSingleOpTest(torch.acos, math.acos)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.tan()
   --local f = loadstring(string.gsub(genericSingleOpTest, 'functionname', 'tan'))
   local maxerrc, maxerrnc = genericSingleOpTest(torch.tan, math.tan)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.tanh()
   --local f = loadstring(string.gsub(genericSingleOpTest, 'functionname', 'tanh'))
   local maxerrc, maxerrnc = genericSingleOpTest(torch.tanh, math.tanh)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.atan()
   --local f = loadstring(string.gsub(genericSingleOpTest, 'functionname', 'atan'))
   local maxerrc, maxerrnc = genericSingleOpTest(torch.atan, math.atan)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.log()
   --local f = loadstring(string.gsub(genericSingleOpTest, 'functionname', 'log'))
   local maxerrc, maxerrnc = genericSingleOpTest(torch.log, math.log)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.sqrt()
   --local f = loadstring(string.gsub(genericSingleOpTest, 'functionname', 'sqrt'))
   local maxerrc, maxerrnc = genericSingleOpTest(torch.sqrt, math.sqrt)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.rsqrt()
   local function TH_rsqrt(x)
      return 1 / math.sqrt(x)
   end

   --local f
   --local t = genericSingleOpTest:gsub('functionname', 'rsqrt'):gsub('math.rsqrt', 'TH_rsqrt')
   --local env = { TH_rsqrt=TH_rsqrt, torch=torch, math=math }
   --if not setfenv then -- Lua 5.2
   --   f = load(t, 'test', 't', env)
   --else
   --   f = loadstring(t)
   --   setfenv(f, env)
   --end

   local maxerrc, maxerrnc = genericSingleOpTest(torch.rsqrt, TH_rsqrt)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.sigmoid()
   -- can't use genericSingleOpTest, since `math.sigmoid` doesn't exist, have to use
   -- `torch.sigmoid` instead
   local inputValues = {-1000,-1,0,0.5,1,2,1000}
   local expectedOutput = {0.0000, 0.2689, 0.5, 0.6225, 0.7311, 0.8808, 1.000}

   local precision_4dps = 0.0002

   -- float
   local inputFT = torch.FloatTensor(inputValues)
   local expectedFT = torch.FloatTensor(expectedOutput)
   tester:assertlt((torch.sigmoid(inputFT) - expectedFT):abs():max(), precision_4dps, 'error in torch.sigmoid - single')
   tester:assertlt((inputFT - torch.FloatTensor(inputValues)):abs():max(), precision_4dps, 'error in torch.sigmoid - single')
   local sigmoidFT = torch.FloatTensor(inputValues):sigmoid()
   tester:assertlt((sigmoidFT - expectedFT):abs():max(), precision_4dps, 'error in torch.sigmoid - single')

   -- double
   local inputDT = torch.DoubleTensor(inputValues)
   local expectedDT = torch.DoubleTensor(expectedOutput)
   tester:assertlt((torch.sigmoid(inputDT) - expectedDT):abs():max(), precision_4dps, 'error in torch.sigmoid - double')
   tester:assertlt((inputDT - torch.DoubleTensor(inputValues)):abs():max(), precision_4dps, 'error in torch.sigmoid - double')
   local sigmoidDT = torch.DoubleTensor(inputValues):sigmoid()
   tester:assertlt((sigmoidDT - expectedDT):abs():max(), precision_4dps, 'error in torch.sigmoid - double')
end

function torch_main_tests.exp()
   --local f = loadstring(string.gsub(genericSingleOpTest, 'functionname', 'exp'))
   local maxerrc, maxerrnc = genericSingleOpTest(torch.exp, math.exp)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.floor()
   --local f = loadstring(string.gsub(genericSingleOpTest, 'functionname', 'floor'))
   local maxerrc, maxerrnc = genericSingleOpTest(torch.floor, math.floor)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.ceil()
   --local f = loadstring(string.gsub(genericSingleOpTest, 'functionname', 'ceil'))
   local maxerrc, maxerrnc = genericSingleOpTest(torch.ceil, math.ceil)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.frac()
   local function TH_frac(x)
      return math.fmod(x, 1)
   end

   --local f
   --local t = genericSingleOpTest:gsub('functionname', 'frac'):gsub('math.frac', 'TH_frac')
   --local env = { TH_frac=TH_frac, torch=torch, math=math }
   --if not setfenv then -- Lua 5.2
   --   f = load(t, 'test', 't', env)
   --else
   --   f = loadstring(t)
   --   setfenv(f, env)
   --end

   local maxerrc, maxerrnc = genericSingleOpTest(torch.frac, TH_frac)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.trunc()
   local function TH_trunc(x)
      return x - math.fmod(x, 1)
   end

   --local f
   --local t = genericSingleOpTest:gsub('functionname', 'trunc'):gsub('math.trunc', 'TH_trunc')
   --local env = { TH_trunc=TH_trunc, torch=torch, math=math }
   --if not setfenv then -- Lua 5.2
   --   f = load(t, 'test', 't', env)
   --else
   --   f = loadstring(t)
   --   setfenv(f, env)
   --end

   local maxerrc, maxerrnc = genericSingleOpTest(torch.trunc, TH_trunc)
   tester:assertlt(maxerrc, precision, 'error in torch.functionname - contiguous')
   tester:assertlt(maxerrnc, precision, 'error in torch.functionname - non-contiguous')
end

function torch_main_tests.round()
   -- [res] torch.round([res,] x)
   -- contiguous
   local m1 = torch.randn(100,100)
   local res1 = torch.round(m1[{ 4,{} }])
   local res2 = res1:clone():zero()
   for i = 1,res1:size(1) do
      res2[i] = math.floor(m1[4][i]+0.5)
   end
   local err = res1:clone():zero()
   -- find absolute error
   for i = 1, res1:size(1) do
      err[i] = math.abs(res1[i] - res2[i])
   end
   -- find maximum element of error
   local maxerrc = 0
   for i = 1, err:size(1) do
      if err[i] > maxerrc then
         maxerrc = err[i]
      end
   end
   tester:assertlt(maxerrc, precision, 'error in torch.round - contiguous')

   -- non-contiguous
   local m1 = torch.randn(100,100)
   local res1 = torch.round(m1[{ {}, 4 }])
   local res2 = res1:clone():zero()
   for i = 1,res1:size(1) do
      res2[i] = math.floor(m1[i][4]+0.5)
   end
   local err = res1:clone():zero()
   -- find absolute error
   for i = 1, res1:size(1) do
      err[i] = math.abs(res1[i] - res2[i])
   end
   -- find maximum element of error
   local maxerrnc = 0
   for i = 1, err:size(1) do
      if err[i] > maxerrnc then
         maxerrnc = err[i]
      end
   end
   tester:assertlt(maxerrnc, precision, 'error in torch.round - non-contiguous')
end

function torch_main_tests.max()  -- torch.max([resval, resind,] x [,dim])

   -- TH_TENSOR_BASE
   local m1 = torch.Tensor(8,2):fill(3):select(2, 1)
   local resval, resind = torch.max(m1, 1)
   tester:assert(resind[1] == 1)

   -- torch.max( x )
   -- contiguous
   local m1 = torch.randn(100,100)
   local res1 = torch.max(m1)
   local res2 = m1[1][1]
   for i = 1,m1:size(1) do
      for j = 1,m1:size(2) do
         if m1[i][j] > res2 then
            res2 = m1[i][j]
         end
      end
   end
   local err = res1 - res2
   tester:assertlt(err, precision, 'error in torch.max - contiguous')

   -- non-contiguous
   local m1 = torch.randn(10,10,10)
   local m2 = m1[{{}, 4, {}}]
   local res1 = torch.max(m2)
   local res2 = m2[1][1]
   for i = 1,m2:size(1) do
      for j = 1,m2:size(2) do
         if m2[i][j] > res2 then
            res2 = m2[i][j]
         end
      end
   end
   local err = res1 - res2
   tester:assertlt(err, precision, 'error in torch.max - non-contiguous')

   -- torch.max([resval, resind,] x ,dim])
   local function lua_max(t, dim)
      assert(t:nDimension() == 2)
      local max_val = t:narrow(dim, 1, 1):clone()
      local max_ind = t:narrow(dim, 1, 1):clone():long():fill(1)
      local other = 3 - dim
      for i = 1, t:size(other) do
         for j = 1, t:size(dim) do
            local val = t:select(other, i):select(dim, j)
            local max = max_val:select(other, i):select(dim, 1)
            if val > max then
               max_val:select(other, i):fill(val)
               max_ind:select(other, i):fill(j)
            end
         end
      end
      return max_val, max_ind
   end

   local m1 = torch.randn(100,100)
   for dim = 1,2 do
      local res1val, res1ind = torch.max(m1, dim)
      local res2val, res2ind = lua_max(m1, dim)
      tester:asserteq((res1val-res2val):abs():max(), 0, 'error in torch.max')
      tester:asserteq((res1ind-res2ind):abs():max(), 0, 'error in torch.max')
   end

   -- NaNs
   for index in pairs{1, 5, 100} do
      local m1 = torch.randn(100)
      m1[index] = 0/0
      local res1val, res1ind = torch.max(m1, 1)
      tester:assert(res1val[1] ~= res1val[1], 'error in torch.max (value) - NaNs')
      tester:assert(res1ind[1] == index, 'error in torch.max (index) - NaNs')
      local res1val = torch.max(m1)
      tester:assert(res1val ~= res1val, 'error in torch.max - NaNs')
   end

   -- dim == nDim -1
   local a = torch.Tensor({{1,2},{3,4}}):select(2, 1)
   local aval, aind = torch.max(a, 1)
   tester:assert(aval[1] == 3)
   tester:assert(aind[1] == 2)

   local b = torch.Tensor({{{1,2},{3,4}},{{5,6},{7,8}}}):select(3, 1)
   local bval, bind = torch.max(b, 2)
   tester:assert(bval[1][1] == 3)
   tester:assert(bind[1][1] == 2)
   tester:assert(bval[2][1] == 7)
   tester:assert(bind[2][1] == 2)
end

function torch_main_tests.min()  -- torch.min([resval, resind,] x [,dim])
   -- torch.min( x )
   -- contiguous
   local m1 = torch.randn(100,100)
   local res1 = torch.min(m1)
   local res2 = m1[1][1]
   for i = 1,m1:size(1) do
      for j = 1,m1:size(2) do
         if m1[i][j] < res2 then
            res2 = m1[i][j]
         end
      end
   end
   local err = res1 - res2
   tester:assertlt(err, precision, 'error in torch.min - contiguous')
   -- non-contiguous
   local m1 = torch.randn(10,10,10)
   local m2 = m1[{{}, 4, {}}]
   local res1 = torch.min(m2)
   local res2 = m2[1][1]
   for i = 1,m2:size(1) do
      for j = 1,m2:size(2) do
         if m2[i][j] < res2 then
            res2 = m2[i][j]
         end
      end
   end
   local err = res1 - res2
   tester:assertlt(err, precision, 'error in torch.min - non-contiguous')

   -- torch.max([resval, resind,] x ,dim])
   local function lua_min(t, dim)
      assert(t:nDimension() == 2)
      local max_val = t:narrow(dim, 1, 1):clone()
      local max_ind = t:narrow(dim, 1, 1):clone():long():fill(1)
      local other = 3 - dim
      for i = 1, t:size(other) do
         for j = 1, t:size(dim) do
            local val = t:select(other, i):select(dim, j)
            local max = max_val:select(other, i):select(dim, 1)
            if val < max then
               max_val:select(other, i):fill(val)
               max_ind:select(other, i):fill(j)
            end
         end
      end
      return max_val, max_ind
   end

   local m1 = torch.randn(100,100)
   for dim = 1,2 do
      local res1val, res1ind = torch.min(m1, dim)
      local res2val, res2ind = lua_min(m1, dim)
      tester:asserteq((res1val-res2val):abs():max(), 0, 'error in torch.max')
      tester:asserteq((res1ind-res2ind):abs():max(), 0, 'error in torch.max')
   end

   -- NaNs
   for index in pairs{1, 5, 100} do
      local m1 = torch.randn(100)
      m1[index] = 0/0
      local res1val, res1ind = torch.min(m1, 1)
      tester:assert(res1val[1] ~= res1val[1], 'error in torch.min (value) - NaNs')
      tester:assert(res1ind[1] == index, 'error in torch.min (index) - NaNs')
      local res1val = torch.min(m1)
      tester:assert(res1val ~= res1val, 'error in torch.min - NaNs')
   end

   -- TH_TENSOR_BASE
   local m1 = torch.Tensor(4):fill(3)
   local resval, resind = torch.min(m1, 1)
   tester:assert(resind[1] == 1)
end

function torch_main_tests.cmax()
  -- Two tensors.
  local a = torch.rand(msize, msize)
  local b = torch.rand(msize, msize)
  local c = torch.cmax(a, b)
  local expected_c = torch.zeros(msize, msize)
  expected_c:map2(a, b, function(_, a, b) return math.max(a, b) end)
  tester:assertTensorEq(expected_c, c, 0,
                          'error in torch.cmax(tensor, tensor)')

  -- Tensor and scalar.
  local v = torch.uniform()
  c = torch.cmax(a, v)
  expected_c:map(a, function(_, a) return math.max(a, v) end)
  tester:assertTensorEq(expected_c, c, 0,
                          'error in torch.cmax(tensor, scalar).')
end

function torch_main_tests.cmin()
  -- Two tensors.
  local a = torch.rand(msize, msize)
  local b = torch.rand(msize, msize)
  local c = torch.cmin(a, b)
  local expected_c = torch.zeros(msize, msize)
  expected_c:map2(a, b, function(_, a, b) return math.min(a, b) end)
  tester:assertTensorEq(expected_c, c, 0,
                          'error in torch.cmin(tensor, tensor)')

  -- Tensor and scalar.
  local v = torch.uniform()
  c = torch.cmin(a, v)
  expected_c:map(a, function(_, a) return math.min(a, v) end)
  tester:assertTensorEq(expected_c, c, 0,
                          'error in torch.cmin(tensor, scalar).')
end

function torch_main_tests.lerp()
   local function TH_lerp(a, b, weight)
      return a + weight * (b-a);
   end

   local a = torch.rand(msize, msize)
   local b = torch.rand(msize, msize)
   local w = math.random()
   local result = torch.lerp(a, b, w)
   local expected = a:new()
   expected:map2(a, b, function(_, a, b) return TH_lerp(a, b, w) end)
   tester:assertTensorEq(expected, result, precision, 'error in torch.lerp(tensor, tensor, weight)')

   local a = (math.random()*2-1) * 100000
   local b = (math.random()*2-1) * 100000
   local w = math.random()
   local result = torch.lerp(a, b, w)
   local expected = TH_lerp(a, b, w)
   tester:assertalmosteq(expected, result, precision, 'error in torch.lerp(scalar, scalar, weight)')
end

for i, v in ipairs{{10}, {5, 5}} do
   torch_main_tests['allAndAny' .. i] =
      function ()
           local x = torch.ones(unpack(v)):byte()
           tester:assert(x:all(), 'error in all()')
           tester:assert(x:any(), 'error in any()')

           x[3] = 0
           tester:assert(not x:all(), 'error in all()')
           tester:assert(x:any(), 'error in any()')

           x:zero()
           tester:assert(not x:all(), 'error in all()')
           tester:assert(not x:any(), 'error in any()')

           x:fill(2)
           tester:assert(x:all(), 'error in all()')
           tester:assert(x:any(), 'error in any()')
       end
end

function torch_main_tests.mv()
   local m1 = torch.randn(100,100)
   local v1 = torch.randn(100)

   local res1 = torch.mv(m1,v1)

   local res2 = res1:clone():zero()
   for i = 1,m1:size(1) do
      for j = 1,m1:size(2) do
         res2[i] = res2[i] + m1[i][j] * v1[j]
      end
   end

   local err = (res1-res2):abs():max()

   tester:assertlt(err, precision, 'error in torch.mv')
end

function torch_main_tests.fill()
   local types = {
      'torch.ByteTensor',
      'torch.CharTensor',
      'torch.ShortTensor',
      'torch.IntTensor',
      'torch.FloatTensor',
      'torch.DoubleTensor',
      'torch.LongTensor',
   }

   for k,t in ipairs(types) do
      -- [res] torch.fill([res,] tensor, value)
      local m1 = torch.ones(100,100):type(t)
      local res1 = m1:clone()
      res1[{ 3,{} }]:fill(2)

      local res2 = m1:clone()
      for i = 1,m1:size(1) do
	 res2[{ 3,i }] = 2
      end

      local err = (res1-res2):double():abs():max()

      tester:assertlt(err, precision, 'error in torch.fill - contiguous')

      local m1 = torch.ones(100,100):type(t)
      local res1 = m1:clone()
      res1[{ {},3 }]:fill(2)

      local res2 = m1:clone()
      for i = 1,m1:size(1) do
	 res2[{ i,3 }] = 2
      end

      local err = (res1-res2):double():abs():max()

      tester:assertlt(err, precision, 'error in torch.fill - non contiguous')
   end
end

function torch_main_tests.add()
   local types = {
      'torch.ByteTensor',
      'torch.CharTensor',
      'torch.ShortTensor',
      'torch.IntTensor',
      'torch.FloatTensor',
      'torch.DoubleTensor',
      'torch.LongTensor',
   }

   for k,t in ipairs(types) do
       -- [res] torch.add([res,] tensor1, tensor2)
       local m1 = torch.randn(100,100):type(t)
       local v1 = torch.randn(100):type(t)

       local res1 = torch.add(m1[{ 4,{} }],v1)

       local res2 = res1:clone():zero()
       for i = 1,m1:size(2) do
           res2[i] = m1[4][i] + v1[i]
       end

       local err = (res1-res2):double():abs():max()

       tester:assertlt(err, precision, 'error in torch.add - contiguous' .. ' ' .. t)

       local m1 = torch.randn(100,100):type(t)
       local v1 = torch.randn(100):type(t)

       local res1 = torch.add(m1[{ {},4 }],v1)

       local res2 = res1:clone():zero()
       for i = 1,m1:size(1) do
           res2[i] = m1[i][4] + v1[i]
       end

       local err = (res1-res2):double():abs():max()

       tester:assertlt(err, precision, 'error in torch.add - non contiguous' .. ' ' .. t)

       -- [res] torch.add([res,] tensor, value)
       local m1 = torch.randn(10,10):type(t)
       local res1 = m1:clone()
       res1[{ 3,{} }]:add(2)

       local res2 = m1:clone()
       for i = 1,m1:size(1) do
           res2[{ 3,i }] = res2[{ 3,i }] + 2
       end

       local err = (res1-res2):double():abs():max()

       tester:assertlt(err, precision, 'error in torch.add - scalar, contiguous' .. ' ' .. t)

       local m1 = torch.randn(10,10)
       local res1 = m1:clone()
       res1[{ {},3 }]:add(2)

       local res2 = m1:clone()
       for i = 1,m1:size(1) do
           res2[{ i,3 }] = res2[{ i,3 }] + 2
       end

       local err = (res1-res2):abs():max()

       tester:assertlt(err, precision, 'error in torch.add - scalar, non contiguous' .. ' ' .. t)

       -- [res] torch.add([res,] tensor1, value, tensor2)
   end
end

function torch_main_tests.csub()
   local rngState = torch.getRNGState()
   torch.manualSeed(123)

   local a = torch.randn(100,90)
   local b = a:clone():normal()

   local res_add = torch.add(a, -1, b)
   local res_csub = a:clone()
   res_csub:csub(b)

   tester:assertlt((res_add - res_csub):abs():max(), 0.00001)

   local _ = torch.setRNGState(rngState)
end

function torch_main_tests.csub_scalar()
   local rngState = torch.getRNGState()
   torch.manualSeed(123)

   local a = torch.randn(100,100)

   local scalar = 123.5
   local res_add = torch.add(a, -scalar)
   local res_csub = a:clone()
   res_csub:csub(scalar)

   tester:assertlt((res_add - res_csub):abs():max(), 0.00001)

   local _ = torch.setRNGState(rngState)
end

function torch_main_tests.neg()
   local rngState = torch.getRNGState()
   torch.manualSeed(123)

   local a = torch.randn(100,90)
   local zeros = torch.Tensor():resizeAs(a):zero()

   local res_add = torch.add(zeros, -1, a)
   local res_neg = a:clone()
   res_neg:neg()

   tester:assertlt((res_add - res_neg):abs():max(), 0.00001)

   local _ = torch.setRNGState(rngState)
end

function torch_main_tests.cinv()
   local rngState = torch.getRNGState()
   torch.manualSeed(123)

   local a = torch.randn(100,89)
   local zeros = torch.Tensor():resizeAs(a):zero()

   local res_pow = torch.pow(a, -1)
   local res_inv = a:clone()
   res_inv:cinv()

   tester:assertlt((res_pow - res_inv):abs():max(), 0.00001)

   local _ = torch.setRNGState(rngState)
end

function torch_main_tests.mul()
   local types = {
      'torch.ByteTensor',
      'torch.CharTensor',
      'torch.ShortTensor',
      'torch.IntTensor',
      'torch.FloatTensor',
      'torch.DoubleTensor',
      'torch.LongTensor',
   }

   for k,t in ipairs(types) do
       local m1 = torch.randn(10,10):type(t)
       local res1 = m1:clone()

       res1[{ {},3 }]:mul(2)

       local res2 = m1:clone()
       for i = 1,m1:size(1) do
           res2[{ i,3 }] = res2[{ i,3 }] * 2
       end

       local err = (res1-res2):double():abs():max()

       tester:assertlt(err, precision, 'error in torch.mul - scalar, non contiguous' .. ' ' .. t)
   end
end

function torch_main_tests.div()
    local types = {
        'torch.ByteTensor',
        'torch.CharTensor',
        'torch.ShortTensor',
        'torch.IntTensor',
        'torch.FloatTensor',
        'torch.DoubleTensor',
        'torch.LongTensor',
    }

    for k,t in ipairs(types) do

        local m1 = torch.Tensor(10,10):uniform(0,10):type(t)
        local res1 = m1:clone()

        res1[{ {},3 }]:div(2)

        local res2 = m1:clone()
        for i = 1,m1:size(1) do
            local ok = pcall(function() res2[{ i,3 }] = res2[{ i,3 }] / 2 end)
            if not ok then
               res2[{ i,3 }] = torch.floor(res2[{ i,3 }] / 2)
            end
        end

        local err = (res1-res2):double():abs():max()

        tester:assertlt(err, precision, 'error in torch.div - scalar, non contiguous' .. ' ' .. t)
    end
end

function torch_main_tests.lshift()
   local m1 = torch.LongTensor(10,10):random(0,100)
   local res1 = m1:clone()

   local q = 2
   local f = math.pow(2, q)
   res1[{ {},3 }]:lshift(q)

   local res2 = m1:clone()
   for i = 1,m1:size(1) do
      res2[{ i,3 }] = res2[{ i,3 }] * f
   end

   local err = (res1-res2):abs():max()

   tester:assertlt(err, precision, 'error in torch.lshift - scalar, non contiguous')

   local m1 = torch.LongTensor(10,10):random(0,100)
   local res1 = m1:clone()

   local q = 2
   res1:lshift(q)

   local res2 = m1:clone()
   for i = 1,m1:size(1) do
      for j = 1,m1:size(1) do
         res2[{ i,j }] = res2[{ i,j }] * f
      end
   end

   local err = (res1-res2):abs():max()

   tester:assertlt(err, precision, 'error in torch.lshift - scalar, contiguous')
end

function torch_main_tests.rshift()
   local m1 = torch.LongTensor(10,10):random(0,100)
   local res1 = m1:clone()

   local q = 2
   local f = math.pow(2, q)
   res1[{ {},3 }]:rshift(q)

   local res2 = m1:clone()
   for i = 1,m1:size(1) do
      res2[{ i,3 }] = math.floor(res2[{ i,3 }] / f)
   end

   local err = (res1-res2):abs():max()

   tester:assertlt(err, precision, 'error in torch.rshift - scalar, non contiguous')

   local m1 = torch.LongTensor(10,10):random(0,100)
   local res1 = m1:clone()

   local q = 2
   res1:rshift(q)

   local res2 = m1:clone()
   for i = 1,m1:size(1) do
      for j = 1,m1:size(1) do
         res2[{ i,j }] = math.floor(res2[{ i,j }] / f)
      end
   end

   local err = (res1-res2):abs():max()

   tester:assertlt(err, precision, 'error in torch.rshift - scalar, contiguous')
end

function torch_main_tests.fmod()
   local m1 = torch.Tensor(10,10):uniform(-10, 10)
   local res1 = m1:clone()

   local q = 2.1
   res1[{ {},3 }]:fmod(q)

   local res2 = m1:clone()
   for i = 1,m1:size(1) do
      res2[{ i,3 }] = math.fmod(res2[{ i,3 }], q)
   end

   local err = (res1-res2):abs():max()

   tester:assertlt(err, precision, 'error in torch.fmod - scalar, non contiguous')
end

function torch_main_tests.remainder()
   local m1 = torch.Tensor(10, 10):uniform(-10, 10)
   local res1 = m1:clone()

   local q = 2.1
   res1[{ {},3 }]:remainder(q)

   local res2 = m1:clone()
   for i = 1,m1:size(1) do
      res2[{ i,3 }] = res2[{ i,3 }] % q
   end

   local err = (res1-res2):abs():max()

   tester:assertlt(err, precision, 'error in torch.remainder - scalar, non contiguous')
end

function torch_main_tests.bitand()
   local m1 = torch.LongTensor(10,10):random(0,100)
   local res1 = m1:clone()

   local val = 32 -- This should be a power of 2
   res1[{ {},3 }]:bitand(val - 1)

   local res2 = m1:clone()
   for i = 1,m1:size(1) do
      res2[{ i,3 }] = res2[{ i,3 }] % val
   end

   local err = (res1-res2):abs():max()

   tester:assertlt(err, precision, 'error in torch.bitand - scalar, non contiguous')

   local m1 = torch.LongTensor(10,10):random(0,100)
   local res1 = m1:clone()

   res1:bitand(val - 1)

   local res2 = m1:clone()
   for i = 1,m1:size(1) do
      for j = 1,m1:size(1) do
         res2[{ i,j }] = res2[{ i,j }] % val
      end
   end

   local err = (res1-res2):abs():max()

   tester:assertlt(err, precision, 'error in torch.bitand - scalar, contiguous')
end

function torch_main_tests.bitor()
   local m1 = torch.LongTensor(10,10):random(0,10000)
   local res1 = m1:clone()

   local val = 32 -- This should be a power of 2
   res1[{ {},3 }]:bitor(val-1)

   local res2 = m1:clone()
   for i = 1,m1:size(1) do
      res2[{ i,3 }] = math.floor(res2[{ i,3 }] / val) * val + (val - 1)
   end

   local err = (res1-res2):abs():max()

   tester:assertlt(err, precision, 'error in torch.bitor - scalar, non contiguous')

   local m1 = torch.LongTensor(10,10):random(0,10000)
   local res1 = m1:clone()

   res1:bitor(val - 1)

   local res2 = m1:clone()
   for i = 1,m1:size(1) do
      for j = 1,m1:size(1) do
         res2[{ i,j }] = math.floor(res2[{ i,j }] / val) * val + (val - 1)
      end
   end

   local err = (res1-res2):abs():max()

   tester:assertlt(err, precision, 'error in torch.bitor - scalar, contiguous')
end

function torch_main_tests.cbitxor()
   local t1 = torch.LongTensor(10,10):random(0,10000)
   local t2 = torch.LongTensor(10,10):random(10001,20000)

   -- Perform xor swap and check results
   local t3 = torch.cbitxor(t1, t2)
   local r1 = torch.cbitxor(t3, t2)
   local r2 = torch.cbitxor(t3, t1)

   local err1 = (r1 - t1):abs():max()
   local err2 = (r2 - t2):abs():max()
   tester:assertlt(err1 + err2, precision, 'error in torch.cbitxor contiguous')
end

function torch_main_tests.mm()
   -- helper function
   local function matrixmultiply(mat1,mat2)
      local n = mat1:size(1)
      local m = mat1:size(2)
      local p = mat2:size(2)
      local res = torch.zeros(n,p)
      for i = 1, n do
         for j = 1, p do
            local sum = 0
            for k = 1, m do
               sum = sum + mat1[i][k]*mat2[k][j]
            end
            res[i][j] = sum
         end
      end
      return res
   end

   -- contiguous case
   local n, m, p = 10, 10, 5
   local mat1 = torch.randn(n,m)
   local mat2 = torch.randn(m,p)
   local res = torch.mm(mat1,mat2)

   local res2 = matrixmultiply(mat1,mat2)
   tester:assertTensorEq(res,res2,precision,'error in torch.mm')

   -- non contiguous case 1
   local n, m, p = 10, 10, 5
   local mat1 = torch.randn(n,m)
   local mat2 = torch.randn(p,m):t()
   local res = torch.mm(mat1,mat2)

   local res2 = matrixmultiply(mat1,mat2)
   tester:assertTensorEq(res,res2,precision,'error in torch.mm, non contiguous')

   -- non contiguous case 2
   local n, m, p = 10, 10, 5
   local mat1 = torch.randn(m,n):t()
   local mat2 = torch.randn(m,p)
   local res = torch.mm(mat1,mat2)

   local res2 = matrixmultiply(mat1,mat2)
   tester:assertTensorEq(res,res2,precision,'error in torch.mm, non contiguous')

   -- non contiguous case 3
   local n, m, p = 10, 10, 5
   local mat1 = torch.randn(m,n):t()
   local mat2 = torch.randn(p,m):t()
   local res = torch.mm(mat1,mat2)

   local res2 = matrixmultiply(mat1,mat2)
   tester:assertTensorEq(res,res2,precision,'error in torch.mm, non contiguous')

   -- test with zero stride
   local n, m, p = 10, 10, 5
   local mat1 = torch.randn(n,m)
   local mat2 = torch.randn(m,1):expand(m,p)
   local res = torch.mm(mat1,mat2)

   local res2 = matrixmultiply(mat1,mat2)
   tester:assertTensorEq(res,res2,precision,'error in torch.mm, non contiguous, zero stride')

end

function torch_main_tests.bmm()
   local num_batches = 10
   local M, N, O = 23, 8, 12
   local b1 = torch.randn(num_batches, M, N)
   local b2 = torch.randn(num_batches, N, O)
   local res = torch.bmm(b1, b2)

   for i = 1, num_batches do
     local r = torch.mm(b1[i], b2[i])
     tester:assertTensorEq(r, res[i], precision, 'result matrix ' .. i .. ' wrong')
   end
end

function torch_main_tests.addbmm()
   local num_batches = 10
   local M, N, O = 12, 8, 5
   local b1 = torch.randn(num_batches, M, N)
   local b2 = torch.randn(num_batches, N, O)
   local res = torch.bmm(b1, b2)
   local res2 = torch.Tensor():resizeAs(res[1]):zero()

   res2:addbmm(b1,b2)
   tester:assertTensorEq(res2, res:sum(1)[1], precision, 'addbmm result wrong')

   res2:addbmm(1,b1,b2)
   tester:assertTensorEq(res2, res:sum(1)[1]*2, precision, 'addbmm result wrong')

   res2:addbmm(1,res2,.5,b1,b2)
   tester:assertTensorEq(res2, res:sum(1)[1]*2.5, precision, 'addbmm result wrong')

   local res3 = torch.addbmm(1,res2,0,b1,b2)
   tester:assertTensorEq(res3, res2, precision, 'addbmm result wrong')

   local res4 = torch.addbmm(1,res2,.5,b1,b2)
   tester:assertTensorEq(res4, res:sum(1)[1]*3, precision, 'addbmm result wrong')

   local res5 = torch.addbmm(0,res2,1,b1,b2)
   tester:assertTensorEq(res5, res:sum(1)[1], precision, 'addbmm result wrong')

   local res6 = torch.addbmm(.1,res2,.5,b1,b2)
   tester:assertTensorEq(res6, res2*.1 + res:sum(1)*.5, precision, 'addbmm result wrong')
end

function torch_main_tests.baddbmm()
   local num_batches = 10
   local M, N, O = 12, 8, 5
   local b1 = torch.randn(num_batches, M, N)
   local b2 = torch.randn(num_batches, N, O)
   local res = torch.bmm(b1, b2)
   local res2 = torch.Tensor():resizeAs(res):zero()

   res2:baddbmm(b1,b2)
   tester:assertTensorEq(res2, res, precision, 'baddbmm result wrong')

   res2:baddbmm(1,b1,b2)
   tester:assertTensorEq(res2, res*2, precision, 'baddbmm result wrong')

   res2:baddbmm(1,res2,.5,b1,b2)
   tester:assertTensorEq(res2, res*2.5, precision, 'baddbmm result wrong')

   local res3 = torch.baddbmm(1,res2,0,b1,b2)
   tester:assertTensorEq(res3, res2, precision, 'baddbmm result wrong')

   local res4 = torch.baddbmm(1,res2,.5,b1,b2)
   tester:assertTensorEq(res4, res*3, precision, 'baddbmm result wrong')

   local res5 = torch.baddbmm(0,res2,1,b1,b2)
   tester:assertTensorEq(res5, res, precision, 'baddbmm result wrong')

   local res6 = torch.baddbmm(.1,res2,.5,b1,b2)
   tester:assertTensorEq(res6, res2*.1 + res*.5, precision, 'baddbmm result wrong')
end

function torch_main_tests.clamp()
   local m1 = torch.rand(100):mul(5):add(-2.5)  -- uniform in [-2.5, 2.5]
   -- just in case we're extremely lucky:
   local min_val = -1
   local max_val = 1
   m1[1] = min_val
   m1[2] = max_val
   local res1 = m1:clone()

   res1:clamp(min_val, max_val)

   local res2 = m1:clone()
   for i = 1,m1:size(1) do
      if res2[i] > max_val then
         res2[i] = max_val
      elseif res2[i] < min_val then
         res2[i] = min_val
      end
   end

   local err = (res1-res2):abs():max()

   tester:assertlt(err, precision, 'error in torch.clamp - scalar, non contiguous')
end

function torch_main_tests.pow() -- [res] torch.pow([res,] x)
   -- base - tensor, exponent - number
   -- contiguous
   local m1 = torch.randn(100,100)
   local res1 = torch.pow(m1[{ 4,{} }], 3)
   local res2 = res1:clone():zero()
   for i = 1,res1:size(1) do
      res2[i] = math.pow(m1[4][i], 3)
   end
   local err = res1:clone():zero()
   -- find absolute error
   for i = 1, res1:size(1) do
      err[i] = math.abs(res1[i] - res2[i])
   end
   -- find maximum element of error
   local maxerr = 0
   for i = 1, err:size(1) do
      if err[i] > maxerr then
         maxerr = err[i]
      end
   end
   tester:assertlt(maxerr, precision, 'error in torch.pow - contiguous')

   -- non-contiguous
   local m1 = torch.randn(100,100)
   local res1 = torch.pow(m1[{ {}, 4 }], 3)
   local res2 = res1:clone():zero()
   for i = 1,res1:size(1) do
      res2[i] = math.pow(m1[i][4], 3)
   end
   local err = res1:clone():zero()
   -- find absolute error
   for i = 1, res1:size(1) do
      err[i] = math.abs(res1[i] - res2[i])
   end
   -- find maximum element of error
   local maxerr = 0
   for i = 1, err:size(1) do
      if err[i] > maxerr then
         maxerr = err[i]
      end
   end
   tester:assertlt(maxerr, precision, 'error in torch.pow - non-contiguous')

   -- base - number, exponent - tensor
   -- contiguous
   local m1 = torch.randn(100,100)
   local res1 = torch.pow(3, m1[{ 4,{} }])
   local res2 = res1:clone():zero()
   for i = 1,res1:size(1) do
      res2[i] = math.pow(3, m1[4][i])
   end
   local err = res1:clone():zero()
   -- find absolute error
   for i = 1, res1:size(1) do
      err[i] = math.abs(res1[i] - res2[i])
   end
   -- find maximum element of error
   local maxerr = 0
   for i = 1, err:size(1) do
      if err[i] > maxerr then
         maxerr = err[i]
      end
   end
   tester:assertlt(maxerr, precision, 'error in torch.pow - contiguous')

   -- non-contiguous
   local m1 = torch.randn(100,100)
   local res1 = torch.pow(3, m1[{ {}, 4 }])
   local res2 = res1:clone():zero()
   for i = 1,res1:size(1) do
      res2[i] = math.pow(3, m1[i][4])
   end
   local err = res1:clone():zero()
   -- find absolute error
   for i = 1, res1:size(1) do
      err[i] = math.abs(res1[i] - res2[i])
   end
   -- find maximum element of error
   local maxerr = 0
   for i = 1, err:size(1) do
      if err[i] > maxerr then
         maxerr = err[i]
      end
   end
   tester:assertlt(maxerr, precision, 'error in torch.pow - non-contiguous')
end

function torch_main_tests.cdiv()
    local types = {
        'torch.ByteTensor',
        'torch.CharTensor',
        'torch.ShortTensor',
        'torch.IntTensor',
        'torch.FloatTensor',
        'torch.DoubleTensor',
        'torch.LongTensor',
    }

    for k,t in ipairs(types) do

        -- [res] torch.cdiv([res,] tensor1, tensor2)
        -- contiguous
        local m1 = torch.Tensor(10, 10, 10):uniform(0,10):type(t)
        local m2 = torch.Tensor(10, 10 * 10):uniform(0,10):type(t)
        m2[m2:eq(0)] = 2
        local sm1 = m1[{4, {}, {}}]
        local sm2 = m2[{4, {}}]
        local res1 = torch.cdiv(sm1, sm2)
        local res2 = res1:clone():zero()
        for i = 1,sm1:size(1) do
            for j = 1, sm1:size(2) do
                local idx1d = (((i-1)*sm1:size(1)))+j
                local ok = pcall(function() res2[i][j] = sm1[i][j] / sm2[idx1d] end)
                if not ok then
                   res2[i][j] = torch.floor(sm1[i][j] / sm2[idx1d])
                end
            end
        end
        local err = res1:clone():zero()
        -- find absolute error
        for i = 1, res1:size(1) do
            for j = 1, res1:size(2) do
                err[i][j] = math.abs(res1[i][j] - res2[i][j])
            end
        end
        -- find maximum element of error
        local maxerr = 0
        for i = 1, err:size(1) do
            for j = 1, err:size(2) do
                if err[i][j] > maxerr then
                    maxerr = err[i][j]
                end
            end
        end
        tester:assertlt(maxerr, precision, 'error in torch.cdiv - contiguous' .. ' ' .. t)

        -- non-contiguous
        local m1 = torch.Tensor(10, 10, 10):uniform(0,10):type(t)
        local m2 = torch.Tensor(10 * 10, 10 * 10):uniform(0,10):type(t)
        m2[m2:eq(0)] = 2
        local sm1 = m1[{{}, 4, {}}]
        local sm2 = m2[{{}, 4}]
        local res1 = torch.cdiv(sm1, sm2)
        local res2 = res1:clone():zero()
        for i = 1,sm1:size(1) do
            for j = 1, sm1:size(2) do
                local idx1d = (((i-1)*sm1:size(1)))+j
                local ok = pcall(function() res2[i][j] = sm1[i][j] / sm2[idx1d] end)
                if not ok then
                   res2[i][j] = torch.floor(sm1[i][j] / sm2[idx1d])
                end
            end
        end
        local err = res1:clone():zero()
        -- find absolute error
        for i = 1, res1:size(1) do
            for j = 1, res1:size(2) do
                err[i][j] = math.abs(res1[i][j] - res2[i][j])
            end
        end
        -- find maximum element of error
        local maxerr = 0
        for i = 1, err:size(1) do
            for j = 1, err:size(2) do
                if err[i][j] > maxerr then
                    maxerr = err[i][j]
                end
            end
        end
        tester:assertlt(maxerr, precision, 'error in torch.cdiv - non-contiguous' .. ' ' .. t)
   end
end

function torch_main_tests.cfmod()
   -- contiguous
   local m1 = torch.Tensor(10, 10, 10):uniform(-10, 10)
   local m2 = torch.Tensor(10, 10 * 10):uniform(-3, 3)
   local sm1 = m1[{4, {}, {}}]
   local sm2 = m2[{4, {}}]
   local res1 = torch.cfmod(sm1, sm2)
   local res2 = res1:clone():zero()
   for i = 1,sm1:size(1) do
      for j = 1, sm1:size(2) do
         local idx1d = (((i-1)*sm1:size(1)))+j
         res2[i][j] = math.fmod(sm1[i][j], sm2[idx1d])
      end
   end
   local err = res1:clone():zero()
   -- find absolute error
   for i = 1, res1:size(1) do
      for j = 1, res1:size(2) do
         err[i][j] = math.abs(res1[i][j] - res2[i][j])
      end
   end
   -- find maximum element of error
   local maxerr = 0
   for i = 1, err:size(1) do
      for j = 1, err:size(2) do
         if err[i][j] > maxerr then
            maxerr = err[i][j]
         end
      end
   end
   tester:assertlt(maxerr, precision, 'error in torch.cfmod - contiguous')

   -- non-contiguous
   local m1 = torch.Tensor(10, 10, 10):uniform(-10, 10)
   local m2 = torch.Tensor(10 * 10, 10 * 10):uniform(-3, 3)
   local sm1 = m1[{{}, 4, {}}]
   local sm2 = m2[{{}, 4}]
   local res1 = torch.cfmod(sm1, sm2)
   local res2 = res1:clone():zero()
   for i = 1,sm1:size(1) do
      for j = 1, sm1:size(2) do
         local idx1d = (((i-1)*sm1:size(1)))+j
         res2[i][j] = math.fmod(sm1[i][j], sm2[idx1d])
      end
   end
   local err = res1:clone():zero()
   -- find absolute error
   for i = 1, res1:size(1) do
      for j = 1, res1:size(2) do
         err[i][j] = math.abs(res1[i][j] - res2[i][j])
      end
   end
   -- find maximum element of error
   local maxerr = 0
   for i = 1, err:size(1) do
      for j = 1, err:size(2) do
         if err[i][j] > maxerr then
            maxerr = err[i][j]
         end
      end
   end
   tester:assertlt(maxerr, precision, 'error in torch.cfmod - non-contiguous')
end

function torch_main_tests.cremainder()
   -- contiguous
   local m1 = torch.Tensor(10, 10, 10):uniform(-10, 10)
   local m2 = torch.Tensor(10, 10 * 10):uniform(-3, 3)
   local sm1 = m1[{4, {}, {}}]
   local sm2 = m2[{4, {}}]
   local res1 = torch.cremainder(sm1, sm2)
   local res2 = res1:clone():zero()
   for i = 1,sm1:size(1) do
      for j = 1, sm1:size(2) do
         local idx1d = (((i-1)*sm1:size(1)))+j
         res2[i][j] = sm1[i][j] % sm2[idx1d]
      end
   end
   local err = res1:clone():zero()
   -- find absolute error
   for i = 1, res1:size(1) do
      for j = 1, res1:size(2) do
         err[i][j] = math.abs(res1[i][j] - res2[i][j])
      end
   end
   -- find maximum element of error
   local maxerr = 0
   for i = 1, err:size(1) do
      for j = 1, err:size(2) do
         if err[i][j] > maxerr then
            maxerr = err[i][j]
         end
      end
   end
   tester:assertlt(maxerr, precision, 'error in torch.cremainder - contiguous')

   -- non-contiguous
   local m1 = torch.Tensor(10, 10, 10):uniform(-10, 10)
   local m2 = torch.Tensor(10 * 10, 10 * 10):uniform(-3, 3)
   local sm1 = m1[{{}, 4, {}}]
   local sm2 = m2[{{}, 4}]
   local res1 = torch.cremainder(sm1, sm2)
   local res2 = res1:clone():zero()
   for i = 1,sm1:size(1) do
      for j = 1, sm1:size(2) do
         local idx1d = (((i-1)*sm1:size(1)))+j
         res2[i][j] = sm1[i][j] % sm2[idx1d]
      end
   end
   local err = res1:clone():zero()
   -- find absolute error
   for i = 1, res1:size(1) do
      for j = 1, res1:size(2) do
         err[i][j] = math.abs(res1[i][j] - res2[i][j])
      end
   end
   -- find maximum element of error
   local maxerr = 0
   for i = 1, err:size(1) do
      for j = 1, err:size(2) do
         if err[i][j] > maxerr then
            maxerr = err[i][j]
         end
      end
   end
   tester:assertlt(maxerr, precision, 'error in torch.cremainder - non-contiguous')
end

function torch_main_tests.cmul()
    local types = {
        'torch.ByteTensor',
        'torch.CharTensor',
        'torch.ShortTensor',
        'torch.IntTensor',
        'torch.FloatTensor',
        'torch.DoubleTensor',
        'torch.LongTensor',
    }

    for k,t in ipairs(types) do

        -- [res] torch.cmul([res,] tensor1, tensor2)
        -- contiguous
        local m1 = torch.randn(10, 10, 10):type(t)
        local m2 = torch.randn(10, 10 * 10):type(t)
        local sm1 = m1[{4, {}, {}}]
        local sm2 = m2[{4, {}}]
        local res1 = torch.cmul(sm1, sm2)
        local res2 = res1:clone():zero()
        for i = 1,sm1:size(1) do
            for j = 1, sm1:size(2) do
                local idx1d = (((i-1)*sm1:size(1)))+j
                res2[i][j] = sm1[i][j] * sm2[idx1d]
            end
        end
        local err = res1:clone():zero()
        -- find absolute error
        for i = 1, res1:size(1) do
            for j = 1, res1:size(2) do
                err[i][j] = math.abs(res1[i][j] - res2[i][j])
            end
        end
        -- find maximum element of error
        local maxerr = 0
        for i = 1, err:size(1) do
            for j = 1, err:size(2) do
                if err[i][j] > maxerr then
                    maxerr = err[i][j]
                end
            end
        end
        tester:assertlt(maxerr, precision, 'error in torch.cmul - contiguous' .. ' ' .. t)

        -- non-contiguous
        local m1 = torch.randn(10, 10, 10):type(t)
        local m2 = torch.randn(10 * 10, 10 * 10):type(t)
        local sm1 = m1[{{}, 4, {}}]
        local sm2 = m2[{{}, 4}]
        local res1 = torch.cmul(sm1, sm2)
        local res2 = res1:clone():zero()
        for i = 1,sm1:size(1) do
            for j = 1, sm1:size(2) do
                local idx1d = (((i-1)*sm1:size(1)))+j
                res2[i][j] = sm1[i][j] * sm2[idx1d]
            end
        end
        local err = res1:clone():zero()
        -- find absolute error
        for i = 1, res1:size(1) do
            for j = 1, res1:size(2) do
                err[i][j] = math.abs(res1[i][j] - res2[i][j])
            end
        end
        -- find maximum element of error
        local maxerr = 0
        for i = 1, err:size(1) do
            for j = 1, err:size(2) do
                if err[i][j] > maxerr then
                    maxerr = err[i][j]
                end
            end
        end
        tester:assertlt(maxerr, precision, 'error in torch.cmul - non-contiguous' .. ' ' .. t)
    end
end

function torch_main_tests.cpow()  -- [res] torch.cpow([res,] tensor1, tensor2)
   -- contiguous
   local m1 = torch.rand(10, 10, 10)
   local m2 = torch.rand(10, 10 * 10)
   local sm1 = m1[{4, {}, {}}]
   local sm2 = m2[{4, {}}]
   local res1 = torch.cpow(sm1, sm2)
   local res2 = res1:clone():zero()
   for i = 1,sm1:size(1) do
      for j = 1, sm1:size(2) do
         local idx1d = (((i-1)*sm1:size(1)))+j
         res2[i][j] = math.pow(sm1[i][j], sm2[idx1d])
      end
   end
   local err = res1:clone():zero()
   -- find absolute error
   for i = 1, res1:size(1) do
      for j = 1, res1:size(2) do
         err[i][j] = math.abs(res1[i][j] - res2[i][j])
      end
   end
   -- find maximum element of error
   local maxerr = 0
   for i = 1, err:size(1) do
      for j = 1, err:size(2) do
         if err[i][j] > maxerr then
            maxerr = err[i][j]
         end
      end
   end
   tester:assertlt(maxerr, precision, 'error in torch.cpow - contiguous')

   -- non-contiguous
   local m1 = torch.rand(10, 10, 10)
   local m2 = torch.rand(10 * 10, 10 * 10)
   local sm1 = m1[{{}, 4, {}}]
   local sm2 = m2[{{}, 4}]
   local res1 = torch.cpow(sm1, sm2)
   local res2 = res1:clone():zero()
   for i = 1,sm1:size(1) do
      for j = 1, sm1:size(2) do
         local idx1d = (((i-1)*sm1:size(1)))+j
         res2[i][j] = math.pow(sm1[i][j],sm2[idx1d])
      end
   end
   local err = res1:clone():zero()
   -- find absolute error
   for i = 1, res1:size(1) do
      for j = 1, res1:size(2) do
         err[i][j] = math.abs(res1[i][j] - res2[i][j])
      end
   end
   -- find maximum element of error
   local maxerr = 0
   for i = 1, err:size(1) do
      for j = 1, err:size(2) do
         if err[i][j] > maxerr then
            maxerr = err[i][j]
         end
      end
   end
   tester:assertlt(maxerr, precision, 'error in torch.cpow - non-contiguous')
end

function torch_main_tests.sum()
   local x = torch.rand(msize,msize)
   local mx = torch.sum(x,2)
   local mxx = torch.Tensor()
   torch.sum(mxx,x,2)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.sum value')

   local y = torch.rand(5, 5, 5)
   for i=1,3 do
      local a = y:sum(i)
      local b = y:narrow(i, 1, 1):clone():zero()
      for j = 1, 5 do
         b:add(y:narrow(i, j, 1))
      end
      tester:asserteq(maxdiff(a, b), 0, 'torch.sum value')
   end
end
function torch_main_tests.prod()
   local x = torch.rand(msize,msize)
   local mx = torch.prod(x,2)
   local mxx = torch.Tensor()
   torch.prod(mxx,x,2)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.prod value')

   local y = torch.rand(5, 5, 5)
   for i=1,3 do
      local a = y:prod(i)
      local b = y:narrow(i, 1, 1):clone():fill(1)
      for j = 1, 5 do
         b:cmul(y:narrow(i, j, 1))
      end
      tester:asserteq(maxdiff(a, b), 0, 'torch.sum value')
   end
end
function torch_main_tests.cumsum()
   local x = torch.rand(msize,msize)
   local mx = torch.cumsum(x,2)
   local mxx = torch.Tensor()
   torch.cumsum(mxx,x,2)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.cumsum value')
end
function torch_main_tests.cumprod()
   local x = torch.rand(msize,msize)
   local mx = torch.cumprod(x,2)
   local mxx = torch.Tensor()
   torch.cumprod(mxx,x,2)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.cumprod value')
end
function torch_main_tests.cross()
   local x = torch.rand(msize,3,msize)
   local y = torch.rand(msize,3,msize)
   local mx = torch.cross(x,y)
   local mxx = torch.Tensor()
   torch.cross(mxx,x,y)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.cross value')
end
function torch_main_tests.zeros()
   local mx = torch.zeros(msize,msize)
   local mxx = torch.Tensor()
   torch.zeros(mxx,msize,msize)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.zeros value')
end
function torch_main_tests.histc()
   local x = torch.Tensor{ 2, 4, 2, 2, 5, 4 }
   local y = torch.histc(x, 5, 1, 5) -- nbins, min, max
   local z = torch.Tensor{ 0, 3, 0, 2, 1 }
   tester:assertTensorEq(y,z,precision,'error in torch.histc')
end
function torch_main_tests.bhistc()
   local x = torch.Tensor(3, 6)
   x[1] = torch.Tensor{ 2, 4, 2, 2, 5, 4 }
   x[2] = torch.Tensor{ 3, 5, 1, 5, 3, 5 }
   x[3] = torch.Tensor{ 3, 4, 2, 5, 5, 1 }
   local y = torch.bhistc(x, 5, 1, 5) -- nbins, min, max
   local z = torch.Tensor(3, 5)
   z[1] = torch.Tensor{ 0, 3, 0, 2, 1 }
   z[2] = torch.Tensor{ 1, 0, 2, 0, 3 }
   z[3] = torch.Tensor{ 1, 1, 1, 1, 2 }
   tester:assertTensorEq(y,z,precision,'error in torch.bhistc in last dimension')
end
function torch_main_tests.ones()
   local mx = torch.ones(msize,msize)
   local mxx = torch.Tensor()
   torch.ones(mxx,msize,msize)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.ones value')
end
function torch_main_tests.diag()
   local x = torch.rand(msize,msize)
   local mx = torch.diag(x)
   local mxx = torch.Tensor()
   torch.diag(mxx,x)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.diag value')
end
function torch_main_tests.eye()
   local mx = torch.eye(msize,msize)
   local mxx = torch.Tensor()
   torch.eye(mxx,msize,msize)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.eye value')
end
function torch_main_tests.renorm()
   local m1 = torch.randn(10,5)
   local res1 = torch.Tensor()
   local m2

   local function renorm(matrix, value, dim, max_norm)
      local m1 = matrix:transpose(dim, 1):contiguous()
      -- collapse non-dim dimensions:
      m2 = m1:reshape(m1:size(1), m1:nElement()/m1:size(1))
      local norms = m2:norm(value,2)
      -- clip
      local new_norms = norms:clone()
      new_norms[torch.gt(norms, max_norm)] = max_norm
      new_norms:cdiv(norms:add(1e-7))
      -- renormalize
      m1:cmul(new_norms:expandAs(m1))
      return m1:transpose(dim, 1)
   end

   -- note that the axis fed to torch.renorm is different (2~=1)
   local maxnorm = m1:norm(2,1):mean()
   m2 = renorm(m1,2,2,maxnorm)

   m1:renorm(2,2,maxnorm)
   tester:assertTensorEq(m1, m2, 0.00001, 'error in renorm')
   tester:assertTensorEq(m1:norm(2,1), m2:norm(2,1), 0.00001, 'error in renorm')

   m1 = torch.randn(3,4,5)
   m2 = m1:transpose(2,3):contiguous():reshape(15,4)

   maxnorm = m2:norm(2,1):mean()
   m2 = renorm(m2,2,2,maxnorm)

   m1:renorm(2,2,maxnorm)
   local m3 = m1:transpose(2,3):contiguous():reshape(15,4)
   tester:assertTensorEq(m3, m2, 0.00001, 'error in renorm')
   tester:assertTensorEq(m3:norm(2,1), m2:norm(2,1), 0.00001, 'error in renorm')
end
function torch_main_tests.multinomialwithreplacement()
   local n_row = 3
   for n_col=4,5 do
      local t=os.time()
      torch.manualSeed(t)
      local prob_dist = torch.rand(n_row,n_col)
      prob_dist:select(2,n_col):fill(0) --index n_col shouldn't be sampled
      local n_sample = n_col
      local sample_indices = torch.multinomial(prob_dist, n_sample, true)
      tester:assert(prob_dist:dim() == 2, "wrong number of prob_dist dimensions")
      tester:assert(sample_indices:size(2) == n_sample, "wrong number of samples")
      for i=1,n_row do
         for j=1,n_sample do
            tester:assert(sample_indices[{i,j}] ~= n_col, "sampled an index with zero probability")
         end
      end
   end
end
function torch_main_tests.multinomialwithoutreplacement()
   local n_row = 3
   for n_col=4,5 do
      local t=os.time()
      torch.manualSeed(t)
      local prob_dist = torch.rand(n_row,n_col)
      prob_dist:select(2,n_col):fill(0) --index n_col shouldn't be sampled
      local n_sample = 3
      local sample_indices = torch.multinomial(prob_dist, n_sample, false)
      tester:assert(prob_dist:dim() == 2, "wrong number of prob_dist dimensions")
      tester:assert(sample_indices:size(2) == n_sample, "wrong number of samples")
      for i=1,n_row do
         local row_samples = {}
         for j=1,n_sample do
            local sample_idx = sample_indices[{i,j}]
            tester:assert(
                sample_idx ~= n_col, "sampled an index with zero probability"
            )
            tester:assert(
                not row_samples[sample_idx], "sampled an index twice"
            )
            row_samples[sample_idx] = true
         end
      end
   end
end
function torch_main_tests.torch_main_tests_aliasMultinomial()
   for i =1,5 do
      local n_class = 5
      local t=os.time()
      torch.manualSeed(t)
      local probs = torch.Tensor(n_class):uniform(0,1)
      probs:div(probs:sum())
      local output = torch.LongTensor(1000, 10000)
      local n_samples = output:nElement()
      local prob_state = torch.multinomialAliasSetup(probs)
      tester:assert(prob_state[1]:min() > 0, "Index ="..prob_state[1]:min().."alias indices has an index below or equal to 0")
      tester:assert(prob_state[1]:max() <= n_class, prob_state[1]:max().." alias indices has an index exceeding num_class")
      local prob_state = torch.multinomialAliasSetup(probs, prob_state)
      tester:assert(prob_state[1]:min() > 0, "Index ="..prob_state[1]:min().."alias indices has an index below or equal to 0(cold)")
      tester:assert(prob_state[1]:max() <= n_class, prob_state[1]:max()..","..prob_state[1]:min().." alias indices has an index exceeding num_class(cold)")
      local output = torch.LongTensor(n_samples)
      output = torch.multinomialAlias(output, prob_state)
      tester:assert(output:nElement() == n_samples, "wrong number of samples")
      tester:assert(output:min() > 0, "sampled indices has an index below or equal to 0")
      tester:assert(output:max() <= n_class, "indices has an index exceeding num_class")
   end

end
function torch_main_tests.multinomialvector()
   local n_col = 4
   local t=os.time()
   torch.manualSeed(t)
   local prob_dist = torch.rand(n_col)
   local n_sample = n_col
   local sample_indices = torch.multinomial(prob_dist, n_sample, true)
   local s_dim = sample_indices:dim()
   tester:assert(s_dim == 1, "wrong number of returned dimensions: "..s_dim)
   tester:assert(prob_dist:dim() == 1, "wrong number of prob_dist dimensions")
   tester:assert(sample_indices:size(1) == n_sample, "wrong number of samples")
end
function torch_main_tests.range()
   local mx = torch.range(0,1)
   local mxx = torch.Tensor()
   torch.range(mxx,0,1)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.range value')

   -- Check range for non-contiguous tensors.
   local x = torch.zeros(2, 3)
   local y = x:narrow(2, 2, 2)
   y:range(0, 3)
   tester:assertTensorEq(x, torch.Tensor{{0, 0, 1}, {0, 2, 3}}, 1e-16,
                           'non-contiguous range failed')
end
function torch_main_tests.rangenegative()
   local mx = torch.Tensor({1,0})
   local mxx = torch.Tensor()
   torch.range(mxx,1,0,-1)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.range value for negative step')
end
function torch_main_tests.rangeequalbounds()
   local mx = torch.Tensor({1})
   local mxx = torch.Tensor()
   torch.range(mxx,1,1,-1)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.range value for equal bounds step')
   torch.range(mxx,1,1,1)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.range value for equal bounds step')
end
function torch_main_tests.rangefloat()
   local mx = torch.FloatTensor():range(0.6, 0.9, 0.1)
   tester:asserteq(mx:size(1), 4, 'wrong size for FloatTensor range')
   mx = torch.FloatTensor():range(1, 10, 0.3)
   tester:asserteq(mx:size(1), 31, 'wrong size for FloatTensor range')
end
function torch_main_tests.rangedouble()
   local mx = torch.DoubleTensor():range(0.6, 0.9, 0.1)
   tester:asserteq(mx:size(1), 4, 'wrong size for DoubleTensor range')
   mx = torch.DoubleTensor():range(1, 10, 0.3)
   tester:asserteq(mx:size(1), 31, 'wrong size for DoubleTensor range')
end
function torch_main_tests.randperm()
   local t=os.time()
   torch.manualSeed(t)
   local mx = torch.randperm(msize)
   local mxx = torch.Tensor()
   torch.manualSeed(t)
   torch.randperm(mxx,msize)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.randperm value')
end
function torch_main_tests.reshape()
   local x = torch.rand(10,13,23)
   local mx = torch.reshape(x,130,23)
   local mxx = torch.Tensor()
   torch.reshape(mxx,x,130,23)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.reshape value')
end

local function assertIsOrdered(order, x, mxx, ixx, task)
  local areOrdered
  if order == 'descending' then
    areOrdered = function(a, b) return a >= b end
  elseif order == 'ascending' then
    areOrdered = function(a, b) return a <= b end
  else
    error('unknown order "' .. order .. '", must be "ascending" or "descending"')
  end

  local decreasing = true
  for j = 1,msize do
    for k = 2,msize do
      decreasing = decreasing and areOrdered(mxx[j][k-1], mxx[j][k])
    end
  end
  tester:assert(decreasing, 'torch.sort (' .. order .. ') values unordered for ' .. task)
  local seen = torch.ByteTensor(msize)
  local indicesCorrect = true
  for k = 1,msize do
    seen:zero()
    for j = 1,msize do
      indicesCorrect = indicesCorrect and (x[k][ixx[k][j]] == mxx[k][j])
      seen[ixx[k][j]] = 1
    end
    indicesCorrect = indicesCorrect and (torch.sum(seen) == msize)
  end
  tester:assert(indicesCorrect, 'torch.sort (' .. order .. ') indices wrong for ' .. task)
end

function torch_main_tests.sortAscending()
   local x = torch.rand(msize,msize)
   local mx,ix = torch.sort(x)

   -- Test use of result tensor
   local mxx = torch.Tensor()
   local ixx = torch.LongTensor()
   torch.sort(mxx,ixx,x)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.sort (ascending) value')
   tester:asserteq(maxdiff(ix,ixx),0,'torch.sort (ascending) index')

   -- Test sorting of random numbers
   assertIsOrdered('ascending', x, mxx, ixx, 'random')

   tester:assertTensorEq(
           torch.sort(torch.Tensor{ 50, 40, 30, 20, 10 }),
           torch.Tensor{ 10, 20, 30, 40, 50 },
           1e-16,
           "torch.sort (ascending) simple sort"
       )
   -- Test that we still have proper sorting with duplicate keys
   local x = torch.floor(torch.rand(msize,msize)*10)
   torch.sort(mxx,ixx,x)
   assertIsOrdered('ascending', x, mxx, ixx, 'random with duplicate keys')
end

function torch_main_tests.sortDescending()
   local x = torch.rand(msize,msize)
   local mx,ix = torch.sort(x,true)

   -- Test use of result tensor
   local mxx = torch.Tensor()
   local ixx = torch.LongTensor()
   torch.sort(mxx,ixx,x,true)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.sort (descending) value')
   tester:asserteq(maxdiff(ix,ixx),0,'torch.sort (descending) index')

   -- Test sorting of random numbers
   assertIsOrdered('descending', x, mxx, ixx, 'random')

   -- Test simple sort task
   tester:assertTensorEq(
           torch.sort(torch.Tensor{ 10, 20, 30, 40, 50 },true),
           torch.Tensor{ 50, 40, 30, 20, 10 },
           1e-16,
           "torch.sort (descending) simple sort"
       )

   -- Test that we still have proper sorting with duplicate keys
   assertIsOrdered('descending', x, mxx, ixx, 'random with duplicate keys')
end

function torch_main_tests.topK()
   local function topKViaSort(t, k, dim, dir)
      local sorted, indices = t:sort(dim, dir)
      return sorted:narrow(dim, 1, k), indices:narrow(dim, 1, k)
   end

   local function compareTensors(t, res1, ind1, res2, ind2, dim, msg)
      -- Values should be exactly equivalent
      tester:assertTensorEq(res1, res2, 0, msg)

      -- Indices might differ based on the implementation, since there is
      -- no guarantee of the relative order of selection
      if ind1:eq(ind2):min() == 0 then
         -- To verify that the indices represent equivalent elements,
         -- gather from the input using the topk indices and compare against
         -- the sort indices
         local vals = t:gather(dim, ind2)
         tester:assertTensorEq(res1, vals, 0, msg)
      end
   end

   local function compare(t, k, dim, dir, msg)
      local topKVal, topKInd = t:topk(k, dim, dir, true)
      local sortKVal, sortKInd = topKViaSort(t, k, dim, dir)

      compareTensors(t, sortKVal, sortKInd, topKVal, topKInd, dim, msg)
   end

   local t = torch.rand(math.random(1, msize),
                        math.random(1, msize),
                        math.random(1, msize))

   for kTries = 1, 3 do
      for dimTries = 1, 3 do
         for _, transpose in ipairs({true, false}) do
            for _, dir in ipairs({true, false}) do
               local testTensor = t

               local transposeMsg = nil
               if transpose then
                  local dim1 = math.random(1, t:nDimension())
                  local dim2 = dim1

                  while dim1 == dim2 do
                     dim2 = math.random(1, t:nDimension())
                  end

                  testTensor = t:transpose(dim1, dim2)
                  transposeMsg = 'transpose(' .. dim1 .. ', ' .. dim2 .. ')'
               end

               local dim = math.random(1, testTensor:nDimension())
               local k = math.random(1, testTensor:size(dim))
               local msg = 'topk(' .. k .. ', ' .. dim .. ', ' .. tostring(dir) .. ', true)'
               if transposeMsg then
                  msg = msg .. ' ' .. transposeMsg
               end

               compare(testTensor, k, dim, dir, msg)
            end
         end
      end
   end
end

function torch_main_tests.kthvalue()
   local x = torch.rand(msize, msize, msize)
   local x0 = x:clone()
   do
      local k = math.random(1, msize)
      local mx, ix = torch.kthvalue(x, k)
      local mxx, ixx = torch.sort(x)

      tester:assertTensorEq(mxx:select(3, k), mx:select(3, 1), 0,
                              'torch.kthvalue value')
      tester:assertTensorEq(ixx:select(3, k), ix:select(3, 1), 0,
                              'torch.kthvalue index')
   end
   do -- test use of result tensors
      local k = math.random(1, msize)
      local mx = torch.Tensor()
      local ix = torch.LongTensor()
      torch.kthvalue(mx, ix, x, k)
      local mxx, ixx = torch.sort(x)
      tester:assertTensorEq(mxx:select(3, k), mx:select(3, 1), 0,
                              'torch.kthvalue value')
      tester:assertTensorEq(ixx:select(3, k), ix:select(3, 1), 0,
                              'torch.kthvalue index')
   end
   do -- test non-default dim
      local k = math.random(1, msize)
      local mx, ix = torch.kthvalue(x, k, 1)
      local mxx, ixx = torch.sort(x, 1)
      tester:assertTensorEq(mxx:select(1, k), mx[1], 0,
                              'torch.kthvalue value')
      tester:assertTensorEq(ixx:select(1, k), ix[1], 0,
                              'torch.kthvalue index')
   end
   do -- non-contiguous
      local y = x:narrow(2, 1, 1)
      local y0 = y:clone()
      local k = math.random(1, msize)
      local my, ix = torch.kthvalue(y, k)
      local my0, ix0 = torch.kthvalue(y0, k)
      tester:assertTensorEq(my, my0, 0, 'torch.kthvalue value')
      tester:assertTensorEq(ix, ix0, 0, 'torch.kthvalue index')
   end
   tester:assertTensorEq(x, x0, 0, 'torch.kthvalue modified input')

   -- simple test case (with repetitions)
   local y = torch.Tensor{3,5,4,1,1,5}
   tester:assertTensorEq(torch.kthvalue(y, 3), torch.Tensor{3}, 1e-16,
      'torch.kthvalue simple')
   tester:assertTensorEq(torch.kthvalue(y, 2), torch.Tensor{1}, 1e-16,
      'torch.kthvalue simple')
end

function torch_main_tests.median()
   for _, msize in ipairs{155,156} do
      local x = torch.rand(msize, msize)
      local x0 = x:clone()

      local mx, ix = torch.median(x)
      local mxx, ixx = torch.sort(x)
      local ind = math.floor((msize+1)/2)

      tester:assertTensorEq(mxx:select(2, ind), mx:select(2, 1), 0,
                              'torch.median value')
      tester:assertTensorEq(ixx:select(2, ind), ix:select(2, 1), 0,
                              'torch.median index')

      -- Test use of result tensor
      local mr = torch.Tensor()
      local ir = torch.LongTensor()
      torch.median(mr, ir, x)
      tester:assertTensorEq(mr, mx, 0, 'torch.median result tensor value')
      tester:assertTensorEq(ir, ix, 0, 'torch.median result tensor index')

      -- Test non-default dim
      mx, ix = torch.median(x, 1)
      mxx, ixx = torch.sort(x, 1)
      tester:assertTensorEq(mxx:select(1, ind), mx[1], 0,
                              'torch.median value')
      tester:assertTensorEq(ixx:select(1, ind), ix[1], 0,
                              'torch.median index')

      -- input unchanged
      tester:assertTensorEq(x, x0, 0, 'torch.median modified input')
   end
end

function torch_main_tests.mode()
   local x = torch.range(1, msize * msize):reshape(msize, msize)
   x:select(1, 1):fill(1)
   x:select(1, 2):fill(1)
   x:select(2, 1):fill(1)
   x:select(2, 2):fill(1)
   local x0 = x:clone()

   -- Pre-calculated results.
   local res = torch.Tensor(msize):fill(1)
   -- The indices are the position of the last appearance of the mode element.
   local resix = torch.LongTensor(msize):fill(2)
   resix[1] = msize
   resix[2] = msize

   local mx, ix = torch.mode(x)

   tester:assertTensorEq(res:view(msize, 1), mx, 0, 'torch.mode value')
   tester:assertTensorEq(resix:view(msize, 1), ix, 0, 'torch.mode index')

   -- Test use of result tensor
   local mr = torch.Tensor()
   local ir = torch.LongTensor()
   torch.mode(mr, ir, x)
   tester:assertTensorEq(mr, mx, 0, 'torch.mode result tensor value')
   tester:assertTensorEq(ir, ix, 0, 'torch.mode result tensor index')

   -- Test non-default dim
   mx, ix = torch.mode(x, 1)
   tester:assertTensorEq(res:view(1, msize), mx, 0, 'torch.mode value')
   tester:assertTensorEq(resix:view(1, msize), ix, 0, 'torch.mode index')

   local input = torch.Tensor({
       {1, 2, 2, 2, 3, 2},
       {1.5, 2, 2, 1.5, 1.5, 5},
   })
   local value, index = torch.mode(input)
   local expected_value = torch.Tensor({{2}, {1.5}})
   tester:assertTensorEq(value, expected_value)

   -- input unchanged
   tester:assertTensorEq(x, x0, 0, 'torch.mode modified input')
end


function torch_main_tests.tril()
   local x = torch.rand(msize,msize)
   local mx = torch.tril(x)
   local mxx = torch.Tensor()
   torch.tril(mxx,x)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.tril value')
end
function torch_main_tests.triu()
   local x = torch.rand(msize,msize)
   local mx = torch.triu(x)
   local mxx = torch.Tensor()
   torch.triu(mxx,x)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.tril value')
end
function torch_main_tests.cat()
   for dim = 1, 3 do
      local x = torch.rand(13, msize, msize):transpose(1, dim)
      local y = torch.rand(17, msize, msize):transpose(1, dim)
      local mx = torch.cat(x, y, dim)
      tester:assertTensorEq(mx:narrow(dim, 1, 13), x, 0, 'torch.cat value')
      tester:assertTensorEq(mx:narrow(dim, 14, 17), y, 0, 'torch.cat value')

      local mxx = torch.Tensor()
      torch.cat(mxx, x, y, dim)
      tester:assertTensorEq(mx, mxx, 0, 'torch.cat value')

      local x = torch.rand(1,2,3)
      local y = torch.Tensor()
      local mx = torch.cat(x,y,dim)
      tester:asserteq(mx:size(1),1,'torch.cat size')
      tester:asserteq(mx:size(2),2,'torch.cat size')
      tester:asserteq(mx:size(3),3,'torch.cat size')
      tester:assertTensorEq(mx, x, 0, 'torch.cat value')

      local x = torch.Tensor()
      local y = torch.Tensor()
      local mx = torch.cat(x,y,dim)
      tester:asserteq(mx:dim(),0,'torch.cat dim')
   end
   local x = torch.Tensor()
   local y = torch.rand(1,2,3)
   local mx = torch.cat(x,y)
   tester:asserteq(mx:size(1),1,'torch.cat size')
   tester:asserteq(mx:size(2),2,'torch.cat size')
   tester:asserteq(mx:size(3),3,'torch.cat size')
   tester:assertTensorEq(mx, y, 0, 'torch.cat value')

   local x = torch.Tensor()
   local y = torch.Tensor()
   local mx = torch.cat(x,y)
   tester:asserteq(mx:dim(),0,'torch.cat dim')
end
function torch_main_tests.catArray()
   for dim = 1, 3 do
      local x = torch.rand(13, msize, msize):transpose(1, dim)
      local y = torch.rand(17, msize, msize):transpose(1, dim)
      local z = torch.rand(19, msize, msize):transpose(1, dim)

      local mx = torch.cat({x, y, z}, dim)
      tester:assertTensorEq(mx:narrow(dim, 1, 13), x, 0, 'torch.cat value')
      tester:assertTensorEq(mx:narrow(dim, 14, 17), y, 0, 'torch.cat value')
      tester:assertTensorEq(mx:narrow(dim, 31, 19), z, 0, 'torch.cat value')

      tester:assertError(function() torch.cat{} end, 'torch.cat empty table')

      local mxx = torch.Tensor()
      torch.cat(mxx, {x, y, z}, dim)
      tester:assertTensorEq(mx, mxx, 0, 'torch.cat value')
      torch.cat(mxx:float(), {x:float(), y:float(), z:float()}, dim)
      tester:assertTensorEq(mx, mxx, 0, 'torch.cat value')
      torch.cat(mxx:double(), {x:double(), y:double(), z:double()}, dim)
      tester:assertTensorEq(mx, mxx, 0, 'torch.cat value')

      local x = torch.rand(1,2,3)
      local y = torch.Tensor()
      local mx = torch.cat({x,y},dim)
      tester:asserteq(mx:size(1),1,'torch.cat size')
      tester:asserteq(mx:size(2),2,'torch.cat size')
      tester:asserteq(mx:size(3),3,'torch.cat size')
      tester:assertTensorEq(mx, x, 0, 'torch.cat value')

      local x = torch.Tensor()
      local y = torch.Tensor()
      local mx = torch.cat({x,y},dim)
      tester:asserteq(mx:dim(),0,'torch.cat dim')
   end
   local x = torch.Tensor()
   local y = torch.rand(1,2,3)
   local mx = torch.cat({x,y})
   tester:asserteq(mx:size(1),1,'torch.cat size')
   tester:asserteq(mx:size(2),2,'torch.cat size')
   tester:asserteq(mx:size(3),3,'torch.cat size')
   tester:assertTensorEq(mx, y, 0, 'torch.cat value')

   local x = torch.Tensor()
   local y = torch.Tensor()
   local mx = torch.cat({x,y})
   tester:asserteq(mx:dim(),0,'torch.cat dim')
end
function torch_main_tests.catNoDim()
   local a
   local b
   local c

   a = torch.Tensor(msize):uniform()
   b = torch.Tensor(msize):uniform()
   c = torch.cat(a, b)
   tester:assertTensorEq(c:narrow(1, 1, msize), a, 0, 'torch.cat value')
   tester:assertTensorEq(c:narrow(1, msize + 1, msize), b, 0, 'torch.cat value')

   a = torch.Tensor(1, msize):uniform()
   b = torch.Tensor(1, msize):uniform()
   c = torch.cat(a, b)
   tester:assertTensorEq(c:narrow(2, 1, msize), a, 0, 'torch.cat value')
   tester:assertTensorEq(c:narrow(2, msize + 1, msize), b, 0, 'torch.cat value')

   a = torch.Tensor(10, msize):uniform()
   b = torch.Tensor(10, msize):uniform()
   c = torch.cat(a, b)
   tester:assertTensorEq(c:narrow(2, 1, msize), a, 0, 'torch.cat value')
   tester:assertTensorEq(c:narrow(2, msize + 1, msize), b, 0, 'torch.cat value')
end
function torch_main_tests.sin_2()
   local x = torch.rand(msize,msize,msize)
   local mx = torch.sin(x)
   local mxx  = torch.Tensor()
   torch.sin(mxx,x)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.sin value')
end
function torch_main_tests.linspace()
   local from = math.random()
   local to = from+math.random()
   local mx = torch.linspace(from,to,137)
   local mxx = torch.Tensor()
   torch.linspace(mxx,from,to,137)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.linspace value')
   tester:assertError(function() torch.linspace(0,1,1) end, 'accepted 1 point between 2 distinct endpoints')
   tester:assertTensorEq(torch.linspace(0,0,1),torch.zeros(1),1e-16, 'failed to generate for torch.linspace(0,0,1)')

   -- Check linspace for generating with start > end.
   tester:assertTensorEq(torch.linspace(2,0,3),
                           torch.Tensor{2,1,0},
                           1e-16,
                           'failed to generate for torch.linspace(2,0,3)')

   -- Check linspace for non-contiguous tensors.
   local x = torch.zeros(2, 3)
   local y = x:narrow(2, 2, 2)
   y:linspace(0, 3, 4)
   tester:assertTensorEq(x, torch.Tensor{{0, 0, 1}, {0, 2, 3}}, 1e-16,
                           'non-contiguous linspace failed')
end
function torch_main_tests.logspace()
   local from = math.random()
   local to = from+math.random()
   local mx = torch.logspace(from,to,137)
   local mxx = torch.Tensor()
   torch.logspace(mxx,from,to,137)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.logspace value')
   tester:assertError(function() torch.logspace(0,1,1) end, 'accepted 1 point between 2 distinct endpoints')
   tester:assertTensorEq(torch.logspace(0,0,1),torch.ones(1),1e-16, 'failed to generate for torch.linspace(0,0,1)')

   -- Check logspace for generating with start > end.
   tester:assertTensorEq(torch.logspace(1,0,2),
                           torch.Tensor{10, 1},
                           1e-16,
                           'failed to generate for torch.logspace(1,0,2)')

   -- Check logspace for non-contiguous tensors.
   local x = torch.zeros(2, 3)
   local y = x:narrow(2, 2, 2)
   y:logspace(0, 3, 4)
   tester:assertTensorEq(x, torch.Tensor{{0, 1, 10}, {0, 100, 1000}}, 1e-16,
                           'non-contiguous logspace failed')
end
function torch_main_tests.rand()
   torch.manualSeed(123456)
   local mx = torch.rand(msize,msize)
   local mxx = torch.Tensor()
   torch.manualSeed(123456)
   torch.rand(mxx,msize,msize)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.rand value')
end
function torch_main_tests.randn()
   torch.manualSeed(123456)
   local mx = torch.randn(msize,msize)
   local mxx = torch.Tensor()
   torch.manualSeed(123456)
   torch.randn(mxx,msize,msize)
   tester:asserteq(maxdiff(mx,mxx),0,'torch.randn value')
end
function torch_main_tests.gesv()
   if not torch.gesv then return end
   local a=torch.Tensor({{6.80, -2.11,  5.66,  5.97,  8.23},
                         {-6.05, -3.30,  5.36, -4.44,  1.08},
                         {-0.45,  2.58, -2.70,  0.27,  9.04},
                         {8.32,  2.71,  4.35, -7.17,  2.14},
                         {-9.67, -5.14, -7.26,  6.08, -6.87}}):t()
   local b=torch.Tensor({{4.02,  6.19, -8.22, -7.57, -3.03},
                         {-1.56,  4.00, -8.67,  1.75,  2.86},
                         {9.81, -4.09, -4.57, -8.61,  8.99}}):t()
   local mx = torch.gesv(b,a)
   tester:assertlt(b:dist(a*mx),1e-12,'torch.gesv')
   local ta = torch.Tensor()
   local tb = torch.Tensor()
   local mxx = torch.gesv(tb,ta,b,a)
   local mxxx = torch.gesv(b,a,b,a)
   tester:asserteq(maxdiff(mx,tb),0,'torch.gesv value temp')
   tester:asserteq(maxdiff(mx,b),0,'torch.gesv value flag')
   tester:asserteq(maxdiff(mx,mxx),0,'torch.gesv value out1')
   tester:asserteq(maxdiff(mx,mxxx),0,'torch.gesv value out2')
end
function torch_main_tests.gesv_reuse()
   if not torch.gesv then return end
   local a=torch.Tensor({{6.80, -2.11,  5.66,  5.97,  8.23},
                         {-6.05, -3.30,  5.36, -4.44,  1.08},
                         {-0.45,  2.58, -2.70,  0.27,  9.04},
                         {8.32,  2.71,  4.35, -7.17,  2.14},
                         {-9.67, -5.14, -7.26,  6.08, -6.87}}):t()
   local b=torch.Tensor({{4.02,  6.19, -8.22, -7.57, -3.03},
                         {-1.56,  4.00, -8.67,  1.75,  2.86},
                         {9.81, -4.09, -4.57, -8.61,  8.99}}):t()
   local mx = torch.gesv(b,a)
   local ta = torch.Tensor()
   local tb = torch.Tensor()
   torch.gesv(tb,ta,b,a)
   tester:asserteq(maxdiff(mx,tb),0,'torch.gesv value temp')
   torch.gesv(tb,ta,b,a)
   tester:asserteq(maxdiff(mx,tb),0,'torch.gesv value reuse')
end
function torch_main_tests.trtrs()
   if not torch.trtrs then return end
   local a=torch.Tensor({{6.80, -2.11,  5.66,  5.97,  8.23},
                         {-6.05, -3.30,  5.36, -4.44,  1.08},
                         {-0.45,  2.58, -2.70,  0.27,  9.04},
                         {8.32,  2.71,  4.35, -7.17,  2.14},
                         {-9.67, -5.14, -7.26,  6.08, -6.87}}):t()
   local b=torch.Tensor({{4.02,  6.19, -8.22, -7.57, -3.03},
                         {-1.56,  4.00, -8.67,  1.75,  2.86},
                         {9.81, -4.09, -4.57, -8.61,  8.99}}):t()

   local U = torch.triu(a)
   local L = torch.tril(a)

   -- solve Ux = b
   local x = torch.trtrs(b, U)
   tester:assertlt(b:dist(U*x),1e-12,'torch.trtrs')
   x = torch.trtrs(b, U, 'U', 'N', 'N')
   tester:assertlt(b:dist(U*x),1e-12,'torch.trtrs')

   -- solve Lx = b
   x = torch.trtrs(b, L, 'L')
   tester:assertlt(b:dist(L*x),1e-12,'torch.trtrs')
   x = torch.trtrs(b, L, 'L', 'N', 'N')
   tester:assertlt(b:dist(L*x),1e-12,'torch.trtrs')

   -- solve U'x = b
   x = torch.trtrs(b, U, 'U', 'T')
   tester:assertlt(b:dist(U:t()*x),1e-12,'torch.trtrs')
   x = torch.trtrs(b, U, 'U', 'T', 'N')
   tester:assertlt(b:dist(U:t()*x),1e-12,'torch.trtrs')

   -- solve U'x = b by manual transposition
   local y = torch.trtrs(b, U:t(), 'L', 'N')
   tester:assertlt(x:dist(y),1e-12,'torch.trtrs')

   -- solve L'x = b
   x = torch.trtrs(b, L, 'L', 'T')
   tester:assertlt(b:dist(L:t()*x),1e-12,'torch.trtrs')
   x = torch.trtrs(b, L, 'L', 'T', 'N')
   tester:assertlt(b:dist(L:t()*x),1e-12,'torch.trtrs')

   -- solve L'x = b by manual transposition
   y = torch.trtrs(b, L:t(), 'U', 'N')
   tester:assertlt(x:dist(y),1e-12,'torch.trtrs')
end
function torch_main_tests.trtrs_reuse()
   if not torch.trtrs then return end
   local a=torch.Tensor({{6.80, -2.11,  5.66,  5.97,  8.23},
                         {-6.05, -3.30,  5.36, -4.44,  1.08},
                         {-0.45,  2.58, -2.70,  0.27,  9.04},
                         {8.32,  2.71,  4.35, -7.17,  2.14},
                         {-9.67, -5.14, -7.26,  6.08, -6.87}}):t()
   local b=torch.Tensor({{4.02,  6.19, -8.22, -7.57, -3.03},
                         {-1.56,  4.00, -8.67,  1.75,  2.86},
                         {9.81, -4.09, -4.57, -8.61,  8.99}}):t()
   local mx = torch.trtrs(b,a)
   local ta = torch.Tensor()
   local tb = torch.Tensor()
   torch.trtrs(tb,ta,b,a)
   tester:asserteq(maxdiff(mx,tb),0,'torch.trtrs value temp')
   tb:zero()
   torch.trtrs(tb,ta,b,a)
   tester:asserteq(maxdiff(mx,tb),0,'torch.trtrs value reuse')
end
function torch_main_tests.gels_uniquely_determined()
   if not torch.gels then return end
   local expectedNorm = 0
   local a=torch.Tensor({{ 1.44, -9.96, -7.55,  8.34},
                         {-7.84, -0.28,  3.24,  8.09},
                         {-4.39, -3.24,  6.27,  5.28},
                         {4.53,  3.83, -6.64,  2.06}}):t()
   local b=torch.Tensor({{8.58,  8.26,  8.48, -5.28},
                         {9.35, -4.43, -0.70, -0.26}}):t()
   local a_copy = a:clone()
   local b_copy = b:clone()
   local mx = torch.gels(b,a)
   tester:asserteq(maxdiff(a,a_copy),0,'torch.gels changed a')
   tester:asserteq(maxdiff(b,b_copy),0,'torch.gels changed b')
   tester:assertalmosteq((torch.mm(a,mx)-b):norm(), expectedNorm, 1e-8, 'torch.gels wrong answer')

   local ta = torch.Tensor()
   local tb = torch.Tensor()
   local mxx = torch.gels(tb,ta,b,a)
   tester:asserteq(maxdiff(a,a_copy),0,'torch.gels changed a')
   tester:asserteq(maxdiff(b,b_copy),0,'torch.gels changed b')
   tester:assertalmosteq((torch.mm(a,tb)-b):norm(), expectedNorm, 1e-8, 'torch.gels wrong answer')

   local mxxx = torch.gels(b,a,b,a)
   tester:assertalmosteq((torch.mm(a_copy,b)-b_copy):norm(), expectedNorm, 1e-8, 'torch.gels wrong answer')
   tester:asserteq(maxdiff(mx,tb),0,'torch.gels value temp')
   tester:asserteq(maxdiff(mx,b),0,'torch.gels value flag')
   tester:asserteq(maxdiff(mx,mxx),0,'torch.gels value out1')
   tester:asserteq(maxdiff(mx,mxxx),0,'torch.gels value out2')
end
function torch_main_tests.gels_reuse()
   if not torch.gels then return end
   local expectedNorm = 0
   local a=torch.Tensor({{ 1.44, -9.96, -7.55,  8.34},
                         {-7.84, -0.28,  3.24,  8.09},
                         {-4.39, -3.24,  6.27,  5.28},
                         {4.53,  3.83, -6.64,  2.06}}):t()
   local b=torch.Tensor({{8.58,  8.26,  8.48, -5.28},
                         {9.35, -4.43, -0.70, -0.26}}):t()
   local ta = torch.Tensor()
   local tb = torch.Tensor()
   torch.gels(tb,ta,b,a)
   tester:assertalmosteq((torch.mm(a,tb)-b):norm(), expectedNorm, 1e-8, 'torch.gels wrong answer')
   torch.gels(tb,ta,b,a)
   tester:assertalmosteq((torch.mm(a,tb)-b):norm(), expectedNorm, 1e-8, 'torch.gels wrong answer')
   torch.gels(tb,ta,b,a)
   tester:assertalmosteq((torch.mm(a,tb)-b):norm(), expectedNorm, 1e-8, 'torch.gels wrong answer')
end
function torch_main_tests.gels_overdetermined()
   if not torch.gels then return end
   local expectedNorm = 17.390200628863
   local a=torch.Tensor({{ 1.44, -9.96, -7.55,  8.34,  7.08, -5.45},
                         {-7.84, -0.28,  3.24,  8.09,  2.52, -5.70},
                         {-4.39, -3.24,  6.27,  5.28,  0.74, -1.19},
                         {4.53,  3.83, -6.64,  2.06, -2.47,  4.70}}):t()
   local b=torch.Tensor({{8.58,  8.26,  8.48, -5.28,  5.72,  8.93},
                         {9.35, -4.43, -0.70, -0.26, -7.36, -2.52}}):t()
   local a_copy = a:clone()
   local b_copy = b:clone()
   local mx = torch.gels(b,a)
   tester:asserteq(maxdiff(a,a_copy),0,'torch.gels changed a')
   tester:asserteq(maxdiff(b,b_copy),0,'torch.gels changed b')
   tester:assertalmosteq((torch.mm(a, mx)-b):norm(), expectedNorm, 1e-8, 'torch.gels wrong answer')

   local ta = torch.Tensor()
   local tb = torch.Tensor()
   local mxx = torch.gels(tb,ta,b,a)
   tester:asserteq(maxdiff(a,a_copy),0,'torch.gels changed a')
   tester:asserteq(maxdiff(b,b_copy),0,'torch.gels changed b')
   tester:assertalmosteq((torch.mm(a,tb)-b):norm(), expectedNorm, 1e-8, 'torch.gels wrong answer')

   local mxxx = torch.gels(b,a,b,a)
   tester:assertalmosteq((torch.mm(a_copy,b)-b_copy):norm(), expectedNorm, 1e-8, 'torch.gels wrong answer')
   tester:asserteq(maxdiff(mx,tb),0,'torch.gels value temp')
   tester:asserteq(maxdiff(mx,b),0,'torch.gels value flag')
   tester:asserteq(maxdiff(mx,mxx),0,'torch.gels value out1')
   tester:asserteq(maxdiff(mx,mxxx),0,'torch.gels value out2')
end
function torch_main_tests.gels_underdetermined()
   if not torch.gels then return end
   local expectedNorm = 0
   local a=torch.Tensor({{ 1.44, -9.96, -7.55},
                         {-7.84, -0.28,  3.24},
                         {-4.39, -3.24,  6.27},
                         {4.53,  3.83, -6.64}}):t()
   local b=torch.Tensor({{8.58,  8.26,  8.48},
                         {9.35, -4.43, -0.70}}):t()

   local a_copy = a:clone()
   local b_copy = b:clone()
   local mx = torch.gels(b,a)
   tester:asserteq(maxdiff(a,a_copy),0,'torch.gels changed a')
   tester:asserteq(maxdiff(b,b_copy),0,'torch.gels changed b')
   tester:assertalmosteq((torch.mm(a,mx)-b):norm(), expectedNorm, 1e-8, 'torch.gels wrong answer')

   local ta = torch.Tensor()
   local tb = torch.Tensor()
   local mxx = torch.gels(tb,ta,b,a)
   tester:asserteq(maxdiff(a,a_copy),0,'torch.gels changed a')
   tester:asserteq(maxdiff(b,b_copy),0,'torch.gels changed b')
   tester:assertalmosteq((torch.mm(a,tb)-b):norm(), expectedNorm, 1e-8, 'torch.gels wrong answer')

   local mxxx = torch.gels(b,a,b,a)
   tester:assertalmosteq((torch.mm(a_copy,b)-b_copy):norm(), expectedNorm, 1e-8, 'torch.gels wrong answer')
   tester:asserteq(maxdiff(mx,tb),0,'torch.gels value temp')
   tester:asserteq(maxdiff(mx,b),0,'torch.gels value flag')
   tester:asserteq(maxdiff(mx,mxx),0,'torch.gels value out1')
   tester:asserteq(maxdiff(mx,mxxx),0,'torch.gels value out2')
end
function torch_main_tests.eig()
   if not torch.eig then return end
   local a=torch.Tensor({{ 1.96,  0.00,  0.00,  0.00,  0.00},
                         {-6.49,  3.80,  0.00,  0.00,  0.00},
                         {-0.47, -6.39,  4.17,  0.00,  0.00},
                         {-7.20,  1.50, -1.51,  5.70,  0.00},
                         {-0.65, -6.34,  2.67,  1.80, -7.10}}):t():clone()
   local e = torch.eig(a)
   local ee,vv = torch.eig(a,'V')
   local te = torch.Tensor()
   local tv = torch.Tensor()
   local eee,vvv = torch.eig(te,tv,a,'V')
   tester:assertlt(maxdiff(e,ee),1e-12,'torch.eig value')
   tester:assertlt(maxdiff(ee,eee),1e-12,'torch.eig value')
   tester:assertlt(maxdiff(ee,te),1e-12,'torch.eig value')
   tester:assertlt(maxdiff(vv,vvv),1e-12,'torch.eig value')
   tester:assertlt(maxdiff(vv,tv),1e-12,'torch.eig value')
end
function torch_main_tests.eig_reuse()
   if not torch.eig then return end
   local X = torch.randn(4,4)
   X = X:t()*X
   local e, v = torch.zeros(4,2), torch.zeros(4,4)
   torch.eig(e, v, X,'V')
   local Xhat = v * torch.diag(e:select(2,1)) * v:t()
   tester:assertTensorEq(X, Xhat, 1e-8, 'VeV\' wrong')
   tester:assert(not v:isContiguous(), 'V is contiguous')

   torch.eig(e, v, X, 'V')
   local Xhat = torch.mm(v, torch.mm(e:select(2,1):diag(), v:t()))
   tester:assertTensorEq(X, Xhat, 1e-8, 'VeV\' wrong')
   tester:assert(not v:isContiguous(), 'V is contiguous')
end
function torch_main_tests.eig_noncontig()
   if not torch.eig then return end
   local X = torch.randn(4,4)
   X = X:t()*X
   local e = torch.zeros(4,2,2)[{ {}, 2, {} }]
   local v = torch.zeros(4,2,4)[{ {}, 2, {} }]
   tester:assert(not v:isContiguous(), 'V is contiguous')
   tester:assert(not e:isContiguous(), 'E is contiguous')
   torch.eig(e, v, X,'V')
   local Xhat = v * torch.diag(e:select(2,1)) * v:t()
   tester:assertTensorEq(X, Xhat, 1e-8, 'VeV\' wrong')
end
function torch_main_tests.test_symeig()
  if not torch.symeig then return end
  local xval = torch.rand(100,3)
  local cov = torch.mm(xval:t(), xval)
  local rese = torch.zeros(3)
  local resv = torch.zeros(3,3)

  -- First call to symeig
  tester:assert(resv:isContiguous(), 'resv is not contiguous') -- PASS
  torch.symeig(rese, resv, cov:clone(), 'V')
  local ahat = resv*torch.diag(rese)*resv:t()
  tester:assertTensorEq(cov, ahat, 1e-8, 'VeV\' wrong') -- PASS

  -- Second call to symeig
  tester:assert(not resv:isContiguous(), 'resv is contiguous') -- FAIL
  torch.symeig(rese, resv, cov:clone(), 'V')
  local ahat = torch.mm(torch.mm(resv, torch.diag(rese)), resv:t())
  tester:assertTensorEq(cov, ahat, 1e-8, 'VeV\' wrong') -- FAIL
end
function torch_main_tests.symeig_noncontig()
  if not torch.symeig then return end
   local X = torch.rand(5,5)
   X = X:t()*X
   local e = torch.zeros(4,2):select(2,2)
   local v = torch.zeros(4,2,4)[{ {}, 2, {} }]
   tester:assert(not v:isContiguous(), 'V is contiguous')
   tester:assert(not e:isContiguous(), 'E is contiguous')
   torch.symeig(e, v, X,'V')
   local Xhat = v * torch.diag(e) * v:t()
   tester:assertTensorEq(X, Xhat, 1e-8, 'VeV\' wrong')
end
function torch_main_tests.svd()
   if not torch.svd then return end
   local a=torch.Tensor({{8.79,  6.11, -9.15,  9.57, -3.49,  9.84},
                         {9.93,  6.91, -7.93,  1.64,  4.02,  0.15},
                         {9.83,  5.04,  4.86,  8.83,  9.80, -8.99},
                         {5.45, -0.27,  4.85,  0.74, 10.00, -6.02},
                         {3.16,  7.98,  3.01,  5.80,  4.27, -5.31}}):t():clone()
   local u,s,v = torch.svd(a)
   local uu = torch.Tensor()
   local ss = torch.Tensor()
   local vv = torch.Tensor()
   local uuu,sss,vvv = torch.svd(uu,ss,vv,a)
   tester:asserteq(maxdiff(u,uu),0,'torch.svd')
   tester:asserteq(maxdiff(u,uuu),0,'torch.svd')
   tester:asserteq(maxdiff(s,ss),0,'torch.svd')
   tester:asserteq(maxdiff(s,sss),0,'torch.svd')
   tester:asserteq(maxdiff(v,vv),0,'torch.svd')
   tester:asserteq(maxdiff(v,vvv),0,'torch.svd')
end
function torch_main_tests.svd_reuse()
   if not torch.svd then return end
   local X = torch.randn(4,4)
   local U, S, V = torch.svd(X)
   local Xhat = torch.mm(U, torch.mm(S:diag(), V:t()))
   tester:assertTensorEq(X, Xhat, 1e-8, 'USV\' wrong')

   tester:assert(not U:isContiguous(), 'U is contiguous')
   torch.svd(U, S, V, X)
   local Xhat = torch.mm(U, torch.mm(S:diag(), V:t()))
   tester:assertTensorEq(X, Xhat, 1e-8, 'USV\' wrong')
end
function torch_main_tests.svd_noncontig()
   if not torch.svd then return end
   local X = torch.randn(5,5)
   local U = torch.zeros(5,2,5)[{ {}, 2, {} }]
   local S = torch.zeros(5,2)[{ {}, 2 }]
   local V = torch.zeros(5,2,5)[{ {}, 2, {} }]

   tester:assert(not U:isContiguous(), 'U is contiguous')
   tester:assert(not S:isContiguous(), 'S is contiguous')
   tester:assert(not V:isContiguous(), 'V is contiguous')
   torch.svd(U, S, V, X)
   local Xhat = torch.mm(U, torch.mm(S:diag(), V:t()))
   tester:assertTensorEq(X, Xhat, 1e-8, 'USV\' wrong')
end
function torch_main_tests.inverse()
   if not torch.inverse then return end
   local M = torch.randn(5,5)
   local MI = torch.inverse(M)
   local E = torch.eye(5)
   tester:assert(not MI:isContiguous(), 'MI is contiguous')
   tester:assertalmosteq(maxdiff(E,torch.mm(M,MI)), 0, 1e-8, 'inverse value')
   tester:assertalmosteq(maxdiff(E,torch.mm(MI,M)), 0, 1e-8, 'inverse value')

   local MII = torch.Tensor(5,5)
   torch.inverse(MII, M)
   tester:assert(not MII:isContiguous(), 'MII is contiguous')
   tester:asserteq(maxdiff(MII, MI), 0, 'inverse value in-place')
   -- second call, now that MII is transposed
   torch.inverse(MII, M)
   tester:assert(not MII:isContiguous(), 'MII is contiguous')
   tester:asserteq(maxdiff(MII, MI), 0, 'inverse value in-place')
end
function torch_main_tests.conv2()
   local x = torch.rand(math.floor(torch.uniform(50,100)),math.floor(torch.uniform(50,100)))
   local k = torch.rand(math.floor(torch.uniform(10,20)),math.floor(torch.uniform(10,20)))
   local imvc = torch.conv2(x,k)
   local imvc2 = torch.conv2(x,k,'V')
   local imfc = torch.conv2(x,k,'F')

   local ki = k:clone();
   local ks = k:storage()
   local kis = ki:storage()
   for i=ks:size(),1,-1 do kis[ks:size()-i+1]=ks[i] end
   local imvx = torch.xcorr2(x,ki)
   local imvx2 = torch.xcorr2(x,ki,'V')
   local imfx = torch.xcorr2(x,ki,'F')

   tester:asserteq(maxdiff(imvc,imvc2),0,'torch.conv2')
   tester:asserteq(maxdiff(imvc,imvx),0,'torch.conv2')
   tester:asserteq(maxdiff(imvc,imvx2),0,'torch.conv2')
   tester:asserteq(maxdiff(imfc,imfx),0,'torch.conv2')
   tester:assertlt(math.abs(x:dot(x)-torch.xcorr2(x,x)[1][1]),1e-10,'torch.conv2')

   local xx = torch.Tensor(2,x:size(1),x:size(2))
   xx[1]:copy(x)
   xx[2]:copy(x)
   local kk = torch.Tensor(2,k:size(1),k:size(2))
   kk[1]:copy(k)
   kk[2]:copy(k)

   local immvc = torch.conv2(xx,kk)
   local immvc2 = torch.conv2(xx,kk,'V')
   local immfc = torch.conv2(xx,kk,'F')

   tester:asserteq(maxdiff(immvc[1],immvc[2]),0,'torch.conv2')
   tester:asserteq(maxdiff(immvc[1],imvc),0,'torch.conv2')
   tester:asserteq(maxdiff(immvc2[1],imvc2),0,'torch.conv2')
   tester:asserteq(maxdiff(immfc[1],immfc[2]),0,'torch.conv2')
   tester:asserteq(maxdiff(immfc[1],imfc),0,'torch.conv2')
end

function torch_main_tests.conv3()
   local x = torch.rand(math.floor(torch.uniform(20,40)),
                        math.floor(torch.uniform(20,40)),
                        math.floor(torch.uniform(20,40)))
   local k = torch.rand(math.floor(torch.uniform(5,10)),
                        math.floor(torch.uniform(5,10)),
                        math.floor(torch.uniform(5,10)))
   local imvc = torch.conv3(x,k)
   local imvc2 = torch.conv3(x,k,'V')
   local imfc = torch.conv3(x,k,'F')

   local ki = k:clone();
   local ks = k:storage()
   local kis = ki:storage()
   for i=ks:size(),1,-1 do kis[ks:size()-i+1]=ks[i] end
   local imvx = torch.xcorr3(x,ki)
   local imvx2 = torch.xcorr3(x,ki,'V')
   local imfx = torch.xcorr3(x,ki,'F')

   tester:asserteq(maxdiff(imvc,imvc2),0,'torch.conv3')
   tester:asserteq(maxdiff(imvc,imvx),0,'torch.conv3')
   tester:asserteq(maxdiff(imvc,imvx2),0,'torch.conv3')
   tester:asserteq(maxdiff(imfc,imfx),0,'torch.conv3')
   tester:assertlt(math.abs(x:dot(x)-torch.xcorr3(x,x)[1][1][1]),4*1e-10,'torch.conv3')

   local xx = torch.Tensor(2,x:size(1),x:size(2),x:size(3))
   xx[1]:copy(x)
   xx[2]:copy(x)
   local kk = torch.Tensor(2,k:size(1),k:size(2),k:size(3))
   kk[1]:copy(k)
   kk[2]:copy(k)

   local immvc = torch.conv3(xx,kk)
   local immvc2 = torch.conv3(xx,kk,'V')
   local immfc = torch.conv3(xx,kk,'F')

   tester:asserteq(maxdiff(immvc[1],immvc[2]),0,'torch.conv3')
   tester:asserteq(maxdiff(immvc[1],imvc),0,'torch.conv3')
   tester:asserteq(maxdiff(immvc2[1],imvc2),0,'torch.conv3')
   tester:asserteq(maxdiff(immfc[1],immfc[2]),0,'torch.conv3')
   tester:asserteq(maxdiff(immfc[1],imfc),0,'torch.conv3')
end

function torch_main_tests.xcorr3_xcorr2_eq()
    local ix = math.floor(torch.uniform(20,40))
    local iy = math.floor(torch.uniform(20,40))
    local iz = math.floor(torch.uniform(20,40))
    local kx = math.floor(torch.uniform(5,10))
    local ky = math.floor(torch.uniform(5,10))
    local kz = math.floor(torch.uniform(5,10))

    local x = torch.rand(ix,iy,iz)
    local k = torch.rand(kx,ky,kz)

    local o3 = torch.xcorr3(x,k)
    local o32 = torch.zeros(o3:size())

    for i=1,o3:size(1) do
        for j=1,k:size(1) do
            o32[i]:add(torch.xcorr2(x[i+j-1],k[j]))
        end
    end

    tester:assertlt(maxdiff(o3,o32),precision,'torch.conv3_conv2_eq')
end

function torch_main_tests.fxcorr3_fxcorr2_eq()
    local ix = math.floor(torch.uniform(20,40))
    local iy = math.floor(torch.uniform(20,40))
    local iz = math.floor(torch.uniform(20,40))
    local kx = math.floor(torch.uniform(5,10))
    local ky = math.floor(torch.uniform(5,10))
    local kz = math.floor(torch.uniform(5,10))

    local x = torch.rand(ix,iy,iz)
    local k = torch.rand(kx,ky,kz)

    local o3 = torch.xcorr3(x,k,'F')

    local o32 = torch.zeros(o3:size())

    for i=1,x:size(1) do
        for j=1,k:size(1) do
            o32[i+j-1]:add(torch.xcorr2(x[i],k[k:size(1)-j + 1],'F'))
        end
    end

    tester:assertlt(maxdiff(o3,o32),precision,'torch.conv3_conv2_eq')
end

function torch_main_tests.conv3_conv2_eq()
    local ix = math.floor(torch.uniform(20,40))
    local iy = math.floor(torch.uniform(20,40))
    local iz = math.floor(torch.uniform(20,40))
    local kx = math.floor(torch.uniform(5,10))
    local ky = math.floor(torch.uniform(5,10))
    local kz = math.floor(torch.uniform(5,10))

    local x = torch.rand(ix,iy,iz)
    local k = torch.rand(kx,ky,kz)

    local o3 = torch.conv3(x,k)
    local o32 = torch.zeros(o3:size())

    for i=1,o3:size(1) do
        for j=1,k:size(1) do
            o32[i]:add(torch.conv2(x[i+j-1],k[k:size(1)-j+1]))
        end
    end

    tester:assertlt(maxdiff(o3,o32),precision,'torch.conv3_conv2_eq')
end

function torch_main_tests.fconv3_fconv2_eq()
    local ix = math.floor(torch.uniform(20,40))
    local iy = math.floor(torch.uniform(20,40))
    local iz = math.floor(torch.uniform(20,40))
    local kx = math.floor(torch.uniform(5,10))
    local ky = math.floor(torch.uniform(5,10))
    local kz = math.floor(torch.uniform(5,10))

    local x = torch.rand(ix,iy,iz)
    local k = torch.rand(kx,ky,kz)

    local o3 = torch.conv3(x,k,'F')

    local o32 = torch.zeros(o3:size())

    for i=1,x:size(1) do
        for j=1,k:size(1) do
            o32[i+j-1]:add(torch.conv2(x[i],k[j],'F'))
        end
    end

    tester:assertlt(maxdiff(o3,o32),precision,'torch.conv3_conv2_eq')
end

function torch_main_tests.logical()
   local x = torch.rand(100,100)*2-1;
   local xx = x:clone()

   local xgt = torch.gt(x,1)
   local xlt = torch.lt(x,1)

   local xeq = torch.eq(x,1)
   local xne = torch.ne(x,1)

   local neqs = xgt+xlt
   local all = neqs + xeq
   tester:asserteq(neqs:sum(), xne:sum(), 'torch.logical')
   tester:asserteq(x:nElement(),all:double():sum() , 'torch.logical')
end

function torch_main_tests.RNGState()
   local state = torch.getRNGState()
   local stateCloned = state:clone()
   local before = torch.rand(1000)

   tester:assert(state:ne(stateCloned):long():sum() == 0, 'getRNGState should have value semantics, but appears to have reference semantics')

   torch.setRNGState(state)
   local after = torch.rand(1000)
   tester:assertTensorEq(before, after, 1e-16, 'getRNGState/setRNGState not generating same sequence')
end

function torch_main_tests.RNGStateAliasing()
    torch.manualSeed(1)
    local unused = torch.uniform()

    -- Fork the random number stream at this point
    local gen = torch.Generator()
    torch.setRNGState(gen, torch.getRNGState())

    local target_value = torch.rand(1000)
    --Dramatically alter the internal state of the main generator
    local also_unused = torch.rand(100000)
    local forked_value = torch.rand(gen, 1000)
    tester:assertTensorEq(target_value, forked_value, 1e-16, "RNG has not forked correctly.")
end

--function torch_main_tests.serializeGenerator()
--   local generator = torch.Generator()
--   torch.manualSeed(generator, 123)
--   local differentGenerator = torch.Generator()
--   torch.manualSeed(differentGenerator, 124)
--   local serializedGenerator = torch.serialize(generator)
--   local deserializedGenerator = torch.deserialize(serializedGenerator)
--   local generated = torch.random(generator)
--   local differentGenerated = torch.random(differentGenerator)
--   local deserializedGenerated = torch.random(deserializedGenerator)
--   tester:asserteq(generated, deserializedGenerated, 'torch.Generator changed internal state after being serialized')
--   tester:assertne(generated, differentGenerated, 'Generators with different random seed should not produce the same output')
--end

function torch_main_tests.testBoxMullerState()
    torch.manualSeed(123)
    local odd_number = 101
    local seeded = torch.randn(odd_number)
    local state = torch.getRNGState()
    local midstream = torch.randn(odd_number)
    torch.setRNGState(state)
    local repeat_midstream = torch.randn(odd_number)
    torch.manualSeed(123)
    local reseeded = torch.randn(odd_number)
    tester:assertTensorEq(midstream, repeat_midstream, 1e-16, 'getRNGState/setRNGState not generating same sequence of normally distributed numbers')
    tester:assertTensorEq(seeded, reseeded, 1e-16, 'repeated calls to manualSeed not generating same sequence of normally distributed numbers')
end

function torch_main_tests.testCholesky()
   local x = torch.rand(10,10)
   local A = torch.mm(x, x:t())

   ---- Default Case
   local C = torch.potrf(A)
   local B = torch.mm(C:t(), C)
   tester:assertTensorEq(A, B, 1e-14, 'potrf did not allow rebuilding the original matrix')

    ---- Test Upper Triangular
    local U = torch.potrf(A, 'U')
          B = torch.mm(U:t(), U)
    tester:assertTensorEq(A, B, 1e-14, 'potrf (upper) did not allow rebuilding the original matrix')

    ---- Test Lower Triangular
    local L = torch.potrf(A, 'L')
          B = torch.mm(L, L:t())
    tester:assertTensorEq(A, B, 1e-14, 'potrf (lower) did not allow rebuilding the original matrix')
end

function torch_main_tests.potrs()
   if not torch.potrs then return end
   local a=torch.Tensor({{6.80, -2.11,  5.66,  5.97,  8.23},
                         {-6.05, -3.30,  5.36, -4.44,  1.08},
                         {-0.45,  2.58, -2.70,  0.27,  9.04},
                         {8.32,  2.71,  4.35, -7.17,  2.14},
                         {-9.67, -5.14, -7.26,  6.08, -6.87}}):t()
   local b=torch.Tensor({{4.02,  6.19, -8.22, -7.57, -3.03},
                         {-1.56,  4.00, -8.67,  1.75,  2.86},
                         {9.81, -4.09, -4.57, -8.61,  8.99}}):t()

   ---- Make sure 'a' is symmetric PSD
   a = torch.mm(a, a:t())

   ---- Upper Triangular Test
   local U = torch.potrf(a, 'U')
   local x = torch.potrs(b, U, 'U')
   tester:assertlt(b:dist(a*x),1e-12,"torch.potrs; uplo='U'")

   ---- Lower Triangular Test
   local L = torch.potrf(a, 'L')
   x = torch.potrs(b, L, 'L')
   tester:assertlt(b:dist(a*x),1e-12,"torch.potrs; uplo='L")
end

function torch_main_tests.potri()
   if not torch.potrs then return end
   local a=torch.Tensor({{6.80, -2.11,  5.66,  5.97,  8.23},
                         {-6.05, -3.30,  5.36, -4.44,  1.08},
                         {-0.45,  2.58, -2.70,  0.27,  9.04},
                         {8.32,  2.71,  4.35, -7.17,  2.14},
                         {-9.67, -5.14, -7.26,  6.08, -6.87}}):t()

   ---- Make sure 'a' is symmetric PSD
   a = torch.mm(a, a:t())

   ---- Compute inverse directly
   local inv0 = torch.inverse(a)

   ---- Default case
   local chol = torch.potrf(a)
   local inv1 = torch.potri(chol)
   tester:assertlt(inv0:dist(inv1),1e-12,"torch.potri; uplo=''")

   ---- Upper Triangular Test
   chol = torch.potrf(a, 'U')
   inv1 = torch.potri(chol, 'U')
   tester:assertlt(inv0:dist(inv1),1e-12,"torch.potri; uplo='U'")

   ---- Lower Triangular Test
   chol = torch.potrf(a, 'L')
   inv1 = torch.potri(chol, 'L')
   tester:assertlt(inv0:dist(inv1),1e-12,"torch.potri; uplo='L'")
end

function torch_main_tests.pstrf()
  local function checkPsdCholesky(a, uplo, inplace)
    local u, piv, args, a_reconstructed
    if inplace then
      u = torch.Tensor(a:size())
      piv = torch.IntTensor(a:size(1))
      args = {u, piv, a}
    else
      args = {a}
    end

    if uplo then table.insert(args, uplo) end

    u, piv = torch.pstrf(unpack(args))

    if uplo == 'L' then
      a_reconstructed = torch.mm(u, u:t())
    else
      a_reconstructed = torch.mm(u:t(), u)
    end

    piv = piv:long()
    local a_permuted = a:index(1, piv):index(2, piv)
    tester:assertTensorEq(a_permuted, a_reconstructed, 1e-14,
                            'torch.pstrf did not allow rebuilding the original matrix;' ..
                            'uplo=' .. tostring(uplo))
  end

  local dimensions = { {5, 1}, {5, 3}, {5, 5}, {10, 10} }
  for _, dim in pairs(dimensions) do
    local m = torch.Tensor(unpack(dim)):uniform()
    local a = torch.mm(m, m:t())
    -- add a small number to the diagonal to make the matrix numerically positive semidefinite
    for i = 1, m:size(1) do
      a[i][i] = a[i][i] + 1e-7
    end
    checkPsdCholesky(a, nil, false)
    checkPsdCholesky(a, 'U', false)
    checkPsdCholesky(a, 'L', false)
    checkPsdCholesky(a, nil, true)
    checkPsdCholesky(a, 'U', true)
    checkPsdCholesky(a, 'L', true)
  end
end

function torch_main_tests.testNumel()
    local b = torch.ByteTensor(3, 100, 100)
    tester:asserteq(b:nElement(), 3*100*100, "nElement not right")
    tester:asserteq(b:numel(), 3*100*100, "numel not right")
end


-- Generate a tensor of size `size` whose values are ascending integers from
-- `start` (or 1, if `start is not given)
local function consecutive(size, start)
    local sequence = torch.ones(torch.Tensor(size):prod(1)[1]):cumsum(1)
    if start then
        sequence:add(start - 1)
    end
    return sequence:resize(unpack(size))
end

function torch_main_tests.index()
    local badIndexMsg = "Lookup with valid index should return correct result"
    local reference = consecutive{3, 3, 3}
    tester:assertTensorEq(reference[1], consecutive{3, 3}, 1e-16, badIndexMsg)
    tester:assertTensorEq(reference[2], consecutive({3, 3}, 10), 1e-16, badIndexMsg)
    tester:assertTensorEq(reference[3], consecutive({3, 3}, 19), 1e-16, badIndexMsg)
    tester:assertTensorEq(reference[{1}], consecutive{3, 3}, 1e-16, badIndexMsg)
    tester:assertTensorEq(reference[{2}], consecutive({3, 3}, 10), 1e-16, badIndexMsg)
    tester:assertTensorEq(reference[{3}], consecutive({3, 3}, 19), 1e-16, badIndexMsg)
    tester:assertTensorEq(reference[{1,2}], consecutive({3}, 4), 1e-16, badIndexMsg)
    tester:assertTensorEq(reference[{{1,2}}], consecutive({2, 3, 3}), 1e-16, badIndexMsg)
    tester:asserteq(reference[{3, 3, 3}], 27, badIndexMsg)
    tester:assertTensorEq(reference[{}], consecutive{3, 3, 3}, 1e-16, badIndexMsg)

    local shouldErrorMsg = "Lookup with too many indices should error"
    tester:assertError(function() return reference[{1, 1, 1, 1}] end, shouldErrorMsg)
    tester:assertError(function() return reference[{1, 1, 1, {1, 1}}] end, shouldErrorMsg)
    tester:assertError(function() return reference[{3, 3, 3, 3, 3, 3, 3, 3}] end, shouldErrorMsg)
end

function torch_main_tests.newIndex()
    local badIndexMsg = "Assignment to valid index should produce correct result"
    local reference = consecutive{3, 3, 3}
    -- This relies on __index__() being correct - but we have separate tests for that
    local function checkPartialAssign(index)
        local reference = torch.zeros(3, 3, 3)
        reference[index] = consecutive{3, 3, 3}[index]
        tester:assertTensorEq(reference[index], consecutive{3, 3, 3}[index], 1e-16, badIndexMsg)
        reference[index] = 0
        tester:assertTensorEq(reference, torch.zeros(3, 3, 3), 1e-16, badIndexMsg)
    end

    checkPartialAssign{1}
    checkPartialAssign{2}
    checkPartialAssign{3}
    checkPartialAssign{1,2}
    checkPartialAssign{2,3}
    checkPartialAssign{1,3}
    checkPartialAssign{}

    local shouldErrorMsg = "Assignment with too many indices should error"
    tester:assertError(function() reference[{1, 1, 1, 1}] = 1 end, shouldErrorMsg)
    tester:assertError(function() reference[{1, 1, 1, {1, 1}}] = 1 end, shouldErrorMsg)
    tester:assertError(function() reference[{3, 3, 3, 3, 3, 3, 3, 3}] = 1 end, shouldErrorMsg)
end

function torch_main_tests.indexCopy()
   local nCopy, nDest = 3, 20
   local dest = torch.randn(nDest,4,5)
   local src = torch.randn(nCopy,4,5)
   local idx = torch.randperm(nDest):narrow(1, 1, nCopy):long()
   local dest2 = dest:clone()
   dest:indexCopy(1, idx, src)
   for i=1,idx:size(1) do
      dest2[idx[i]]:copy(src[i])
   end
   tester:assertTensorEq(dest, dest2, 0.000001, "indexCopy tensor error")

   local dest = torch.randn(nDest)
   local src = torch.randn(nCopy)
   local idx = torch.randperm(nDest):narrow(1, 1, nCopy):long()
   local dest2 = dest:clone()
   dest:indexCopy(1, idx, src)
   for i=1,idx:size(1) do
      dest2[idx[i]] = src[i]
   end
   tester:assertTensorEq(dest, dest2, 0.000001, "indexCopy scalar error")
end

function torch_main_tests.indexAdd()
   local nCopy, nDest = 3, 20
   local dest = torch.randn(nDest,4,5)
   local src = torch.randn(nCopy,4,5)
   local idx = torch.randperm(nDest):narrow(1, 1, nCopy):long()
   local dest2 = dest:clone()
   dest:indexAdd(1, idx, src)
   for i=1,idx:size(1) do
      dest2[idx[i]]:add(src[i])
   end
   tester:assertTensorEq(dest, dest2, 0.000001, "indexAdd tensor error")

   local dest = torch.randn(nDest)
   local src = torch.randn(nCopy)
   local idx = torch.randperm(nDest):narrow(1, 1, nCopy):long()
   local dest2 = dest:clone()
   dest:indexAdd(1, idx, src)
   for i=1,idx:size(1) do
      dest2[idx[i]] = dest2[idx[i]] + src[i]
   end
   tester:assertTensorEq(dest, dest2, 0.000001, "indexAdd scalar error")
end

-- Fill idx with valid indices.
local function fillIdx(idx, dim, dim_size, elems_per_row, m, n, o)
   for i = 1, (dim == 1 and 1 or m) do
      for j = 1, (dim == 2 and 1 or n) do
         for k = 1, (dim == 3 and 1 or o) do
            local ii = {i, j, k}
            ii[dim] = {}
            idx[ii] = torch.randperm(dim_size)[{{1, elems_per_row}}]
         end
      end
   end
end

function torch_main_tests.gather()
   local m, n, o = torch.random(10, 20), torch.random(10, 20), torch.random(10, 20)
   local elems_per_row = torch.random(10)
   local dim = torch.random(3)

   local src = torch.randn(m, n, o)
   local idx_size = {m, n, o}
   idx_size[dim] = elems_per_row
   local idx = torch.LongTensor():resize(unpack(idx_size))
   fillIdx(idx, dim, src:size(dim), elems_per_row, m, n, o)

   local actual = torch.gather(src, dim, idx)
   local expected = torch.Tensor():resize(unpack(idx_size))
   for i = 1, idx_size[1] do
      for j = 1, idx_size[2] do
         for k = 1, idx_size[3] do
            local ii = {i, j, k}
            ii[dim] = idx[i][j][k]
            expected[i][j][k] = src[ii]
         end
      end
   end
   tester:assertTensorEq(actual, expected, 0, "Wrong values for gather")

   idx[1][1][1] = 23
   tester:assertError(function() torch.gather(src, dim, idx) end,
                        "Invalid index not detected")
end

function torch_main_tests.gatherMax()
   local src = torch.randn(3, 4, 5)
   local expected, idx = src:max(3)
   local actual = torch.gather(src, 3, idx)
   tester:assertTensorEq(actual, expected, 0, "Wrong values for gather")
end

function torch_main_tests.scatter()
   local m, n, o = torch.random(10, 20), torch.random(10, 20), torch.random(10, 20)
   local elems_per_row = torch.random(10)
   local dim = torch.random(3)

   local idx_size = {m, n, o}
   idx_size[dim] = elems_per_row
   local idx = torch.LongTensor():resize(unpack(idx_size))
   fillIdx(idx, dim, ({m, n, o})[dim], elems_per_row, m, n, o)
   local src = torch.Tensor():resize(unpack(idx_size)):normal()

   local actual = torch.zeros(m, n, o):scatter(dim, idx, src)
   local expected = torch.zeros(m, n, o)
   for i = 1, idx_size[1] do
      for j = 1, idx_size[2] do
         for k = 1, idx_size[3] do
            local ii = {i, j, k}
            ii[dim] = idx[i][j][k]
           expected[ii] = src[i][j][k]
         end
      end
   end
   tester:assertTensorEq(actual, expected, 0, "Wrong values for scatter")

   idx[1][1][1] = 34
   tester:assertError(function() torch.zeros(m, n, o):scatter(dim, idx, src) end,
                        "Invalid index not detected")
end

function torch_main_tests.scatterFill()
   local m, n, o = torch.random(10, 20), torch.random(10, 20), torch.random(10, 20)
   local elems_per_row = torch.random(10)
   local dim = torch.random(3)

   local val = torch.uniform()
   local idx_size = {m, n, o}
   idx_size[dim] = elems_per_row
   local idx = torch.LongTensor():resize(unpack(idx_size))
   fillIdx(idx, dim, ({m, n, o})[dim], elems_per_row, m, n, o)

   local actual = torch.zeros(m, n, o):scatter(dim, idx, val)
   local expected = torch.zeros(m, n, o)
   for i = 1, idx_size[1] do
      for j = 1, idx_size[2] do
         for k = 1, idx_size[3] do
            local ii = {i, j, k}
            ii[dim] = idx[i][j][k]
            expected[ii] = val
         end
      end
   end
   tester:assertTensorEq(actual, expected, 0, "Wrong values for scatter")

   idx[1][1][1] = 28
   tester:assertError(function() torch.zeros(m, n, o):scatter(dim, idx, val) end,
                        "Invalid index not detected")
end

function torch_main_tests.maskedCopy()
   local nCopy, nDest = 3, 10
   local dest = torch.randn(nDest)
   local src = torch.randn(nCopy)
   local mask = torch.ByteTensor{0,0,0,0,1,0,1,0,1,0}
   local dest2 = dest:clone()
   dest:maskedCopy(mask, src)
   local j = 1
   for i=1,nDest do
      if mask[i] == 1 then
         dest2[i] = src[j]
         j = j + 1
      end
   end
   tester:assertTensorEq(dest, dest2, 0.000001, "maskedCopy error")

   -- make source bigger than number of 1s in mask
   src = torch.randn(nDest)
   local ok = pcall(dest.maskedCopy, dest, mask, src)
   tester:assert(ok, "maskedCopy incorrect complaint when"
		      .. " src is bigger than mask's one count")

   src = torch.randn(nCopy - 1) -- make src smaller. this should fail
   local ok = pcall(dest.maskedCopy, dest, mask, src)
   tester:assert(not ok, "maskedCopy not erroring when"
		      .. " src is smaller than mask's one count")
end

function torch_main_tests.maskedSelect()
   local nSrc = 10
   local src = torch.randn(nSrc)
   local mask = torch.rand(nSrc):mul(2):floor():byte()
   local dst = torch.Tensor()
   dst:maskedSelect(src, mask)
   local dst2 = {}
   for i=1,nSrc do
      if mask[i] == 1 then
         table.insert(dst2, src[i])
      end
   end
   tester:assertTensorEq(dst, torch.DoubleTensor(dst2), 0.000001, "maskedSelect error")
end

function torch_main_tests.maskedFill()
   local nDst = 10
   local dst = torch.randn(nDst)
   local mask = torch.rand(nDst):mul(2):floor():byte()
   local val = math.random()
   local dst2 = dst:clone()
   dst:maskedFill(mask, val)
   for i=1,nDst do
      if mask[i] == 1 then
         dst2[i] = val
      end
   end
   tester:assertTensorEq(dst, dst2, 0.000001, "maskedFill error")
end

function torch_main_tests.abs()
   local size = 1000
   local range = 1000
   local original = torch.rand(size):mul(range)
   -- Tensor filled with {-1,1}
   local switch = torch.rand(size):mul(2):floor():mul(2):add(-1)

   local types = {'torch.DoubleTensor', 'torch.FloatTensor', 'torch.LongTensor', 'torch.IntTensor'}
   for k,t in ipairs(types) do
      local data = original:type(t)
      local switch = switch:type(t)
      local input = torch.cmul(data, switch)
      tester:assertTensorEq(input:abs(), data, 1e-16, 'Error in abs() for '..t)
   end

   -- Checking that the right abs function is called for LongTensor
   local bignumber
   if torch.LongTensor():elementSize() > 4 then
      bignumber = 2^31 + 1
   else
      bignumber = 2^15 + 1
   end
   local input = torch.LongTensor{-bignumber}
   tester:assertgt(input:abs()[1], 0, 'torch.abs(3)')
end

--function torch_main_tests.classInModule()
--    -- Need a global for this module
--    _mymodule123 = {}
--    local x = torch.class('_mymodule123.myclass')
--    tester:assert(x ~= nil, 'Could not create class in module')
--    -- Remove the global
--    _G['_mymodule123'] = nil
--    debug.getregistry()['_mymodule123.myclass']=nil
--end

--function torch_main_tests.classNoModule()
--    local x = torch.class('_myclass123')
--    tester:assert(x ~= nil, 'Could not create class in module')
--    debug.getregistry()['_myclass123'] = nil
--end

function torch_main_tests.type()
   local objects = {torch.DoubleTensor(), {}, nil, 2, "asdf"}
   local types = {'torch.DoubleTensor', 'table', 'nil', 'number', 'string'}
   for i,obj in ipairs(objects) do
      tester:assert(torch.type(obj) == types[i], "wrong type "..types[i])
   end
end

--function torch_main_tests.isTypeOfInheritance()
--   do
--      local A = torch.class('A')
--      local B, parB = torch.class('B', 'A')
--      local C, parC = torch.class('C', 'A')
--   end
--   local a, b, c = A(), B(), C()
--
--   tester:assert(torch.isTypeOf(a, 'A'), 'isTypeOf error, string spec')
--   tester:assert(torch.isTypeOf(a, A), 'isTypeOf error, constructor')
--   tester:assert(torch.isTypeOf(b, 'B'), 'isTypeOf error child class')
--   tester:assert(torch.isTypeOf(b, B), 'isTypeOf error child class ctor')
--   tester:assert(torch.isTypeOf(b, 'A'), 'isTypeOf error: inheritance')
--   tester:assert(torch.isTypeOf(b, A), 'isTypeOf error: inheritance')
--   tester:assert(not torch.isTypeOf(c, 'B'), 'isTypeOf error: common parent')
--   tester:assert(not torch.isTypeOf(c, B), 'isTypeOf error: common parent')
--   debug.getregistry()['A'] = nil
--   debug.getregistry()['B'] = nil
--   debug.getregistry()['C'] = nil
--end

--function torch_main_tests.isTypeOfPartial()
--    do
--      local TorchDummy = torch.class('TorchDummy')
--      local OtherTorchDummy = torch.class('OtherTorchDummy')
--      local TorchMember = torch.class('TorchMember')
--      local OtherTorchMember = torch.class('OtherTorchMember')
--      local FirstTorchMember = torch.class('FirstTorchMember',
--                                           'TorchMember')
--      local SecondTorchMember = torch.class('SecondTorchMember',
--                                            'TorchMember')
--      local ThirdTorchMember = torch.class('ThirdTorchMember',
--                                           'OtherTorchMember')
--   end
--   local td, otd = TorchDummy(), OtherTorchDummy()
--   local tm, ftm, stm, ttm = TorchMember(), FirstTorchMember(),
--      SecondTorchMember(), ThirdTorchMember()
--
--   tester:assert(not torch.isTypeOf(td, 'OtherTorchDummy'),
--                   'isTypeOf error: incorrect partial match')
--   tester:assert(not torch.isTypeOf(otd, 'TorchDummy'),
--                   'isTypeOf error: incorrect partial match')
--   tester:assert(torch.isTypeOf(tm, 'TorchMember'),
--                   'isTypeOf error, string spec')
--   tester:assert(torch.isTypeOf(tm, TorchMember),
--                   'isTypeOf error, constructor')
--   tester:assert(torch.isTypeOf(ftm, 'FirstTorchMember'),
--                   'isTypeOf error child class')
--   tester:assert(torch.isTypeOf(ftm, FirstTorchMember),
--                   'isTypeOf error child class ctor')
--   tester:assert(torch.isTypeOf(ftm, 'TorchMember'),
--                   'isTypeOf error: inheritance')
--   tester:assert(torch.isTypeOf(ftm, TorchMember),
--                   'isTypeOf error: inheritance')
--   tester:assert(not torch.isTypeOf(stm, 'FirstTorchMember'),
--                   'isTypeOf error: common parent')
--   tester:assert(not torch.isTypeOf(stm, FirstTorchMember),
--                   'isTypeOf error: common parent')
--   tester:assert(not torch.isTypeOf(ttm, TorchMember),
--                   'isTypeOf error: inheritance')
--   tester:assert(not torch.isTypeOf(ttm, 'TorchMember'),
--                   'isTypeOf error: inheritance')
--   debug.getregistry()['TorchDummy'] = nil
--   debug.getregistry()['OtherTorchDummy'] = nil
--   debug.getregistry()['TorchMember'] = nil
--   debug.getregistry()['OtherTorchMember'] = nil
--   debug.getregistry()['FirstTorchMember'] = nil
--   debug.getregistry()['SecondTorchMember'] = nil
--   debug.getregistry()['ThirdTorchMember'] = nil
--end

function torch_main_tests.isTypeOfPattern()
   local t = torch.LongTensor()
   tester:assert(torch.isTypeOf(t, torch.LongTensor),
                   'isTypeOf error: incorrect match')
   tester:assert(not torch.isTypeOf(t, torch.IntTensor),
                   'isTypeOf error: incorrect match')
   tester:assert(torch.isTypeOf(t, 'torch.LongTensor'),
                   'isTypeOf error: incorrect match')
   tester:assert(not torch.isTypeOf(t, 'torch.Long'),
                   'isTypeOf error: incorrect match')
   tester:assert(torch.isTypeOf(t, 'torch.*Tensor'),
                   'isTypeOf error: incorrect match')
   tester:assert(torch.isTypeOf(t, '.*Long'),
                   'isTypeOf error: incorrect match')
   tester:assert(not torch.isTypeOf(t, 'torch.IntTensor'),
                   'isTypeOf error: incorrect match')
end

local function torchtest_isTensor(func)
   local t = func(torch.randn(3,4))
   tester:assert(torch.isTensor(t), 'error in isTensor')
   tester:assert(torch.isTensor(t[1]), 'error in isTensor for subTensor')
   tester:assert(not torch.isTensor(t[1][2]), 'false positive in isTensor')
   tester:assert(torch.Tensor.isTensor(t), 'alias not working')
end

function torch_main_tests.isTensor()
   for k,v in ipairs({"real", "half"}) do
      torchtest_isTensor(torch.getmetatable(torch.Tensor():type())[v])
   end
end

local function torchtest_isStorage(func)
  local t = torch.randn(3,4)
  tester:assert(torch.isStorage(t:storage()), 'error in isStorage')
  tester:assert(not torch.isStorage(t), 'false positive in isStorage')
end

function torch_main_tests.isStorage()
   for k,v in ipairs({"real", "half"}) do
      torchtest_isStorage(torch.getmetatable(torch.Tensor():type())[v])
   end
end

local function torchtest_view(func)
   local tensor = func(torch.rand(15))
   local template = func(torch.rand(3,5))
   local target = template:size():totable()
   tester:assertTableEq(tensor:viewAs(template):size():totable(), target, 'Error in viewAs')
   tester:assertTableEq(tensor:view(3,5):size():totable(), target, 'Error in view')
   tester:assertTableEq(tensor:view(torch.LongStorage{3,5}):size():totable(), target, 'Error in view using LongStorage')
   tester:assertTableEq(tensor:view(-1,5):size():totable(), target, 'Error in view using dimension -1')
   tester:assertTableEq(tensor:view(3,-1):size():totable(), target, 'Error in view using dimension -1')
   local tensor_view = tensor:view(5,3)
   tensor_view:fill(torch.rand(1)[1])
   tester:asserteq((tensor_view-tensor):abs():max(), 0, 'Error in view')

   local target_tensor = func(torch.Tensor())
   tester:assertTableEq(target_tensor:viewAs(tensor, template):size():totable(), target, 'Error in viewAs')
   tester:assertTableEq(target_tensor:view(tensor, 3,5):size():totable(), target, 'Error in view')
   tester:assertTableEq(target_tensor:view(tensor, torch.LongStorage{3,5}):size():totable(), target, 'Error in view using LongStorage')
   tester:assertTableEq(target_tensor:view(tensor, -1,5):size():totable(), target, 'Error in view using dimension -1')
   tester:assertTableEq(target_tensor:view(tensor, 3,-1):size():totable(), target, 'Error in view using dimension -1')
   target_tensor:fill(torch.rand(1)[1])
   tester:asserteq((target_tensor-tensor):abs():max(), 0, 'Error in viewAs')
end

function torch_main_tests.view()
   for k,v in ipairs({"real", "half"}) do
      torchtest_view(torch.getmetatable(torch.Tensor():type())[v])
   end
end

local function torchtest_expand(func)
   local result = func(torch.Tensor())
   local tensor = func(torch.rand(8,1))
   local template = func(torch.rand(8,5))
   local target = template:size():totable()
   tester:assertTableEq(tensor:expandAs(template):size():totable(), target, 'Error in expandAs')
   tester:assertTableEq(tensor:expand(8,5):size():totable(), target, 'Error in expand')
   tester:assertTableEq(tensor:expand(torch.LongStorage{8,5}):size():totable(), target, 'Error in expand using LongStorage')
   result:expandAs(tensor,template)
   tester:assertTableEq(result:size():totable(), target, 'Error in expandAs using result')
   result:expand(tensor,8,5)
   tester:assertTableEq(result:size():totable(), target, 'Error in expand using result')
   result:expand(tensor,torch.LongStorage{8,5})
   tester:assertTableEq(result:size():totable(), target, 'Error in expand using result and LongStorage')
   tester:asserteq((result:mean(2):view(8,1)-tensor):abs():max(), 0, 'Error in expand (not equal)')
end

function torch_main_tests.expand()
   for k,v in ipairs({"real", "half"}) do
      torchtest_expand(torch.getmetatable(torch.Tensor():type())[v])
   end
end

local function torchtest_repeatTensor(func, mean)
   local result = func(torch.Tensor())
   local tensor = func(torch.rand(8,4))
   local size = {3,1,1}
   local sizeStorage = torch.LongStorage(size)
   local target = {3,8,4}
   tester:assertTableEq(tensor:repeatTensor(unpack(size)):size():totable(), target, 'Error in repeatTensor')
   tester:assertTableEq(tensor:repeatTensor(sizeStorage):size():totable(), target, 'Error in repeatTensor using LongStorage')
   result:repeatTensor(tensor,unpack(size))
   tester:assertTableEq(result:size():totable(), target, 'Error in repeatTensor using result')
   result:repeatTensor(tensor,sizeStorage)
   tester:assertTableEq(result:size():totable(), target, 'Error in repeatTensor using result and LongStorage')
   tester:asserteq((result:mean(1):view(8,4)-tensor):abs():max(), 0, 'Error in repeatTensor (not equal)')
end

function torch_main_tests.repeatTensor()
   for k,v in ipairs({"real", "half"}) do
      torchtest_repeatTensor(torch.getmetatable(torch.Tensor():type())[v])
   end
end

local function torchtest_isSameSizeAs(func)
   local t1 = func(torch.Tensor(3, 4, 9, 10))
   local t2 = func(torch.Tensor(3, 4))
   local t3 = func(torch.Tensor(1, 9, 3, 3))
   local t4 = func(torch.Tensor(3, 4, 9, 10))

   tester:assert(t1:isSameSizeAs(t2) == false, "wrong answer ")
   tester:assert(t1:isSameSizeAs(t3) == false, "wrong answer ")
   tester:assert(t1:isSameSizeAs(t4) == true, "wrong answer ")
end

function torch_main_tests.isSameSizeAs()
   for k,v in ipairs({"real", "half"}) do
      torchtest_isSameSizeAs(torch.getmetatable(torch.Tensor():type())[v])
   end
end

local function torchtest_isSetTo(func)
   local t1 = func(torch.Tensor(3, 4, 9, 10))
   local t2 = func(torch.Tensor(3, 4, 9, 10))
   local t3 = func(torch.Tensor()):set(t1)
   local t4 = t3:reshape(12, 90)
   tester:assert(t1:isSetTo(t2) == false, "tensors do not share storage")
   tester:assert(t1:isSetTo(t3) == true, "tensor is set to other")
   tester:assert(t3:isSetTo(t1) == true, "isSetTo should be symmetric")
   tester:assert(t1:isSetTo(t4) == false, "tensors have different view")
   tester:assert(not func(torch.Tensor()):isSetTo(func(torch.Tensor())),
                   "Tensors with no storages should not appear to be set " ..
                   "to each other")
end

function torch_main_tests.isSetTo()
   for k,v in ipairs({"real", "half"}) do
      torchtest_isSetTo(torch.getmetatable(torch.Tensor():type())[v])
   end
end

function torch_main_tests.equal()
  -- Contiguous, 1D
  local t1 = torch.Tensor{3, 4, 9, 10}
  local t2 = t1:clone()
  local t3 = torch.Tensor{1, 9, 3, 10}
  local t4 = torch.Tensor{3, 4, 9}
  local t5 = torch.Tensor()
  tester:assert(t1:equal(t2) == true, "wrong answer ")
  tester:assert(t1:equal(t3) == false, "wrong answer ")
  tester:assert(t1:equal(t4) == false, "wrong answer ")
  tester:assert(t1:equal(t5) == false, "wrong answer ")
  tester:assert(torch.equal(t1, t2) == true, "wrong answer ")
  tester:assert(torch.equal(t1, t3) == false, "wrong answer ")
  tester:assert(torch.equal(t1, t4) == false, "wrong answer ")
  tester:assert(torch.equal(t1, t5) == false, "wrong answer ")

  -- Non contiguous, 2D
  local s = torch.Tensor({{1, 2, 3, 4}, {5, 6, 7, 8}})
  local s1 = s[{{}, {2, 3}}]
  local s2 = s1:clone()
  local s3 = torch.Tensor({{2, 3}, {6, 7}})
  local s4 = torch.Tensor({{0, 0}, {0, 0}})

  tester:assert(not s1:isContiguous(), "wrong answer ")
  tester:assert(s1:equal(s2) == true, "wrong answer ")
  tester:assert(s1:equal(s3) == true, "wrong answer ")
  tester:assert(s1:equal(s4) == false, "wrong answer ")
  tester:assert(torch.equal(s1, s2) == true, "wrong answer ")
  tester:assert(torch.equal(s1, s3) == true, "wrong answer ")
  tester:assert(torch.equal(s1, s4) == false, "wrong answer ")
end

local function torchtest_isSize(func)
  local t1 = func(torch.Tensor(3, 4, 5))
  local s1 = torch.LongStorage({3, 4, 5})
  local s2 = torch.LongStorage({5, 4, 3})

   tester:assert(t1:isSize(s1) == true, "wrong answer ")
   tester:assert(t1:isSize(s2) == false, "wrong answer ")
   tester:assert(t1:isSize(t1:size()) == true, "wrong answer ")
end

function torch_main_tests.isSize()
   for k,v in ipairs({"real", "half"}) do
      torchtest_isSize(torch.getmetatable(torch.Tensor():type())[v])
   end
end

function torch_main_tests.elementSize()
  local byte   =   torch.ByteStorage():elementSize()
  local char   =   torch.CharStorage():elementSize()
  local short  =  torch.ShortStorage():elementSize()
  local int    =    torch.IntStorage():elementSize()
  local long   =   torch.LongStorage():elementSize()
  local float  =  torch.FloatStorage():elementSize()
  local double = torch.DoubleStorage():elementSize()
  local half = torch.HalfStorage():elementSize()

  tester:asserteq(byte,   torch.ByteTensor():elementSize())
  tester:asserteq(char,   torch.CharTensor():elementSize())
  tester:asserteq(short,  torch.ShortTensor():elementSize())
  tester:asserteq(int,    torch.IntTensor():elementSize())
  tester:asserteq(long,   torch.LongTensor():elementSize())
  tester:asserteq(float,  torch.FloatTensor():elementSize())
  tester:asserteq(double, torch.DoubleTensor():elementSize())
  tester:asserteq(half, torch.HalfTensor():elementSize())

  tester:assertne(byte, 0)
  tester:assertne(char, 0)
  tester:assertne(short, 0)
  tester:assertne(int, 0)
  tester:assertne(long, 0)
  tester:assertne(float, 0)
  tester:assertne(double, 0)
  tester:assertne(half, 0)

  -- These tests are portable, not necessarily strict for your system.
  tester:asserteq(byte, 1)
  tester:asserteq(char, 1)
  tester:assert(short >= 2)
  tester:assert(int >= 2)
  tester:assert(int >= short)
  tester:assert(long >= 4)
  tester:assert(long >= int)
  tester:assert(double >= float)
  tester:assert(half <= float)
end

local function torchtest_split(func)
   local result = {}
   local tensor = func(torch.rand(7,4))
   local splitSize = 3
   local targetSize = {{3,4},{3,4},{1,4}}
   local dim = 1
   local splits = tensor:split(splitSize, dim)
   local start = 1
   for i, split in ipairs(splits) do
      tester:assertTableEq(split:size():totable(), targetSize[i], 'Size error in split '..i)
      tester:assertTensorEq(tensor:narrow(dim, start, targetSize[i][dim]), split, 0.00001, 'Content error in split '..i)
      start = start + targetSize[i][dim]
   end
   torch.split(result, tensor, splitSize, dim)
   local start = 1
   for i, split in ipairs(result) do
      tester:assertTableEq(split:size():totable(), targetSize[i], 'Result size error in split '..i)
      tester:assertTensorEq(tensor:narrow(dim, start, targetSize[i][dim]), split, 0.000001, 'Result content error in split '..i)
      start = start + targetSize[i][dim]
   end
   tester:asserteq(#splits, #result, 'Non-consistent output size from split')
   for i, split in ipairs(splits) do
      tester:assertTensorEq(split,result[i], 0, 'Non-consistent outputs from split')
   end
end

function torch_main_tests.split()
   for k,v in ipairs({"real", "half"}) do
      torchtest_split(torch.getmetatable(torch.Tensor():type())[v])
   end
end

local function torchtest_chunk(func)
   local result = {}
   local tensor = func(torch.rand(4,7))
   local nChunk = 3
   local targetSize = {{4,3},{4,3},{4,1}}
   local dim = 2
   local splits = tensor:chunk(nChunk, dim)
   local start = 1
   for i, split in ipairs(splits) do
      tester:assertTableEq(split:size():totable(), targetSize[i], 'Size error in chunk '..i)
      tester:assertTensorEq(tensor:narrow(dim, start, targetSize[i][dim]), split, 0.00001, 'Content error in chunk '..i)
      start = start + targetSize[i][dim]
   end
   torch.split(result, tensor, nChunk, dim)
   local start = 1
   for i, split in ipairs(result) do
      tester:assertTableEq(split:size():totable(), targetSize[i], 'Result size error in chunk '..i)
      tester:assertTensorEq(tensor:narrow(dim, start, targetSize[i][dim]), split, 0.000001, 'Result content error in chunk '..i)
      start = start + targetSize[i][dim]
   end
end

function torch_main_tests.chunk()
   for k,v in ipairs({"real", "half"}) do
      torchtest_chunk(torch.getmetatable(torch.Tensor():type())[v])
   end
end

local function torchtest_totable(func, storageType)
  local table0D = {}
  local tensor0D = func(torch.Tensor(table0D))
  tester:assertTableEq(torch.totable(tensor0D), table0D, 'tensor0D:totable incorrect')

  local table1D = {1, 2, 3}
  local tensor1D = func(torch.Tensor(table1D))
  local storage = torch[storageType](table1D)
  tester:assertTableEq(tensor1D:totable(), table1D, 'tensor1D:totable incorrect')
  tester:assertTableEq(storage:totable(), table1D, 'storage:totable incorrect')
  tester:assertTableEq(torch.totable(tensor1D), table1D, 'torch.totable incorrect for Tensors')
  tester:assertTableEq(torch.totable(storage), table1D, 'torch.totable incorrect for Storages')

  local table2D = {{1, 2}, {3, 4}}
  local tensor2D = func(torch.Tensor(table2D))
  tester:assertTableEq(tensor2D:totable(), table2D, 'tensor2D:totable incorrect')

  local tensor3D = func(torch.Tensor({{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}}))
  local tensorNonContig = tensor3D:select(2, 2)
  tester:assert(not tensorNonContig:isContiguous(), 'invalid test')
  tester:assertTableEq(tensorNonContig:totable(), {{3, 4}, {7, 8}},
                         'totable() incorrect for non-contiguous tensors')
end

function torch_main_tests.table()
   local convStorage = {
     ['real'] = 'FloatStorage',
     ['half'] = 'HalfStorage'
   }
   for k,v in ipairs(convStorage) do
      torchtest_totable(torch.getmetatable(torch.Tensor():type())[k], v)
   end
end

local function torchtest_permute(func)
   local orig = {1,2,3,4,5,6,7}
   local perm = torch.randperm(7):totable()
   local x = torch.Tensor(unpack(orig)):fill(0)
   local new = x:permute(unpack(perm)):size():totable()
   tester:assertTableEq(perm, new, 'Tensor:permute incorrect')
   tester:assertTableEq(x:size():totable(), orig, 'Tensor:permute changes tensor')
 end

function torch_main_tests.permute()
   for k,v in ipairs({"real", "half"}) do
      torchtest_permute(torch.getmetatable(torch.Tensor():type())[v])
   end
end

--function torch_main_tests.serialize()
--   local tableObj = {6, a = 42}
--   local tensObj = torch.randn(3,4,5)
--
--   -- Test serializing a table
--   local serString = torch.serialize(tableObj)
--   local serStorage = torch.serializeToStorage(tableObj)
--   tester:assertTableEq(tableObj, torch.deserialize(serString))
--   tester:assertTableEq(tableObj, torch.deserializeFromStorage(serStorage))
--
--   -- Test serializing a Tensor
--   serString = torch.serialize(tensObj)
--   serStorage = torch.serializeToStorage(tensObj)
--   tester:assertTensorEq(tensObj, torch.deserialize(serString), 1e-10)
--   tester:assertTensorEq(tensObj, torch.deserializeFromStorage(serStorage), 1e-10)
--end

function torch_main_tests.storageview()
   local s1 = torch.LongStorage({3, 4, 5})
   local s2 = torch.LongStorage(s1, 2)

   tester:assert(s2:size() == 2, "should be size 2")
   tester:assert(s2[1] == s1[2], "should have 4 at position 1")
   tester:assert(s2[2] == s1[3], "should have 5 at position 2")

   s2[1] = 13
   tester:assert(13 == s1[2], "should have 13 at position 1")
end

function torch_main_tests.nonzero()
  local nSrc = 12

  local types = {
      'torch.ByteTensor',
      'torch.CharTensor',
      'torch.ShortTensor',
      'torch.IntTensor',
      'torch.FloatTensor',
      'torch.DoubleTensor',
      'torch.LongTensor',
  }

  local shapes = {
      torch.LongStorage{12},
      torch.LongStorage{12, 1},
      torch.LongStorage{1, 12},
      torch.LongStorage{6, 2},
      torch.LongStorage{3, 2, 2},
  }

  for _, type in ipairs(types) do
    local tensor = torch.rand(nSrc):mul(2):floor():type(type)
      for _, shape in ipairs(shapes) do
        tensor = tensor:reshape(shape)
        local dst1 = torch.nonzero(tensor)
        local dst2 = tensor:nonzero()
        -- Does not work. Torch uses the first argument to determine what
        -- type the Tensor is expected to be. In our case the second argument
        -- determines the type of Tensor.
        --local dst3 = torch.LongTensor()
        --torch.nonzero(dst3, tensor)
        -- However, there are workarounds to this issue when it is desired to
        -- use an existing tensor for the result:
        local dst4 = torch.LongTensor()
        tensor.nonzero(dst4, tensor)
        if shape:size() == 1 then
          local dst = {}
          for i = 1 , nSrc do
            if tensor[i] ~= 0 then
              table.insert(dst, i)
            end
          end
          tester:assertTensorEq(dst1:select(2, 1), torch.LongTensor(dst), 0.0,
                                  "nonzero error")
          tester:assertTensorEq(dst2:select(2, 1), torch.LongTensor(dst), 0.0,
                                  "nonzero error")
          --tester:assertTensorEq(dst3:select(2, 1), torch.LongTensor(dst),
          --                        0.0,  "nonzero error")
          tester:assertTensorEq(dst4:select(2, 1), torch.LongTensor(dst), 0.0,
                                  "nonzero error")
        elseif shape:size() == 2 then
          -- This test will allow through some false positives. It only checks
          -- that the elements flagged positive are indeed non-zero.
          for i=1,dst1:size()[1] do
            tester:assert(tensor[dst1[i][1]][dst1[i][2]] ~= 0)
          end
        elseif shape:size() == 3 then
          -- This test will allow through some false positives. It only checks
          -- that the elements flagged positive are indeed non-zero.
          for i=1,dst1:size()[1] do
            tester:assert(tensor[dst1[i][1]][dst1[i][2]][dst1[i][3]] ~= 0)
          end
        end
      end
   end

end

--function torch_main_tests.testheaptracking()
--  local oldheaptracking = torch._heaptracking
--  if oldheaptracking == nil then
--    oldheaptracking = false
--  end
--  torch.setheaptracking(true)
--  tester:assert(torch._heaptracking == true, 'Heap tracking expected true')
--
--  torch.setheaptracking(false)
--  tester:assert(torch._heaptracking == false, 'Heap tracking expected false')
--
--  -- put heap tracking to its original state
--  torch.setheaptracking(oldheaptracking)
--end

function torch_main_tests.bernoulli()
  local size = torch.LongStorage{10, 10}
  local t = torch.ByteTensor(size)

  local function isBinary(t)
    return torch.ne(t, 0):cmul(torch.ne(t, 1)):sum() == 0
  end

  local p = 0.5
  t:bernoulli(p)
  tester:assert(isBinary(t), 'Sample from torch.bernoulli is not binary')

  local p = torch.rand(size)
  t:bernoulli(p)
  tester:assert(isBinary(t), 'Sample from torch.bernoulli is not binary')
end

function torch_main_tests.logNormal()
    local t = torch.FloatTensor(10, 10)
    local mean, std = torch.uniform(), 0.1 * torch.uniform()
    local tolerance = 0.02

    t:logNormal(mean, std)
    local logt = t:log()
    tester:assertalmosteq(logt:mean(), mean, tolerance, 'mean is wrong')
    tester:assertalmosteq(logt:std(), std, tolerance, 'tolerance is wrong')
end

math.randomseed(os.time())

if torch.getdefaulttensortype() == 'torch.FloatTensor' then
   precision = 1e-4
elseif  torch.getdefaulttensortype() == 'torch.DoubleTensor' then
   precision = 1e-8
end

tester:add(torch_main_tests)



tester:run()

