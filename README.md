# objlua-bytecode-parse
objlua字节码的序列化与反序列化Lua(objlua)实现

# demo
```lua
local lib = require("lib")
local lundump = lib.lundump
local ldump = lib.ldump
local s = string.dump(function(...) print("HATE YOU!", 123) end);
load(s)()
local u = lundump(s);
local p = u.load();
print(p)
for k, v in pairs(p.k) do
    if v.get() == "HATE YOU!" then v.value_.string = "LOVE YOU!" end
end
for k, v in pairs(p.code) do
    if v.getOpName() == "LOADI" and v.getsBx() == 123 then
        v.setsBx(321);
    end
end
local d = ldump(p);
s = d.dump()
load(s)()
```
