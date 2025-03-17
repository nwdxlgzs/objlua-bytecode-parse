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

# 结束语
正如demo所示，这个语言对于尝试入手的人可能稍微有些奇怪，杂糅了很多语言风格（当然这个是我个人针对我用过的各种语言整合出的一个实现方式），希望你能真正从对陌生的事物逐渐熟悉最后喜欢。考虑到这个语法魔改问题以及添加了新指令，所以我直接提供一套工具可以对字节码解析操作，方便对objlua/lua感兴趣的人进行使用。
