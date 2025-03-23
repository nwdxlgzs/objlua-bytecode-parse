--[[
objlua示例：
    解析Lua54字节码(load/dump)的库实现
]]
local LUA_TNIL,LUA_TBOOLEAN,LUA_TLIGHTUSERDATA,LUA_TNUMBER,LUA_TSTRING,LUA_TTABLE,LUA_TFUNCTION,LUA_TUSERDATA,LUA_TTHREAD = 0,1,2,3,4,5,6,7,8
local LUAI_MAXSHORTLEN = 40
local function makevariant(t,v) return t | (v << 4); end
local function ttypetag(o) return o.tt_ & 0x3F;end
local LUA_VSHRSTR,LUA_VLNGSTR = makevariant(LUA_TSTRING, 0),makevariant(LUA_TSTRING, 1)
local LUA_VNUMINT,LUA_VNUMFLT = makevariant(LUA_TNUMBER, 0),makevariant(LUA_TNUMBER, 1)
local LUA_VNIL = makevariant(LUA_TNIL, 0)
local LUA_VFALSE,LUA_VTRUE = makevariant(LUA_TBOOLEAN, 0),makevariant(LUA_TBOOLEAN, 1)
local opmodes = {
    "iABC","iAsBx","iAsBx","iABx","iABx","iABC","iABC","iABC","iABC","iABC",
    "iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC",
    "iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC",
    "iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC",
    "iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC",
    "iABC","iABC","iABC","iABC","iABC","iABC","isJ","iABC","iABC","iABC",
    "iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC","iABC",
    "iABC","iABC","iABC","iABx","iABx","iABx","iABC","iABx","iABC","iABx",
    "iABC","iABC","iAx","iABC","iABC","iABC","iABC","iABC","iABC","iABC",
    "iABC","iABC",
};
local opnames = {
    "MOVE","LOADI","LOADF","LOADK","LOADKX",
    "LOADFALSE","LFALSESKIP","LOADTRUE","LOADNIL","GETUPVAL",
    "SETUPVAL","GETTABUP","GETTABLE","GETI","GETFIELD",
    "SETTABUP","SETTABLE","SETI","SETFIELD","NEWTABLE",
    "SELF","ADDI","ADDK","SUBK","MULK",
    "MODK","POWK","DIVK","IDIVK","BANDK",
    "BORK","BXORK","SHRI","SHLI","ADD",
    "SUB","MUL","MOD","POW","DIV",
    "IDIV","BAND","BOR","BXOR","SHL",
    "SHR","MMBIN","MMBINI","MMBINK","UNM",
    "BNOT","NOT","LEN","CONCAT","CLOSE",
    "TBC","JMP","EQ","LT","LE",
    "EQK","EQI","LTI","LEI","GTI",
    "GEI","TEST","TESTSET","CALL","TAILCALL",
    "RETURN","RETURN0","RETURN1","FORLOOP","FORPREP",
    "TFORPREP","TFORCALL","TFORLOOP","SETLIST","CLOSURE",
    "VARARG","VARARGPREP","EXTRAARG","DEFCLASS","DEFFIELD",
    "METHODINIT","DEFMETHODARGTYPE","DEFMETHOD","CKMCONST","CKCABSTRACT",
    "TYPEOF","INSTANCEOF",
}
local iABC, iABx, iAsBx, iAx, isJ;
local class Instruction{
    instruction=0;
    public Instruction(instruction:number) {
        self.instruction = instruction;
    }
    public Instruction(instruction:<Instruction>) {
        self.instruction = instruction.instruction;
    }
    public static const parse(instruction:number) {
        local op = (instruction >> Instruction.POS_OP) & Instruction.MASK1(Instruction.SIZE_OP, 0);
        local opmode = opmodes[op + 1];
        if opmode=="iABC" then
            return iABC(instruction);
        elseif opmode=="iABx" then
            return iABx(instruction);
        elseif opmode=="iAsBx" then
            return iAsBx(instruction);
        elseif opmode=="iAx" then
            return iAx(instruction);
        elseif opmode=="isJ" then
            return isJ(instruction);
        end
        return Instruction(instruction); 
    }
    getOpName(){
        local opname = opnames[self.getOpCode() + 1];
        if opname==nil then return "UNKNOWN"; end
        return opname;
    }
    @meta __tostring(){
        return string.format("<%s\t%d>", self.getOpName(), self.instruction);
    }
    public static const SIZE_C = 8;
    public static const SIZE_B = 8;
    public static const SIZE_Bx = (Instruction.SIZE_C + Instruction.SIZE_B + 1);
    public static const SIZE_A = 8;
    public static const SIZE_Ax = (Instruction.SIZE_Bx + 8);
    public static const SIZE_sJ = (Instruction.SIZE_Bx + 8);
    public static const SIZE_OP = 7;
    public static const POS_OP = 0;
    public static const POS_A = (Instruction.POS_OP + 7);
    public static const POS_k = (Instruction.POS_A + 8);
    public static const POS_B = (Instruction.POS_k + 1);
    public static const POS_C = (Instruction.POS_B + 8);
    public static const POS_Bx = (Instruction.POS_A + 8);
    public static const POS_Ax = (Instruction.POS_A);
    public static const POS_sJ = (Instruction.POS_A);
    public static const MAXARG_Bx = ((1 << Instruction.SIZE_Bx) - 1);
    public static const OFFSET_sBx = (Instruction.MAXARG_Bx>>1);
    public static const MAXARG_Ax = ((1 << Instruction.SIZE_Ax) - 1);
    public static const MAXARG_sJ = ((1 << Instruction.SIZE_sJ) - 1);
    public static const OFFSET_sJ = (Instruction.MAXARG_sJ >> 1);
    public static const MAXARG_A = ((1<<Instruction.SIZE_A)-1);
    public static const MAXARG_B = ((1<<Instruction.SIZE_B)-1);
    public static const MAXARG_C = ((1<<Instruction.SIZE_C)-1);
    public static const OFFSET_sC = (Instruction.MAXARG_C >> 1);
    public static const int2sC(i:number){
        return i + Instruction.OFFSET_sC;
    }
    public static const sC2int(i:number){
        return i - Instruction.OFFSET_sC;
    }
    public static const MASK1(n:number, p:number){
        return ((~((~0)<<n))<<p)
    }
    public static const MASK0(n:number, p:number){
        return ~Instruction.MASK1(n, p)
    }
    getOpCode(){
        return (self.instruction >> self.POS_OP) & Instruction.MASK1(Instruction.SIZE_OP, 0);
    }
    setOpCode(o:number){
        self.instruction = (self.instruction & Instruction.MASK0(Instruction.SIZE_OP, Instruction.POS_OP)) |
        ((o << Instruction.POS_OP) & Instruction.MASK1(Instruction.SIZE_OP, Instruction.POS_OP));
    }
    getarg(pos:number, size:number){
        return (self.instruction >> pos) & Instruction.MASK1(size, 0);
    }
    setarg(v:number, pos:number, size:number){
        self.instruction = (self.instruction & Instruction.MASK0(size, pos)) | ((v << pos) & Instruction.MASK1(size, pos));
    }
}
class iABC:Instruction{
    @meta static __tostring(){
        return string.format("<iABC|%s\tA=%d\tB=%d\tC=%d\tK=%d>", self.getOpName(), self.getA(), self.getB(), self.getC(), self.getK());
    }
    public getA(){
        return self.getarg(Instruction.POS_A, Instruction.SIZE_A);
    }
    public setA(v:number){
        self.setarg(v, Instruction.POS_A, Instruction.SIZE_A);
    }
    public getB(){
        return self.getarg(Instruction.POS_B, Instruction.SIZE_B);
    }
    public getsB(){
        return Instruction.sC2int(self.getB());
    }
    public setB(v:number){
        self.setarg(v, Instruction.POS_B, Instruction.SIZE_B);
    }
    public getC(){
        return self.getarg(Instruction.POS_C, Instruction.SIZE_C);
    }
    public getsC(){
        return Instruction.sC2int(self.getC());
    }
    public setC(v:number){
        self.setarg(v, Instruction.POS_C, Instruction.SIZE_C);
    }
    public getK(){
        return self.getarg(Instruction.POS_k, 1);
    }
    public setK(v:number){
        self.setarg(v, Instruction.POS_k, 1);
    }
}
class iABx:Instruction{
    @meta static __tostring(){
        return string.format("<iABx|%s\tA=%d\tBx=%d>", self.getOpName(), self.getA(), self.getBx());
    }
    public getA(){
        return self.getarg(Instruction.POS_A, Instruction.SIZE_A);
    }
    public setA(v:number){
        self.setarg(v, Instruction.POS_A, Instruction.SIZE_A);
    }
    public getBx(){
        return self.getarg(Instruction.POS_Bx, Instruction.SIZE_Bx);
    }
    public setBx(v:number){
        self.setarg(v, Instruction.POS_Bx, Instruction.SIZE_Bx);
    }
}
class iAsBx:Instruction{
    @meta static __tostring(){
        return string.format("<iAsBx|%s\tA=%d\tsBx=%d>", self.getOpName(), self.getA(), self.getsBx());
    }
    public getA(){
        return self.getarg(Instruction.POS_A, Instruction.SIZE_A);
    }
    public setA(v:number){
        self.setarg(v, Instruction.POS_A, Instruction.SIZE_A);
    }
    public setBx(v:number){
        self.setarg(v, Instruction.POS_Bx, Instruction.SIZE_Bx);
    }
    public getsBx(){
        return self.getarg(Instruction.POS_Bx, Instruction.SIZE_Bx) - Instruction.OFFSET_sBx;
    }
    public setsBx(v:number){
        self.setBx(v + Instruction.OFFSET_sBx);
    }
}
class iAx:Instruction{
    @meta static __tostring(){
        return string.format("<iAx|%s\tAx=%d>", self.getOpName(), self.getAx());
    }
    public getAx(){
        return self.getarg(Instruction.POS_Ax, Instruction.SIZE_Ax);
    }
    public setAx(v:number){
        self.setarg(v, Instruction.POS_Ax, Instruction.SIZE_Ax);
    }
}
class isJ:Instruction{
    @meta static __tostring(){
        return string.format("<isJ|%s\tsJ=%d>", self.getOpName(), self.getsJ());
    }
    public getsJ(){
        return self.getarg(Instruction.POS_sJ, Instruction.SIZE_sJ) - Instruction.OFFSET_sJ;
    }
    public setsJ(v:number){
        self.setarg(v + Instruction.OFFSET_sJ, Instruction.POS_sJ, Instruction.SIZE_sJ);
    }
}
local class Value{
    string;--字符串
    i;--整型
    n;--浮点
    b;--布尔
    --nil就是nil
}
local class TValue {
    value_;
    tt_;
    @meta static __tostring(){
        return string.format("<TValue\tt_=%d\tvalue=%s>", self.tt_, self.get());
    }
    public static const tsvalue(o:<TValue>){
        return o.value_.string;
    }
    public static const ivalue(o:<TValue>){
        return o.value_.i;
    }
    public static const fltvalue(o:<TValue>){
        return o.value_.n;
    }
    public get(){
        local tag = ttypetag(self);
        if tag == LUA_VNIL then
            return nil;
        elseif tag == LUA_VFALSE then
            return false;
        elseif tag == LUA_VTRUE then
            return true;
        elseif tag == LUA_VNUMINT then
            return TValue.ivalue(self);
        elseif tag == LUA_VNUMFLT then
            return TValue.fltvalue(self);
        elseif tag == LUA_VSHRSTR or tag == LUA_VLNGSTR then
            return TValue.tsvalue(self);
        else
            return nil;
        end
    }
    public TValue(value_, tt_) {
        self.value_ = value_;
        self.tt_ = tt_;
    }
    public TValue(value:string){
        local len = string.len(value);
        local value_ = Value();
        if len < LUAI_MAXSHORTLEN then
            value_.string = value;
            self.tt_ = LUA_VSHRSTR;
        else
            value_.string = value;
            self.tt_ = LUA_VLNGSTR;
        end
        self.value_ = value_;
    }
    public TValue(value:number){
        local value_ = Value();
        if math.floor(value) == value then
            value_.i = value;
            self.tt_ = LUA_VNUMINT;
        else
            value_.n = value;
            self.tt_ = LUA_VNUMFLT;
        end
        self.value_ = value_;
    }
    public TValue(value:boolean){
        local value_ = Value();
        if value then
            value_.b = 1;
            self.tt_ = LUA_VTRUE;
        else
            value_.b = 0;
            self.tt_ = LUA_VFALSE;
        end
        self.value_ = value_;
    }
    public TValue(value:nil){
        self.tt_ = LUA_VNIL;
        self.value_ =  Value();
    }
}
local class Upvaldesc{
    name;
    instack;
    idx=0;
    kind=0;
    public Upvaldesc(name, instack:boolean, idx:number, kind:number) {
        self.name = name;
        self.instack = instack;
        self.idx = idx;
        self.kind = kind;
    }
    @meta static __tostring(){
        local name = self.name
        if (name == nil) then name = "<none>"; end
        return string.format("<Upvaldesc\tname=%s\tinstack=%s\tidx=%d\tkind=%d>", name, self.instack, self.idx, self.kind);
    }
}
local class LocVar{
    varname;
    startpc;
    endpc;
    public LocVar(varname, startpc:number, endpc:number) {
        self.varname = varname;
        self.startpc = startpc;
        self.endpc = endpc;
    }
    @meta static __tostring(){
        local varname = self.varname
        if (varname == nil) then varname = "<none>"; end
        return string.format("<LocVar\tvarname=%s\tstartpc=%d\tendpc=%d>", varname, self.startpc, self.endpc);
    }
}
local class AbsLineInfo {
    pc=-1;
    line=-1;
    public AbsLineInfo(pc, line) {
        self.pc = pc;
        self.line = line;
    }
    @meta static __tostring(){
        return string.format("<AbsLineInfo\tpc=%d\tline=%d>", self.pc, self.line);
    }
};
local class Proto{
    public Proto() {
        self.k = {};
        self.code = {};
        self.p = {};
        self.upvalues = {};
        self.lineinfo = {};
        self.abslineinfo = {};
        self.locvars = {};
    }
    @meta static __tostring(){
        local buffer={}
        table.insert(buffer,string.format("<Proto\n\tnumparams=%d\n\tis_vararg=%s\n\tmaxstacksize=%d\n\tsizeupvalues=%d\n\t"..
            "sizek=%d\n\tsizecode=%d\n\tsizelineinfo=%d\n\tsizep=%d\n\tsizelocvars=%d\n\tsizeabslineinfo=%d\n\t"..
            "linedefined=%d\n\tlastlinedefined=%d\n\tnupvalues=%d\n\tk={\n",
            self.numparams, self.is_vararg, self.maxstacksize, self.sizeupvalues,
            self.sizek, self.sizecode, self.sizelineinfo, self.sizep, self.sizelocvars, self.sizeabslineinfo,
            self.linedefined, self.lastlinedefined, self.nupvalues))
        for i=0,self.sizek-1 do
            table.insert(buffer,string.format("\t\t[%d]=%s,\n",i,self.k[i]))
        end
        table.insert(buffer,"\t},\n\tcode={\n")
        for i=0,self.sizecode-1 do
            table.insert(buffer,string.format("\t\t[%d]=%s,\n",i,self.code[i]))
        end
        table.insert(buffer,"\t},\n\tupvalues={\n")
        for i=0,self.sizeupvalues-1 do
            table.insert(buffer,string.format("\t\t[%d]=%s,\n",i,self.upvalues[i]))
        end
        table.insert(buffer,"\t},\n\tlineinfo={\n")
        for i=0,self.sizelineinfo-1 do
            table.insert(buffer,string.format("\t\t[%d]=%s,\n",i,self.lineinfo[i]))
        end
        table.insert(buffer,"\t},\n\tabslineinfo={\n")
        for i=0,self.sizeabslineinfo-1 do
            table.insert(buffer,string.format("\t\t[%d]=%s,\n",i,self.abslineinfo[i]))
        end
        table.insert(buffer,"\t},\n\tlocvars={\n")
        for i=0,self.sizelocvars-1 do
            table.insert(buffer,string.format("\t\t[%d]=%s,\n",i,self.locvars[i]))
        end
        table.insert(buffer,"\t},\n\tsource=")
        if self.source then
            table.insert(buffer,self.source)
        else
            table.insert(buffer,"<null>")
        end
        table.insert(buffer,"\n>")
        return table.concat(buffer)
    }
    numparams = 0;
    is_vararg;
    maxstacksize = 0;
    sizeupvalues = 0;
    sizek = 0;
    sizecode = 0;
    sizelineinfo = 0;
    sizep = 0;
    sizelocvars = 0;
    sizeabslineinfo = 0;
    linedefined = 0;
    lastlinedefined = 0;
    k;
    code;
    p;
    upvalues;
    lineinfo;
    abslineinfo;
    locvars;
    source;
    nupvalues=0;
}

local class lundump{
    public static const LUA_SIGNATURE = "\x1bLua";
    public static const LUAC_DATA = "\x19\x93\r\n\x1a\n";
    public static const LUAC_VERSION = 0x54;
    public static const LUAC_FORMAT = 0;
    public static const sizeof_Instruction = 4;
    public static const sizeof_lua_Integer = 8;
    public static const sizeof_lua_Number = 8;
    public static const MAX_SIZET = math.maxinteger;--需要括号……emmm
    public static const INT_MAX = 0x7fffffff;
    public static const LUAC_INT = 0x5678;
    public static const LUAC_NUM = 370.5;
    const bytes;
    pos;
    const len;
    public lundump(bytes:string) {
        self.bytes = bytes;
        self.pos = 0;
        self.len = string.len(bytes);
    }
    public load(){
        print("parse start, total bytes "..self.len)
        self.pos = 0;
        self.checkHeader()
        local proto = Proto()
        proto.nupvalues = self.loadByte()
        self.loadFunction(proto, nil)
        print("parse end, read "..self.pos.." bytes")
        return proto;
    }
    private loadFunction(f:<Proto>,psource:any) {--psource可能nill
        f.source = self.loadStringN();
        if f.source == nil then
            f.source = psource;
        end
        f.linedefined = self.loadInt(S);
        f.lastlinedefined = self.loadInt(S);
        f.numparams = self.loadByte(S);
        f.is_vararg = self.loadByte(S)~=0;
        f.maxstacksize = self.loadByte(S);
        self.loadCode(f);
        self.loadConstants(f);
        self.loadUpvalues(f);
        self.loadProtos(f);
        self.loadDebug(f);
    }
    private loadProtos(f:<Proto>){
        local n = self.loadInt()
        f.sizep = n
        for i = 1, n do
            local p = Proto()
            self.loadFunction(p, f.source)
            f.p[i-1] = p--这里按0开始索引
        end
    }
    private loadDebug(f:<Proto>){
        local i;
        local n = self.loadInt()
        f.sizelineinfo = n
        for i = 1, n do
            f.lineinfo[i-1] = self.loadByte()--这里按0开始索引
        end
        n = self.loadInt()
        f.sizeabslineinfo = n
        for i = 1, n do
            local pc = self.loadInt()
            local line = self.loadInt()
            f.abslineinfo[i-1] = AbsLineInfo(pc, line)--这里按0开始索引
        end
        n = self.loadInt()
        f.sizelocvars = n
        for i = 1, n do
            local varname = self.loadStringN()
            local startpc = self.loadInt();
            local endpc = self.loadInt();
            f.locvars[i-1] = LocVar(varname, startpc, endpc)--这里按0开始索引
        end
        n = self.loadInt()
        if n ~= 0 then
            n = f.sizeupvalues
        end
        for i = 1, n do
            f.upvalues[i-1].name = self.loadStringN()--这里按0开始索引
        end
    }
    private loadUpvalues(f:<Proto>){
        local n = self.loadInt()
        f.sizeupvalues = n
        for i = 1, n do
            local name = nil
            local instack = self.loadByte() ~= 0
            local idx = self.loadByte()
            local kind = self.loadByte()
            f.upvalues[i-1] = Upvaldesc(name, instack, idx, kind)--这里按0开始索引
        end
    }

    private loadConstants(f:<Proto>){
        local n = self.loadInt()
        f.sizek = n
        for i = 1, n do
            local t = self.loadByte(S);
            local o;
            if (t == LUA_VNIL) then
                o = TValue(nil);
            elseif (t == LUA_VFALSE) then
                o = TValue(false);
            elseif (t == LUA_VTRUE) then
                o = TValue(true);
            elseif (t == LUA_VNUMFLT) then
                o = TValue(self.loadNumber());
            elseif (t == LUA_VNUMINT) then
                o = TValue(self.loadInteger());
            elseif (t == LUA_VSHRSTR or t == LUA_VLNGSTR) then
                o = TValue(self.loadString());
            else
                o = TValue(nil);
            end
            f.k[i-1] = o;--这里按0开始索引
        end
    }
    private loadString() {
        local st = self.loadStringN();
        if (st == nil) then
          error(S, "bad format for constant string");
        end
        return st;
    }
      
    private loadCode(f:<Proto>){
        local n = self.loadInt();
        f.sizecode = n;
        local fmt = ""
        if lundump.LUAC_FORMAT == 0 then fmt = fmt.. "<" 
        else fmt = fmt.. ">" end
        fmt = fmt.. "i" .. math.floor(lundump.sizeof_Instruction)
        for i = 1, n do
            local str = self.loadVector(lundump.sizeof_Instruction);
            local inst=string.unpack(fmt,str)
            f.code[i-1] = Instruction.parse(inst)--这里按0开始索引
        end
    }
    private loadStringN() {
        local size = self.loadSize();
        if (size == 0) then return nil end
        size = size - 1
        local str = self.loadVector(size)
        return str
    }
    private loadSize(){
        return self.loadUnsigned(lundump.MAX_SIZET)
    }
    private loadInt() {
        return self.loadUnsigned(lundump.INT_MAX)
    }
      
    private loadUnsigned(limit:number) {--lua层面肯定是sign的了没办法，但是大部份应该都能正常
        local x = 0;
        local b;
        limit = limit >> 7;
        repeat
            b = self.loadByte(S);
            if (x >= limit) then
                error("integer overflow");
            end
            x = (x << 7) | (b & 0x7f);
        until ((b & 0x80)~=0)
        return x;
      }
      
    private checkHeader(){
        self.checkliteral(lundump.LUA_SIGNATURE, "not a binary chunk")
        if (self.loadByte() ~= lundump.LUAC_VERSION) then
            error("version mismatch");
        end
        if (self.loadByte() ~= lundump.LUAC_FORMAT) then
            error("format mismatch");
        end
        self.checkliteral(lundump.LUAC_DATA, "corrupted chunk");
        self.checksize(lundump.sizeof_Instruction, "Instruction");
        self.checksize(lundump.sizeof_lua_Integer, "lua_Integer");
        self.checksize(lundump.sizeof_lua_Number, "lua_Number");
        if (self.loadInteger(S) ~= lundump.LUAC_INT) then
            error("integer format mismatch");
        end
        if (self.loadNumber(S) ~= lundump.LUAC_NUM) then
            error("float format mismatch");
        end
    }
    private loadInteger() {
        local str = self.loadVector(lundump.sizeof_lua_Integer);
        local fmt = ""
        if lundump.LUAC_FORMAT == 0 then fmt = fmt.. "<" 
        else fmt = fmt.. ">" end
        fmt = fmt.. "i" .. math.floor(lundump.sizeof_lua_Integer)
        local num = string.unpack(fmt,str)
        return num;
    }
    private loadNumber() {
        local str = self.loadVector(lundump.sizeof_lua_Number);
        local fmt = ""
        if lundump.LUAC_FORMAT == 0 then fmt = fmt.. "<" 
        else fmt = fmt.. ">" end
        fmt = fmt.. "d"
        local num = string.unpack(fmt,str)
        return num;
    }
    private checksize(size:number,tname:string) {
        local gotsize = self.loadByte();
        if (gotsize ~= size) then
            error(string.format("%s size mismatch", tname));
        end
    }
    private loadVector(size:number){
        local adjust_size = size;
        if (adjust_size < 1) then adjust_size = 1; end
        if (self.pos + adjust_size > self.len) then
            adjust_size = self.len - self.pos;
        end
        bytestr = string.sub(self.bytes, self.pos + 1, self.pos + adjust_size);
        self.pos = self.pos + adjust_size;
        return bytestr;
    }
    private loadByte(){
        local byesval = self.loadVector(1);
        return byesval:byte(1,1);
    }
    private checkliteral(checkstr:string,errmsg:string) {
        local gotliteral = self.loadVector(string.len(checkstr))
        if(gotliteral~= checkstr) then
            error(msg);
        end
    }
}
local CHAR_BIT = 8
local sizeof_size_t = 8
local class ldump{
    public static const DIBS = ((sizeof_size_t * CHAR_BIT + 6) / 7);
    private buffer;
    private f;
    private strip;
    public ldump(f:<Proto>,strip:boolean){
        self.f = f;
        self.strip = strip;
    }
    public ldump(f:<Proto>){
        self.ldump(f,false);
    }
    public dump(){
        print("dumping bytecode...")
        self.buffer = {};
        self.dumpHeader();
        self.dumpByte(self.f.sizeupvalues);
        self.dumpFunction(self.f, nil);
        local bytecode = table.concat(self.buffer);
        print("dumping bytecode... done! size = "..string.len(bytecode).." with ".. #self.buffer .. " elements.")
        return bytecode
    }
    private dumpSize(x:number) {
        local buff = {}
        for i=0,ldump.DIBS-1 do
            buff[i] = 0
        end
        local n = 0;
        repeat
            n = n + 1;
            buff[ldump.DIBS - n] = x & 0x7f;
            x = x >> 7;
        until (x == 0)
        buff[ldump.DIBS - 1] = buff[ldump.DIBS - 1] | 0x80;
        local buffs = ""
        for i=0,ldump.DIBS-1 do
            buffs = buffs..string.char(buff[i])
        end
        buff = buffs:sub(ldump.DIBS - n + 1,ldump.DIBS)
        self.dumpVector(buff, n);
    }
    private dumpVector(s:string,n:number) {
        s = s:sub(1,n)
        table.insert(self.buffer,s);
    }
    private dumpString(s:nil) {
        self.dumpSize(0);
    }
    private dumpString(s:string) {
        local size = string.len(s)
        local str = s.."\0"
        self.dumpSize(size + 1);
        self.dumpVector(str, size);
    }
    private dumpFunction(f:<Proto>,psource:any) {--psource可能nil或者字符串
        if (self.strip or f.source == psource) then
            self.dumpString(nil);
        else
            self.dumpString(f.source);
        end
        self.dumpInt(f.linedefined);
        self.dumpInt(f.lastlinedefined);
        self.dumpByte(f.numparams);
        self.dumpByte(f.is_vararg);
        self.dumpByte(f.maxstacksize);
        self.dumpCode(f);
        self.dumpConstants(f);
        self.dumpUpvalues(f);
        self.dumpProtos(f);
        self.dumpDebug(f);
    }

    private dumpDebug(f:<Proto>) {
        local n = f.sizelineinfo;
        if self.strip then n = 0; end
        self.dumpInt(n);
        for i = 0, n - 1 do
            self.dumpByte(f.lineinfo[i]);
        end
        n = f.sizeabslineinfo;
        if self.strip then n = 0; end
        self.dumpInt(n);
        for i = 0, n - 1 do
            self.dumpInt(f.abslineinfo[i].pc);
            self.dumpInt(f.abslineinfo[i].line);
        end
        n = f.sizelocvars;
        if self.strip then n = 0; end
        self.dumpInt(n);
        for i = 0, n - 1 do
            self.dumpString(f.locvars[i].varname);
            self.dumpInt(f.locvars[i].startpc);
            self.dumpInt(f.locvars[i].endpc);
        end
        n = f.sizeupvalues;
        if self.strip then n = 0; end
        self.dumpInt(n);
        for i = 0, n - 1 do
            self.dumpString(f.upvalues[i].name);
        end
    }
    private dumpProtos(f:<Proto>) {
        local n = f.sizep;
        self.dumpInt(n);
        for i = 0, n - 1 do
            self.dumpFunction(f.p[i], f.source);
        end
    }
    private dumpUpvalues(f:<Proto>) {
        local n = f.sizeupvalues;
        self.dumpInt(n);
        for i = 0, n - 1 do
            self.dumpByte(f.upvalues[i].instack);
            self.dumpByte(f.upvalues[i].idx);
            self.dumpByte(f.upvalues[i].kind);
        end
    }
    private dumpConstants(f:<Proto>) {
        local n = f.sizek;
        self.dumpInt(n);
        for i = 0, n - 1 do
            local o = f.k[i];
            local tt = ttypetag(o);
            self.dumpByte(tt);
            if tt == LUA_VNUMFLT then
                self.dumpNumber(o.get());
            elseif tt == LUA_VNUMINT then
                self.dumpInteger(o.get());
            elseif tt == LUA_VSHRSTR or tt == LUA_VLNGSTR then
                self.dumpString(o.get());
            end
        end
    }
    
    private dumpCode(f:<Proto>) {
        self.dumpInt(f.sizecode);
        local fmt = ""
        if lundump.LUAC_FORMAT == 0 then fmt = fmt.. "<" 
        else fmt = fmt.. ">" end
        fmt = fmt.. "i" .. math.floor(lundump.sizeof_Instruction)
        for i = 0, f.sizecode - 1 do
            local inst = f.code[i];
            table.insert(self.buffer,string.pack(fmt,inst.instruction));
        end
    }
    private dumpInt(x:number) {
        self.dumpSize(x);
    }
    private dumpHeader() {
        self.dumpLiteral(lundump.LUA_SIGNATURE);
        self.dumpByte(lundump.LUAC_VERSION);
        self.dumpByte(lundump.LUAC_FORMAT);
        self.dumpLiteral(lundump.LUAC_DATA);
        self.dumpByte(lundump.sizeof_Instruction);
        self.dumpByte(lundump.sizeof_lua_Integer);
        self.dumpByte(lundump.sizeof_lua_Number);
        self.dumpInteger(lundump.LUAC_INT);
        self.dumpNumber(lundump.LUAC_NUM);
    }
    private dumpLiteral(str:string) {
        table.insert(self.buffer,str);
    }
    private dumpByte(b:boolean) {
        if b then b = 1 else b = 0 end
        self.dumpByte(b);
    }
    private dumpByte(b:number) {
        b = b & 0xff;
        table.insert(self.buffer,string.char(b));
    }
    private dumpInteger(b:number) {
        local fmt = ""
        if lundump.LUAC_FORMAT == 0 then fmt = fmt.. "<" 
        else fmt = fmt.. ">" end
        fmt = fmt.. "i" .. math.floor(lundump.sizeof_lua_Integer)
        table.insert(self.buffer,string.pack(fmt,b));
    }
    private dumpNumber(b:number) {
        local fmt = ""
        if lundump.LUAC_FORMAT == 0 then fmt = fmt.. "<" 
        else fmt = fmt.. ">" end
        fmt = fmt.. "d"
        table.insert(self.buffer,string.pack(fmt,b));
    }
}


return {
    ldump = ldump,
    lundump = lundump,
}
