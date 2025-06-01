#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <array>
#include <memory>
#include <cctype>
#include <cstdint>
#include <cassert>

class SymbolTable {
private:
    std::unordered_map<std::string, uint32_t> stringToId;
    std::vector<std::string> idToString;
    uint32_t nextId = 0;

public:
    uint32_t intern(const std::string& str) {
        auto it = stringToId.find(str);
        if (it != stringToId.end()) {
            return it->second;
        }
        uint32_t id = nextId++;
        stringToId[str] = id;
        idToString.push_back(str);
        return id;
    }

    const std::string& getString(uint32_t id) const {
        if (id >= idToString.size()) {
            static const std::string empty = "";
            return empty;
        }
        return idToString[id];
    }

    size_t size() const { return idToString.size(); }
};

static SymbolTable g_symbols;

static const uint32_t SYM_MAIN = g_symbols.intern("main");
static const uint32_t SYM_PRINT = g_symbols.intern("print");
static const uint32_t SYM_READ_FILE = g_symbols.intern("read_file");
static const uint32_t SYM_WRITE_FILE = g_symbols.intern("write_file");
static const uint32_t SYM_HTTP_GET = g_symbols.intern("http_get");

enum class ValueType : uint8_t {
    Int32 = 0,
    Float = 1,
    Bool = 2,
    String = 3,
    Void = 4
};

struct Value {
    ValueType type;
    union {
        int32_t i32;
        float f32;
        bool boolean;
        uint32_t stringId;
    } data;

    Value() : type(ValueType::Void) { data.i32 = 0; }
    Value(int32_t v) : type(ValueType::Int32) { data.i32 = v; }
    Value(float v) : type(ValueType::Float) { data.f32 = v; }
    Value(bool v) : type(ValueType::Bool) { data.boolean = v; }
    Value(uint32_t stringId) : type(ValueType::String) { data.stringId = stringId; }

    static Value fromString(const std::string& str) {
        return Value(g_symbols.intern(str));
    }

    static Value makeVoid() {
        Value v;
        v.type = ValueType::Void;
        return v;
    }

    std::string toString() const {
        switch (type) {
        case ValueType::Int32: return std::to_string(data.i32);
        case ValueType::Float: return std::to_string(data.f32);
        case ValueType::Bool: return data.boolean ? "true" : "false";
        case ValueType::String: return g_symbols.getString(data.stringId);
        case ValueType::Void: return "void";
        }
        return "";
    }

    bool isTrue() const {
        switch (type) {
        case ValueType::Bool: return data.boolean;
        case ValueType::Int32: return data.i32 != 0;
        case ValueType::Float: return data.f32 != 0.0f;
        case ValueType::String: return !g_symbols.getString(data.stringId).empty();
        case ValueType::Void: return false;
        }
        return false;
    }
};

enum class OpCode : uint8_t {
    LOAD_CONST = 0,        
    LOAD_GLOBAL = 1,       
    STORE_GLOBAL = 2,      
    MOVE = 3,              

    ADD = 10,
    SUB = 11,
    MUL = 12,
    DIV = 13,

    EQ = 20,
    NE = 21,
    LT = 22,
    LE = 23,
    GT = 24,
    GE = 25,

    JUMP = 30,             
    JUMP_IF = 31,            
    JUMP_IF_NOT = 32,        
    CALL = 33,            
    RETURN = 34,            

    PRINT = 40,            
    READ_FILE = 41,        
    WRITE_FILE = 42,      
    HTTP_GET = 43,         

    HALT = 255
};

struct Instruction {
    OpCode opcode;
    uint8_t dst;           
    uint8_t src1;          
    uint8_t src2;             
    uint32_t immediate;     

    Instruction(OpCode op = OpCode::HALT, uint8_t d = 0, uint8_t s1 = 0, uint8_t s2 = 0, uint32_t imm = 0)
        : opcode(op), dst(d), src1(s1), src2(s2), immediate(imm) {
    }
};

enum class ASTNodeType : uint8_t {
    LITERAL,
    VAR_EXPR,
    CALL_EXPR,
    VAR_DECL,
    PRINT_STMT,
    RETURN_STMT,
    FUNC_DECL,
    CLASS_DECL
};

struct ASTNode {
    ASTNodeType type;

    union {
        struct {
            ValueType valueType;
            union {
                int32_t intVal;
                float floatVal;
                bool boolVal;
                uint32_t stringId;
            } value;
        } literal;

        struct {
            uint32_t nameId;
        } varExpr;

        struct {
            uint32_t nameId;
            std::vector<std::unique_ptr<ASTNode>>* args;
        } callExpr;

        struct {
            bool isPub, isStatic;
            ValueType varType;
            uint32_t nameId;
            std::unique_ptr<ASTNode>* init;
        } varDecl;

        struct {
            std::unique_ptr<ASTNode>* expr;
        } printStmt;

        struct {
            std::unique_ptr<ASTNode>* expr;
        } returnStmt;

        struct {
            uint32_t nameId;
            bool isPub;
            ValueType returnType;
            std::vector<std::unique_ptr<ASTNode>>* body;
        } funcDecl;

        struct {
            uint32_t nameId;
            bool isPub, isAbstract;
        } classDecl;
    } data;

    std::vector<std::unique_ptr<ASTNode>> children;
    std::unique_ptr<ASTNode> child;

    explicit ASTNode(ASTNodeType t) : type(t) {
        switch (t) {
        case ASTNodeType::CALL_EXPR:
            data.callExpr.args = &children;
            break;
        case ASTNodeType::VAR_DECL:
            data.varDecl.init = &child;
            break;
        case ASTNodeType::PRINT_STMT:
            data.printStmt.expr = &child;
            break;
        case ASTNodeType::RETURN_STMT:
            data.returnStmt.expr = &child;
            break;
        case ASTNodeType::FUNC_DECL:
            data.funcDecl.body = &children;
            break;
        default:
            break;
        }
    }

    ~ASTNode() = default;
};

class RegisterVM {
private:
    static constexpr size_t NUM_REGISTERS = 256;
    static constexpr size_t GLOBALS_SIZE = 1024;
    static constexpr size_t CALL_STACK_SIZE = 256;

    std::array<Value, NUM_REGISTERS> registers;
    std::array<Value, GLOBALS_SIZE> globals;

    struct CallFrame {
        size_t returnPC;
        uint8_t returnReg;
    };
    std::array<CallFrame, CALL_STACK_SIZE> callStack;
    size_t callStackTop = 0;

    std::vector<Value> constants;
    std::vector<Instruction> code;
    std::unordered_map<uint32_t, size_t> functionAddresses;
    size_t pc = 0;

public:
    void setRegister(uint8_t reg, const Value& val) {
        registers[reg] = val;
    }

    Value getRegister(uint8_t reg) const {
        return registers[reg];
    }

    void setGlobal(uint32_t id, const Value& val) {
        if (id >= GLOBALS_SIZE) {
            throw std::runtime_error("Global variable index out of range");
        }
        globals[id] = val;
    }

    Value getGlobal(uint32_t id) const {
        if (id >= GLOBALS_SIZE) {
            throw std::runtime_error("Global variable index out of range");
        }
        return globals[id];
    }

    uint32_t addConstant(const Value& val) {
        constants.push_back(val);
        return constants.size() - 1;
    }

    void emit(const Instruction& instr) {
        code.push_back(instr);
    }

    void setFunctionAddress(uint32_t nameId, size_t address) {
        functionAddresses[nameId] = address;
    }

    size_t getCurrentAddress() const {
        return code.size();
    }

    Value execute() {
        pc = 0;
        callStackTop = 0;

        auto mainIt = functionAddresses.find(SYM_MAIN);
        if (mainIt != functionAddresses.end()) {
            pc = mainIt->second;
        }

        while (pc < code.size()) {
            const Instruction& instr = code[pc];

            switch (instr.opcode) {
            case OpCode::LOAD_CONST: {
                registers[instr.dst] = constants[instr.immediate];
                break;
            }

            case OpCode::LOAD_GLOBAL: {
                registers[instr.dst] = getGlobal(instr.immediate);
                break;
            }

            case OpCode::STORE_GLOBAL: {
                setGlobal(instr.immediate, registers[instr.src1]);
                break;
            }

            case OpCode::MOVE: {
                registers[instr.dst] = registers[instr.src1];
                break;
            }

            case OpCode::ADD: {
                Value a = registers[instr.src1];
                Value b = registers[instr.src2];
                if (a.type == ValueType::Int32 && b.type == ValueType::Int32) {
                    registers[instr.dst] = Value(a.data.i32 + b.data.i32);
                }
                else {
                    float af = (a.type == ValueType::Float) ? a.data.f32 :
                        (a.type == ValueType::Int32) ? a.data.i32 : 0.0f;
                    float bf = (b.type == ValueType::Float) ? b.data.f32 :
                        (b.type == ValueType::Int32) ? b.data.i32 : 0.0f;
                    registers[instr.dst] = Value(af + bf);
                }
                break;
            }

            case OpCode::SUB: {
                Value a = registers[instr.src1];
                Value b = registers[instr.src2];
                if (a.type == ValueType::Int32 && b.type == ValueType::Int32) {
                    registers[instr.dst] = Value(a.data.i32 - b.data.i32);
                }
                else {
                    float af = (a.type == ValueType::Float) ? a.data.f32 :
                        (a.type == ValueType::Int32) ? a.data.i32 : 0.0f;
                    float bf = (b.type == ValueType::Float) ? b.data.f32 :
                        (b.type == ValueType::Int32) ? b.data.i32 : 0.0f;
                    registers[instr.dst] = Value(af - bf);
                }
                break;
            }

            case OpCode::MUL: {
                Value a = registers[instr.src1];
                Value b = registers[instr.src2];
                if (a.type == ValueType::Int32 && b.type == ValueType::Int32) {
                    registers[instr.dst] = Value(a.data.i32 * b.data.i32);
                }
                else {
                    float af = (a.type == ValueType::Float) ? a.data.f32 :
                        (a.type == ValueType::Int32) ? a.data.i32 : 0.0f;
                    float bf = (b.type == ValueType::Float) ? b.data.f32 :
                        (b.type == ValueType::Int32) ? b.data.i32 : 0.0f;
                    registers[instr.dst] = Value(af * bf);
                }
                break;
            }

            case OpCode::DIV: {
                Value a = registers[instr.src1];
                Value b = registers[instr.src2];
                float af = (a.type == ValueType::Float) ? a.data.f32 :
                    (a.type == ValueType::Int32) ? a.data.i32 : 0.0f;
                float bf = (b.type == ValueType::Float) ? b.data.f32 :
                    (b.type == ValueType::Int32) ? b.data.i32 : 1.0f;
                if (bf != 0.0f) {
                    registers[instr.dst] = Value(af / bf);
                }
                else {
                    registers[instr.dst] = Value(0.0f);
                }
                break;
            }

            case OpCode::EQ: {
                Value a = registers[instr.src1];
                Value b = registers[instr.src2];
                bool result = false;
                if (a.type == b.type) {
                    switch (a.type) {
                    case ValueType::Int32: result = a.data.i32 == b.data.i32; break;
                    case ValueType::Float: result = a.data.f32 == b.data.f32; break;
                    case ValueType::Bool: result = a.data.boolean == b.data.boolean; break;
                    case ValueType::String: result = a.data.stringId == b.data.stringId; break;
                    default: break;
                    }
                }
                registers[instr.dst] = Value(result);
                break;
            }

            case OpCode::LT: {
                Value a = registers[instr.src1];
                Value b = registers[instr.src2];
                bool result = false;
                if (a.type == ValueType::Int32 && b.type == ValueType::Int32) {
                    result = a.data.i32 < b.data.i32;
                }
                else if (a.type == ValueType::Float || b.type == ValueType::Float) {
                    float af = (a.type == ValueType::Float) ? a.data.f32 : a.data.i32;
                    float bf = (b.type == ValueType::Float) ? b.data.f32 : b.data.i32;
                    result = af < bf;
                }
                registers[instr.dst] = Value(result);
                break;
            }

            case OpCode::JUMP: {
                pc = instr.immediate;
                continue;
            }

            case OpCode::JUMP_IF: {
                if (registers[instr.src1].isTrue()) {
                    pc = instr.immediate;
                    continue;
                }
                break;
            }

            case OpCode::JUMP_IF_NOT: {
                if (!registers[instr.src1].isTrue()) {
                    pc = instr.immediate;
                    continue;
                }
                break;
            }

            case OpCode::CALL: {
                auto it = functionAddresses.find(instr.immediate);
                if (it != functionAddresses.end()) {
                    if (callStackTop >= CALL_STACK_SIZE) {
                        throw std::runtime_error("Call stack overflow");
                    }
                    callStack[callStackTop++] = { pc + 1, instr.dst };
                    pc = it->second;
                    continue;
                }
                break;
            }

            case OpCode::RETURN: {
                if (callStackTop > 0) {
                    CallFrame frame = callStack[--callStackTop];
                    registers[frame.returnReg] = registers[instr.src1];
                    pc = frame.returnPC;
                    continue;
                }
                else {
                    return registers[instr.src1];
                }
            }

            case OpCode::PRINT: {
                Value val = registers[instr.src1];
                std::cout << val.toString() << std::endl;
                break;
            }

            case OpCode::READ_FILE: {
                Value pathVal = registers[instr.src1];
                if (pathVal.type == ValueType::String) {
                    std::string path = g_symbols.getString(pathVal.data.stringId);
                    std::ifstream file(path);
                    if (file.is_open()) {
                        std::ostringstream ss;
                        ss << file.rdbuf();
                        registers[instr.dst] = Value::fromString(ss.str());
                    }
                    else {
                        registers[instr.dst] = Value::fromString("");
                    }
                }
                break;
            }

            case OpCode::WRITE_FILE: {
                Value pathVal = registers[instr.src1];
                Value dataVal = registers[instr.src2];
                if (pathVal.type == ValueType::String && dataVal.type == ValueType::String) {
                    std::string path = g_symbols.getString(pathVal.data.stringId);
                    std::string data = g_symbols.getString(dataVal.data.stringId);
                    std::ofstream file(path);
                    if (file.is_open()) {
                        file << data;
                    }
                }
                break;
            }

            case OpCode::HTTP_GET: {
                Value urlVal = registers[instr.src1];
                if (urlVal.type == ValueType::String) {
                    std::string url = g_symbols.getString(urlVal.data.stringId);
                    std::string response = "HTTP GET Response: " + url;
                    registers[instr.dst] = Value::fromString(response);
                }
                break;
            }

            case OpCode::HALT: {
                return registers[0];
            }

            default:
                throw std::runtime_error("Unknown opcode: " + std::to_string(static_cast<int>(instr.opcode)));
            }

            pc++;
        }

        return Value(0);
    }

    std::string generateWASM() const {
        std::ostringstream wasm;

        wasm << "(module\n";
        wasm << "  (memory 1)\n";
        wasm << "  (export \"memory\" (memory 0))\n";

        wasm << "  (func $main (export \"main\") (result i32)\n";
        wasm << "    (local $reg0 i32) (local $reg1 i32) (local $reg2 i32) (local $reg3 i32)\n";
        wasm << "    (local $reg4 f32) (local $reg5 f32)\n";

        for (size_t i = 0; i < code.size(); ++i) {
            const Instruction& instr = code[i];

            switch (instr.opcode) {
            case OpCode::LOAD_CONST:
                if (instr.immediate < constants.size()) {
                    const Value& val = constants[instr.immediate];
                    if (val.type == ValueType::Int32) {
                        wasm << "    i32.const " << val.data.i32 << "\n";
                        wasm << "    local.set $reg" << static_cast<int>(instr.dst) << "\n";
                    }
                    else if (val.type == ValueType::Float) {
                        wasm << "    f32.const " << val.data.f32 << "\n";
                        wasm << "    local.set $reg" << (static_cast<int>(instr.dst) + 4) << "\n";
                    }
                }
                break;

            case OpCode::ADD:
                wasm << "    local.get $reg" << static_cast<int>(instr.src1) << "\n";
                wasm << "    local.get $reg" << static_cast<int>(instr.src2) << "\n";
                wasm << "    i32.add\n";
                wasm << "    local.set $reg" << static_cast<int>(instr.dst) << "\n";
                break;

            case OpCode::RETURN:
                wasm << "    local.get $reg" << static_cast<int>(instr.src1) << "\n";
                wasm << "    return\n";
                break;

            case OpCode::HALT:
                wasm << "    i32.const 0\n";
                wasm << "    return\n";
                break;

            default:
                wasm << "    ;; Opcode " << static_cast<int>(instr.opcode) << " not implemented\n";
                break;
            }
        }

        wasm << "    i32.const 0\n";
        wasm << "  )\n";
        wasm << ")\n";

        return wasm.str();
    }
};

enum class TokenKind {
    Identifier, IntLit, FloatLit, StringLit, BoolLit,
    KwPublic, KwFunction, KwReturn, KwClass, KwAbstract,
    KwStatic, KwIs, KwIf, KwElse, KwWhile, KwFor,
    SymLBrace, SymRBrace, SymLParen, SymRParen,
    SymEq, SymSemicolon, SymComma, SymAttrL, SymAttrR,
    SymPlus, SymMinus, SymStar, SymSlash,
    SymEqEq, SymNe, SymLt, SymLe, SymGt, SymGe,
    EndOfFile
};

struct Token {
    TokenKind kind;
    std::string text;
    int line;
};

struct Lexer {
    std::string src;
    size_t pos = 0;
    int line = 1;

    explicit Lexer(const std::string& s) : src(s) {}

    std::vector<Token> tokenize() {
        std::vector<Token> tokens;

        while (pos < src.size()) {
            char c = src[pos];

            if (isspace(c)) {
                if (c == '\n') line++;
                pos++;
                continue;
            }

            if (isalpha(c) || c == '_') {
                size_t start = pos;
                while (pos < src.size() && (isalnum(src[pos]) || src[pos] == '_')) {
                    pos++;
                }
                std::string word = src.substr(start, pos - start);

                TokenKind kind = TokenKind::Identifier;
                if (word == "public") kind = TokenKind::KwPublic;
                else if (word == "function") kind = TokenKind::KwFunction;
                else if (word == "return") kind = TokenKind::KwReturn;
                else if (word == "class") kind = TokenKind::KwClass;
                else if (word == "abstract") kind = TokenKind::KwAbstract;
                else if (word == "static") kind = TokenKind::KwStatic;
                else if (word == "is") kind = TokenKind::KwIs;
                else if (word == "if") kind = TokenKind::KwIf;
                else if (word == "else") kind = TokenKind::KwElse;
                else if (word == "while") kind = TokenKind::KwWhile;
                else if (word == "for") kind = TokenKind::KwFor;
                else if (word == "true" || word == "false") kind = TokenKind::BoolLit;

                tokens.push_back({ kind, word, line });
                continue;
            }

            if (isdigit(c)) {
                size_t start = pos;
                bool hasDecimal = false;

                while (pos < src.size() && (isdigit(src[pos]) || src[pos] == '.')) {
                    if (src[pos] == '.') {
                        if (hasDecimal) break;
                        hasDecimal = true;
                    }
                    pos++;
                }

                std::string literal = src.substr(start, pos - start);
                TokenKind kind = hasDecimal ? TokenKind::FloatLit : TokenKind::IntLit;
                tokens.push_back({ kind, literal, line });
                continue;
            }

            if (c == '"') {
                pos++;
                size_t start = pos;

                while (pos < src.size() && src[pos] != '"') {
                    if (src[pos] == '\\' && pos + 1 < src.size()) {
                        pos += 2;
                    }
                    else {
                        pos++;
                    }
                }

                std::string literal = src.substr(start, pos - start);
                if (pos < src.size()) pos++;    

                tokens.push_back({ TokenKind::StringLit, literal, line });
                continue;
            }

            if (pos + 1 < src.size()) {
                char next = src[pos + 1];
                TokenKind kind = TokenKind::EndOfFile;

                if (c == '=' && next == '=') kind = TokenKind::SymEqEq;
                else if (c == '!' && next == '=') kind = TokenKind::SymNe;
                else if (c == '<' && next == '=') kind = TokenKind::SymLe;
                else if (c == '>' && next == '=') kind = TokenKind::SymGe;

                if (kind != TokenKind::EndOfFile) {
                    tokens.push_back({ kind, src.substr(pos, 2), line });
                    pos += 2;
                    continue;
                }
            }

            TokenKind kind = TokenKind::EndOfFile;
            switch (c) {
            case '{': kind = TokenKind::SymLBrace; break;
            case '}': kind = TokenKind::SymRBrace; break;
            case '(': kind = TokenKind::SymLParen; break;
            case ')': kind = TokenKind::SymRParen; break;
            case '=': kind = TokenKind::SymEq; break;
            case ';': kind = TokenKind::SymSemicolon; break;
            case ',': kind = TokenKind::SymComma; break;
            case '[': kind = TokenKind::SymAttrL; break;
            case ']': kind = TokenKind::SymAttrR; break;
            case '+': kind = TokenKind::SymPlus; break;
            case '-': kind = TokenKind::SymMinus; break;
            case '*': kind = TokenKind::SymStar; break;
            case '/': kind = TokenKind::SymSlash; break;
            case '<': kind = TokenKind::SymLt; break;
            case '>': kind = TokenKind::SymGt; break;
            default:
                pos++;
                continue;
            }

            tokens.push_back({ kind, std::string(1, c), line });
            pos++;
        }

        tokens.push_back({ TokenKind::EndOfFile, "", line });
        return tokens;
    }
};

class Parser {
private:
    std::vector<Token> tokens;
    size_t idx = 0;

public:
    explicit Parser(std::vector<Token>& t) : tokens(std::move(t)) {}

    Token& current() {
        return tokens[idx];
    }

    void advance() {
        if (idx + 1 < tokens.size()) {
            idx++;
        }
    }

    bool accept(TokenKind kind) {
        if (current().kind == kind) {
            advance();
            return true;
        }
        return false;
    }

    void expect(TokenKind kind) {
        if (current().kind != kind) {
            std::cerr << "Parse error at line " << current().line
                << ": expected token " << static_cast<int>(kind)
                << ", got " << static_cast<int>(current().kind) << std::endl;
            exit(1);
        }
        advance();
    }

    ValueType parseType() {
        if (current().text == "int32") {
            advance();
            return ValueType::Int32;
        }
        if (current().text == "fl32") {
            advance();
            return ValueType::Float;
        }
        if (current().text == "bool") {
            advance();
            return ValueType::Bool;
        }
        if (current().text == "string") {
            advance();
            return ValueType::String;
        }
        if (current().text == "void") {
            advance();
            return ValueType::Void;
        }

        std::cerr << "Unknown type: " << current().text << std::endl;
        exit(1);
    }

    std::unique_ptr<ASTNode> parseExpression() {
        return parseComparison();
    }

    std::unique_ptr<ASTNode> parseComparison() {
        auto left = parseArithmetic();

        while (current().kind == TokenKind::SymEqEq || current().kind == TokenKind::SymNe ||
            current().kind == TokenKind::SymLt || current().kind == TokenKind::SymLe ||
            current().kind == TokenKind::SymGt || current().kind == TokenKind::SymGe) {

            TokenKind op = current().kind;
            advance();
            auto right = parseArithmetic();

            left = std::move(right);  
        }

        return left;
    }

    std::unique_ptr<ASTNode> parseArithmetic() {
        auto left = parseTerm();

        while (current().kind == TokenKind::SymPlus || current().kind == TokenKind::SymMinus) {
            TokenKind op = current().kind;
            advance();
            auto right = parseTerm();

            left = std::move(right);  
        }

        return left;
    }

    std::unique_ptr<ASTNode> parseTerm() {
        auto left = parsePrimary();

        while (current().kind == TokenKind::SymStar || current().kind == TokenKind::SymSlash) {
            TokenKind op = current().kind;
            advance();
            auto right = parsePrimary();

            left = std::move(right);  
        }

        return left;
    }

    std::unique_ptr<ASTNode> parsePrimary() {
        switch (current().kind) {
        case TokenKind::IntLit: {
            auto node = std::make_unique<ASTNode>(ASTNodeType::LITERAL);
            node->data.literal.valueType = ValueType::Int32;
            node->data.literal.value.intVal = std::stoi(current().text);
            advance();
            return node;
        }

        case TokenKind::FloatLit: {
            auto node = std::make_unique<ASTNode>(ASTNodeType::LITERAL);
            node->data.literal.valueType = ValueType::Float;
            node->data.literal.value.floatVal = std::stof(current().text);
            advance();
            return node;
        }

        case TokenKind::BoolLit: {
            auto node = std::make_unique<ASTNode>(ASTNodeType::LITERAL);
            node->data.literal.valueType = ValueType::Bool;
            node->data.literal.value.boolVal = (current().text == "true");
            advance();
            return node;
        }

        case TokenKind::StringLit: {
            auto node = std::make_unique<ASTNode>(ASTNodeType::LITERAL);
            node->data.literal.valueType = ValueType::String;
            node->data.literal.value.stringId = g_symbols.intern(current().text);
            advance();
            return node;
        }

        case TokenKind::Identifier: {
            std::string name = current().text;
            uint32_t nameId = g_symbols.intern(name);
            advance();

            if (accept(TokenKind::SymLParen)) {
                auto node = std::make_unique<ASTNode>(ASTNodeType::CALL_EXPR);
                node->data.callExpr.nameId = nameId;

                if (!accept(TokenKind::SymRParen)) {
                    do {
                        node->children.push_back(parseExpression());
                    } while (accept(TokenKind::SymComma));
                    expect(TokenKind::SymRParen);
                }

                return node;
            }
            else {
                auto node = std::make_unique<ASTNode>(ASTNodeType::VAR_EXPR);
                node->data.varExpr.nameId = nameId;
                return node;
            }
        }

        case TokenKind::SymLParen: {
            advance();
            auto expr = parseExpression();
            expect(TokenKind::SymRParen);
            return expr;
        }

        default:
            std::cerr << "Unexpected token in expression: " << current().text << std::endl;
            exit(1);
        }
    }

    std::unique_ptr<ASTNode> parseStatement() {
        if (current().kind == TokenKind::Identifier && current().text == "print") {
            advance();
            expect(TokenKind::SymLParen);

            auto node = std::make_unique<ASTNode>(ASTNodeType::PRINT_STMT);
            node->child = parseExpression();

            expect(TokenKind::SymRParen);
            expect(TokenKind::SymSemicolon);

            return node;
        }

        if (accept(TokenKind::KwReturn)) {
            auto node = std::make_unique<ASTNode>(ASTNodeType::RETURN_STMT);
            node->child = parseExpression();
            expect(TokenKind::SymSemicolon);
            return node;
        }

        ValueType type = parseType();
        std::string name = current().text;
        expect(TokenKind::Identifier);
        expect(TokenKind::SymEq);

        auto node = std::make_unique<ASTNode>(ASTNodeType::VAR_DECL);
        node->data.varDecl.varType = type;
        node->data.varDecl.nameId = g_symbols.intern(name);
        node->data.varDecl.isPub = false;
        node->data.varDecl.isStatic = false;
        node->child = parseExpression();

        expect(TokenKind::SymSemicolon);
        return node;
    }

    std::vector<std::unique_ptr<ASTNode>> parse() {
        std::vector<std::unique_ptr<ASTNode>> nodes;

        while (current().kind != TokenKind::EndOfFile) {
            if (accept(TokenKind::SymAttrL)) {
                while (!accept(TokenKind::SymAttrR)) {
                    advance();
                }
            }

            bool isPub = accept(TokenKind::KwPublic);
            bool isStatic = accept(TokenKind::KwStatic);
            bool isAbstract = accept(TokenKind::KwAbstract);

            if (current().kind == TokenKind::KwClass) {
                advance();
                std::string className = current().text;
                expect(TokenKind::Identifier);
                expect(TokenKind::SymLBrace);

                auto node = std::make_unique<ASTNode>(ASTNodeType::CLASS_DECL);
                node->data.classDecl.nameId = g_symbols.intern(className);
                node->data.classDecl.isPub = isPub;
                node->data.classDecl.isAbstract = isAbstract;

                while (!accept(TokenKind::SymRBrace)) {
                    advance();      
                }

                nodes.push_back(std::move(node));
                continue;
            }

            if (current().kind == TokenKind::KwFunction) {
                advance();
                std::string funcName = current().text;
                expect(TokenKind::Identifier);
                expect(TokenKind::SymLParen);
                expect(TokenKind::SymRParen);
                expect(TokenKind::KwReturn);

                ValueType returnType = parseType();
                expect(TokenKind::KwIs);
                expect(TokenKind::SymLBrace);

                auto node = std::make_unique<ASTNode>(ASTNodeType::FUNC_DECL);
                node->data.funcDecl.nameId = g_symbols.intern(funcName);
                node->data.funcDecl.isPub = isPub;
                node->data.funcDecl.returnType = returnType;

                while (!accept(TokenKind::SymRBrace)) {
                    node->children.push_back(parseStatement());
                }

                nodes.push_back(std::move(node));
                continue;
            }

            nodes.push_back(parseStatement());
        }

        return nodes;
    }
};

class BytecodeCompiler {
private:
    RegisterVM& vm;
    std::unordered_map<uint32_t, uint32_t> globalVarMap;
    uint32_t nextGlobalSlot = 0;
    uint8_t nextRegister = 0;

    uint8_t allocRegister() {
        if (nextRegister >= 256) {
            throw std::runtime_error("Register allocation overflow");
        }
        return nextRegister++;
    }

    void freeRegister() {
        if (nextRegister > 0) {
            nextRegister--;
        }
    }

public:
    explicit BytecodeCompiler(RegisterVM& vm) : vm(vm) {}

    uint8_t compileExpression(const ASTNode& node) {
        switch (node.type) {
        case ASTNodeType::LITERAL: {
            Value val;
            switch (node.data.literal.valueType) {
            case ValueType::Int32:
                val = Value(node.data.literal.value.intVal);
                break;
            case ValueType::Float:
                val = Value(node.data.literal.value.floatVal);
                break;
            case ValueType::Bool:
                val = Value(node.data.literal.value.boolVal);
                break;
            case ValueType::String:
                val = Value(node.data.literal.value.stringId);
                break;
            default:
                val = Value::makeVoid();
                break;
            }

            uint32_t constId = vm.addConstant(val);
            uint8_t reg = allocRegister();
            vm.emit(Instruction(OpCode::LOAD_CONST, reg, 0, 0, constId));
            return reg;
        }

        case ASTNodeType::VAR_EXPR: {
            auto it = globalVarMap.find(node.data.varExpr.nameId);
            if (it != globalVarMap.end()) {
                uint8_t reg = allocRegister();
                vm.emit(Instruction(OpCode::LOAD_GLOBAL, reg, 0, 0, it->second));
                return reg;
            }
            throw std::runtime_error("Undefined variable");
        }

        case ASTNodeType::CALL_EXPR: {
            uint32_t nameId = node.data.callExpr.nameId;

            if (nameId == SYM_READ_FILE && node.children.size() >= 1) {
                uint8_t pathReg = compileExpression(*node.children[0]);
                uint8_t resultReg = allocRegister();
                vm.emit(Instruction(OpCode::READ_FILE, resultReg, pathReg));
                freeRegister();   
                return resultReg;
            }

            if (nameId == SYM_WRITE_FILE && node.children.size() >= 2) {
                uint8_t pathReg = compileExpression(*node.children[0]);
                uint8_t dataReg = compileExpression(*node.children[1]);
                vm.emit(Instruction(OpCode::WRITE_FILE, 0, pathReg, dataReg));
                freeRegister();   
                freeRegister();   
                return allocRegister();    
            }

            if (nameId == SYM_HTTP_GET && node.children.size() >= 1) {
                uint8_t urlReg = compileExpression(*node.children[0]);
                uint8_t resultReg = allocRegister();
                vm.emit(Instruction(OpCode::HTTP_GET, resultReg, urlReg));
                freeRegister();   
                return resultReg;
            }

            uint8_t resultReg = allocRegister();
            vm.emit(Instruction(OpCode::CALL, resultReg, 0, 0, nameId));
            return resultReg;
        }

        default:
            throw std::runtime_error("Cannot compile expression of this type");
        }
    }

    void compileStatement(const ASTNode& node) {
        switch (node.type) {
        case ASTNodeType::VAR_DECL: {
            uint8_t valueReg = compileExpression(*node.child);
            uint32_t globalSlot = nextGlobalSlot++;
            globalVarMap[node.data.varDecl.nameId] = globalSlot;
            vm.emit(Instruction(OpCode::STORE_GLOBAL, 0, valueReg, 0, globalSlot));
            freeRegister();   
            break;
        }

        case ASTNodeType::PRINT_STMT: {
            uint8_t exprReg = compileExpression(*node.child);
            vm.emit(Instruction(OpCode::PRINT, 0, exprReg));
            freeRegister();   
            break;
        }

        case ASTNodeType::RETURN_STMT: {
            uint8_t exprReg = compileExpression(*node.child);
            vm.emit(Instruction(OpCode::RETURN, 0, exprReg));
            freeRegister();   
            break;
        }

        default:
            break;
        }
    }

    void compile(const std::vector<std::unique_ptr<ASTNode>>& nodes) {
        for (const auto& node : nodes) {
            if (node->type == ASTNodeType::FUNC_DECL) {
                vm.setFunctionAddress(node->data.funcDecl.nameId, vm.getCurrentAddress());

                for (const auto& stmt : node->children) {
                    compileStatement(*stmt);
                }

                vm.emit(Instruction(OpCode::RETURN, 0, 0));
            }
        }

        for (const auto& node : nodes) {
            if (node->type == ASTNodeType::VAR_DECL) {
                compileStatement(*node);
            }
        }

        vm.emit(Instruction(OpCode::HALT));
    }
};

int main(int argc, char** argv) {
    if (argc < 2) {
        std::cerr << "Usage: sfl <file> [--wasm]\n";
        return 1;
    }

    bool generateWasm = false;
    if (argc >= 3 && std::string(argv[2]) == "--wasm") {
        generateWasm = true;
    }

    try {
        std::ifstream file(argv[1]);
        if (!file.is_open()) {
            std::cerr << "Cannot open file: " << argv[1] << std::endl;
            return 1;
        }

        std::string source((std::istreambuf_iterator<char>(file)), {});

        Lexer lexer(source);
        auto tokens = lexer.tokenize();

        Parser parser(tokens);
        auto ast = parser.parse();

        RegisterVM vm;
        BytecodeCompiler compiler(vm);

        compiler.compile(ast);

        if (generateWasm) {
            std::string wasmCode = vm.generateWASM();
            std::cout << "Generated WASM:\n" << wasmCode << std::endl;

            std::string wasmFile = std::string(argv[1]) + ".wasm";
            std::ofstream wasmOut(wasmFile);
            wasmOut << wasmCode;
            std::cout << "WASM written to: " << wasmFile << std::endl;
        }
        else {
            Value result = vm.execute();

            if (result.type == ValueType::Int32) {
                return result.data.i32;
            }
        }

    }
    catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}