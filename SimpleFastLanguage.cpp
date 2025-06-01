#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <memory>
#include <cctype>
#include <cstdint>

enum class TokenKind {
    Identifier, IntLit, FloatLit, StringLit, BoolLit,
    KwPublic, KwFunction, KwReturn, KwClass, KwAbstract,
    KwStatic, KwIs, SymLBrace, SymRBrace, SymLParen, SymRParen,
    SymEq, SymSemicolon, SymComma, SymAttrL, SymAttrR,
    EndOfFile
};
struct Token {
    TokenKind kind;
    std::string text;
    int line;
};

struct Lexer {
    std::string src; size_t pos = 0; int line = 1;
    Lexer(const std::string& s) : src(s) {}
    std::vector<Token> tokenize() {
        std::vector<Token> T;
        while (pos < src.size()) {
            char c = src[pos];
            if (isspace(c)) { if (c == '\n') line++; pos++; continue; }
            if (isalpha(c) || c == '_') {
                size_t st = pos;
                while (pos < src.size() && (isalnum(src[pos]) || src[pos] == '_')) pos++;
                std::string w = src.substr(st, pos - st);
                TokenKind k = TokenKind::Identifier;
                if (w == "public")   k = TokenKind::KwPublic;
                else if (w == "function") k = TokenKind::KwFunction;
                else if (w == "return")   k = TokenKind::KwReturn;
                else if (w == "class")    k = TokenKind::KwClass;
                else if (w == "abstract") k = TokenKind::KwAbstract;
                else if (w == "static")   k = TokenKind::KwStatic;
                else if (w == "is")       k = TokenKind::KwIs;
                else if (w == "true" || w == "false") k = TokenKind::BoolLit;
                T.push_back({ k,w,line });
                continue;
            }
            if (isdigit(c)) {
                size_t st = pos; bool fp = false;
                while (pos < src.size() && (isdigit(src[pos]) || src[pos] == '.')) {
                    if (src[pos] == '.') fp = true;
                    pos++;
                }
                std::string lit = src.substr(st, pos - st);
                T.push_back({ fp ? TokenKind::FloatLit : TokenKind::IntLit,lit,line });
                continue;
            }
            if (c == '"') {
                pos++; size_t st = pos;
                while (pos < src.size() && src[pos] != '"') pos++;
                std::string lit = src.substr(st, pos - st);
                pos++;
                T.push_back({ TokenKind::StringLit,lit,line });
                continue;
            }
            TokenKind k = TokenKind::EndOfFile;
            switch (c) {
            case '{': k = TokenKind::SymLBrace; break;
            case '}': k = TokenKind::SymRBrace; break;
            case '(': k = TokenKind::SymLParen; break;
            case ')': k = TokenKind::SymRParen; break;
            case '=': k = TokenKind::SymEq; break;
            case ';': k = TokenKind::SymSemicolon; break;
            case ',': k = TokenKind::SymComma; break;
            case '[': k = TokenKind::SymAttrL; break;
            case ']': k = TokenKind::SymAttrR; break;
            default: pos++; continue;
            }
            T.push_back({ k,std::string(1,c),line });
            pos++;
        }
        T.push_back({ TokenKind::EndOfFile,"",line });
        return T;
    }
};

enum class TypeKind { Int32, Float, Bool, String, Void, Class };
struct Type { TypeKind kind; std::string cls; };

struct ASTNode { virtual ~ASTNode() = default; };

struct Expr : ASTNode {};
struct Stmt : ASTNode {};

struct Literal : Expr {
    Type type; std::string val;
    Literal(Type t, const std::string& v) : type(t), val(v) {}
};

struct VarExpr : Expr {
    std::string name;
    VarExpr(const std::string& n) : name(n) {}
};

struct CallExpr : Expr {
    std::string name; std::vector<std::unique_ptr<Expr>> args;
};

struct VarDecl : Stmt {
    bool isPub, isStatic;
    Type type; std::string name;
    std::unique_ptr<Expr> init;
};

struct PrintStmt : Stmt {
    std::unique_ptr<Expr> expr;

    PrintStmt() = default;
    explicit PrintStmt(std::unique_ptr<Expr> e) : expr(std::move(e)) {}
};

struct ReturnStmt : Stmt {
    std::unique_ptr<Expr> expr;

    ReturnStmt() = default;
    explicit ReturnStmt(std::unique_ptr<Expr> e) : expr(std::move(e)) {}
};

struct FuncDecl : ASTNode {
    bool isPub;
    std::string name;
    Type retType;
    std::vector<std::unique_ptr<Stmt>> body;
};

struct ClassDecl : ASTNode {
    bool isPub, isAbstract;
    std::string name;
};

struct Parser {
    std::vector<Token> T; size_t idx = 0;
    Parser(std::vector<Token>& t) : T(t) {}
    Token& cur() { return T[idx]; }
    void next() { if (idx + 1 < T.size()) idx++; }
    bool accept(TokenKind k) { if (cur().kind == k) { next(); return true; } return false; }
    void expect(TokenKind k) {
        if (cur().kind != k) { std::cerr << "Parse error\n"; exit(1); }
        next();
    }

    std::vector<std::unique_ptr<ASTNode>> parse() {
        std::vector<std::unique_ptr<ASTNode>> nodes;
        while (cur().kind != TokenKind::EndOfFile) {
            if (accept(TokenKind::SymAttrL)) {
                while (!accept(TokenKind::SymAttrR)) next();
            }
            bool isPub = accept(TokenKind::KwPublic);
            bool isStatic = accept(TokenKind::KwStatic);
            if (cur().kind == TokenKind::KwClass) {
                next(); expect(TokenKind::Identifier); expect(TokenKind::SymLBrace);
                while (!accept(TokenKind::SymRBrace)) next();
                continue;
            }
            if (cur().kind == TokenKind::KwFunction) {
                next();
                std::string fname = cur().text; expect(TokenKind::Identifier);
                expect(TokenKind::SymLParen); expect(TokenKind::SymRParen);
                expect(TokenKind::KwReturn); expect(TokenKind::Identifier);  
                expect(TokenKind::KwIs);
                Type rt = parseType();
                expect(TokenKind::SymLBrace);
                auto f = new FuncDecl();
                f->isPub = isPub; f->name = fname; f->retType = rt;
                while (!accept(TokenKind::SymRBrace)) {
                    if (cur().kind == TokenKind::Identifier && cur().text == "print") {
                        next(); expect(TokenKind::SymLParen);
                        auto lit = parseExpr();
                        expect(TokenKind::SymRParen); expect(TokenKind::SymSemicolon);
                        auto printStmt = std::make_unique<PrintStmt>();
                        printStmt->expr = std::move(lit);
                        f->body.push_back(std::move(printStmt));
                    }
                    else if (accept(TokenKind::KwReturn)) {
                        auto e = parseExpr(); expect(TokenKind::SymSemicolon);
                        auto returnStmt = std::make_unique<ReturnStmt>();
                        returnStmt->expr = std::move(e);
                        f->body.push_back(std::move(returnStmt));
                    }
                    else next();
                }
                nodes.emplace_back(f);
                continue;
            }
            Type vt = parseType();
            std::string vname = cur().text; expect(TokenKind::Identifier);
            expect(TokenKind::SymEq);
            auto init = parseExpr(); expect(TokenKind::SymSemicolon);
            auto v = new VarDecl();
            v->isPub = isPub; v->isStatic = isStatic;
            v->type = vt; v->name = vname; v->init = std::move(init);
            nodes.emplace_back(v);
        }
        return nodes;
    }

    Type parseType() {
        if (cur().text == "int32") { next(); return { TypeKind::Int32,"" }; }
        if (cur().text == "fl32") { next(); return { TypeKind::Float,"" }; }
        if (cur().text == "bool") { next(); return { TypeKind::Bool,"" }; }
        if (cur().text == "string") { next(); return { TypeKind::String,"" }; }
        if (cur().kind == TokenKind::Identifier) {
            std::string cn = cur().text; next();
            return { TypeKind::Class,cn };
        }
        std::cerr << "Unknown type\n"; exit(1);
    }

    std::unique_ptr<Expr> parseExpr() {
        if (cur().kind == TokenKind::IntLit) {
            auto r = std::make_unique<Literal>(Literal({ TypeKind::Int32,"" }, cur().text));
            next(); return r;
        }
        if (cur().kind == TokenKind::FloatLit) {
            auto r = std::make_unique<Literal>(Literal({ TypeKind::Float,"" }, cur().text));
            next(); return r;
        }
        if (cur().kind == TokenKind::BoolLit) {
            auto r = std::make_unique<Literal>(Literal({ TypeKind::Bool,"" }, cur().text));
            next(); return r;
        }
        if (cur().kind == TokenKind::StringLit) {
            auto r = std::make_unique<Literal>(Literal({ TypeKind::String,"" }, cur().text));
            next(); return r;
        }
        if (cur().kind == TokenKind::Identifier) {
            std::string nm = cur().text; next();
            if (accept(TokenKind::SymLParen)) {
                auto c = new CallExpr(); c->name = nm;
                if (!accept(TokenKind::SymRParen)) {
                    do {
                        c->args.push_back(parseExpr());
                    } while (accept(TokenKind::SymComma));
                    expect(TokenKind::SymRParen);
                }
                return std::unique_ptr<Expr>(c);
            }
            return std::make_unique<VarExpr>(nm);
        }
        std::cerr << "Expr error\n"; exit(1);
    }
};

size_t curl_cb(void* b, size_t s, size_t n, void* u) {
    ((std::string*)u)->append((char*)b, s * n);
    return s * n;
}

std::string http_get(const std::string& url) {
    return "HTTP GET Response: " + url;
}

class Value {
public:
    enum class ValueType { Int32, Float, Bool, String };

private:
    ValueType type;
    union {
        int32_t intVal;
        float floatVal;
        bool boolVal;
    };
    std::string stringVal;       

public:
    Value() : type(ValueType::Int32), intVal(0) {}
    Value(int32_t v) : type(ValueType::Int32), intVal(v) {}
    Value(float v) : type(ValueType::Float), floatVal(v) {}
    Value(bool v) : type(ValueType::Bool), boolVal(v) {}
    Value(const std::string& v) : type(ValueType::String), stringVal(v) {}

    bool isInt() const { return type == ValueType::Int32; }
    bool isFloat() const { return type == ValueType::Float; }
    bool isBool() const { return type == ValueType::Bool; }
    bool isString() const { return type == ValueType::String; }

    int32_t getInt() const { return intVal; }
    float getFloat() const { return floatVal; }
    bool getBool() const { return boolVal; }
    const std::string& getString() const { return stringVal; }
};

std::unordered_map<std::string, Value> globals;

Value eval(Expr* e) {
    if (auto lit = dynamic_cast<Literal*>(e)) {
        switch (lit->type.kind) {
        case TypeKind::Int32:  return Value(std::stoi(lit->val));
        case TypeKind::Float:  return Value(std::stof(lit->val));
        case TypeKind::Bool:   return Value(lit->val == "true");
        case TypeKind::String: return Value(lit->val);
        default: return Value(0);
        }
    }
    if (auto ve = dynamic_cast<VarExpr*>(e)) {
        return globals[ve->name];
    }
    if (auto ce = dynamic_cast<CallExpr*>(e)) {
        if (ce->name == "read_file") {
            auto path = eval(ce->args[0].get()).getString();
            std::ifstream f(path.c_str());
            std::ostringstream ss; ss << f.rdbuf();
            return Value(ss.str());
        }
        if (ce->name == "write_file") {
            auto path = eval(ce->args[0].get()).getString();
            auto data = eval(ce->args[1].get()).getString();
            std::ofstream f(path.c_str());
            f << data;
            return Value("");
        }
        if (ce->name == "http_get") {
            return Value(http_get(eval(ce->args[0].get()).getString()));
        }
    }
    return Value(0);
}

int main(int argc, char** argv) {
    if (argc < 2) { std::cerr << "Usage: sfl <file>\n"; return 1; }
    std::ifstream in(argv[1]);
    std::string src((std::istreambuf_iterator<char>(in)), {});

    Lexer lex(src);
    auto toks = lex.tokenize();
    Parser p(toks);
    auto nodes = p.parse();
    FuncDecl* mainFn = nullptr;
    for (auto& n : nodes) {
        if (auto v = dynamic_cast<VarDecl*>(n.get())) {
            globals[v->name] = eval(v->init.get());
        }
        else if (auto f = dynamic_cast<FuncDecl*>(n.get())) {
            if (f->name == "main") mainFn = f;
        }
    }
    if (!mainFn) { std::cerr << "No main\n"; return 1; }
    Value ret;
    for (auto& s : mainFn->body) {
        if (auto ps = dynamic_cast<PrintStmt*>(s.get())) {
            Value v = eval(ps->expr.get());
            if (v.isInt())    std::cout << v.getInt() << "\n";
            if (v.isFloat())  std::cout << v.getFloat() << "\n";
            if (v.isBool())   std::cout << (v.getBool() ? "true" : "false") << "\n";
            if (v.isString()) std::cout << v.getString() << "\n";
        }
        if (auto rs = dynamic_cast<ReturnStmt*>(s.get())) {
            ret = eval(rs->expr.get());
            break;
        }
    }

    if (ret.isInt()) return ret.getInt();
    return 0;
}