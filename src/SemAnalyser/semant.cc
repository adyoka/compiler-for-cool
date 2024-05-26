

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <unordered_set>

extern int semant_debug;
extern char *curr_filename;

SymbolTable<Symbol, Symbol> *symbol_table;
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    this->install_classes(classes);
    this->build_inhertiance_graph();

    if (this->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
    
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);
    
    this->classMap[Object] = Object_class;
    this->classMap[IO] = IO_class;
    this->classMap[Int] = Int_class;
    this->classMap[Bool] = Bool_class;
    this->classMap[Str] = Str_class;
}

bool ClassTable::is_basic_class(Symbol symbol) {
    return (
        symbol == Object ||
        symbol == IO     ||
        symbol == Int    ||
        symbol == Bool   ||
        symbol == Str
    );
}

// Class_ ClassTable::get_class(Symbol symbol) {
//     if (this->classMap.find(symbol) == this->classMap.end())
//         return No_class;

//     return classMap[symbol];
// }

Symbol ClassTable::get_parent_class(Symbol symbol) {
    if (this->parentClass.find(symbol) == this->parentClass.end())
        return No_type;

    return parentClass[symbol];
}


void ClassTable::install_classes(Classes classes)   {
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        curr_class = classes->nth(i);
        Symbol class_name = curr_class->get_name();
        Symbol parent_name = curr_class->get_parent_name();
        if (is_basic_class(class_name) || class_name == SELF_TYPE)
        {
            semant_error(curr_class) << "Redefinition of " << class_name << " is not allowed. \n";
        }
        else if (this->classMap.find(class_name) != this->classMap.end()) {
            semant_error(curr_class) << "Class " << curr_class->get_name() << " was previously defined.\n";
        }
        
        else {
            this->classMap[class_name] = curr_class;
        }
    }
}

void ClassTable::build_inhertiance_graph() {
    for (auto const& mapEntry : this->classMap) {
        Symbol class_name = mapEntry.first;

        curr_class = mapEntry.second;
        Symbol class_parent_name = curr_class->get_parent_name();

        parentClass[class_name] = class_parent_name;

        if (
            class_parent_name == Int ||
            class_parent_name == Bool ||
            class_parent_name == Str    ||
            class_parent_name == SELF_TYPE
        ) {
            semant_error(curr_class) << "Class " << curr_class->get_name() << " cannot inherit basic class " << class_parent_name << ".\n";
        }

        if (this->classMap.find(class_parent_name) == this->classMap.end())
        {
            semant_error(curr_class) << "Class "<< class_name<< " inherits from an undefined class "<< class_parent_name<< ".\n";
        }

        if (this->inheritanceMap.find(class_parent_name) == this->inheritanceMap.end()) 
            this->inheritanceMap[class_parent_name] = std::vector<Symbol>();
    
        this->inheritanceMap[class_parent_name].push_back(class_name);

    }   
}

enum Color { gray, black, white };
std::unordered_map<Symbol, Color>colorMap;

bool ClassTable::traverse_graph(Symbol symbol) {
    colorMap[symbol] = gray;
    for (auto const& child_class : inheritanceMap[symbol]) {
        if (colorMap[child_class] == gray) {
            semant_error(classMap[symbol])<<"There is a circular dependency between classes: "<<symbol<<" and "<<child_class<<".\n";
            return false;
        }
        if (!traverse_graph(child_class)) 
            return false;
    }
    colorMap[symbol] = black;
    return true;
}

bool ClassTable::is_acyclic() {
    for (auto const& mapEntry : this->classMap) {
        colorMap[mapEntry.first] = white;
    }

    for (auto const& mapEntry : this->classMap) {
        Symbol class_name = mapEntry.first;
        if (colorMap[class_name] == white) {
            if (!this->traverse_graph(class_name)) {
                return false;
            }
        }
    }
    return true;
}


bool ClassTable::check_main() {
    if (classMap.find(Main) == classMap.end()) {
        semant_error()<<"Class Main is not defined.\n";
        return false;
    }

    Class_ main_class = classMap[Main];
    Features features = main_class->get_features();

    for (int i = features->first(); features->more(i); i = features->next(i)) {
        if (features->nth(i)->is_method() && static_cast<method_class*>(features->nth(i))->get_name() == main_meth   ){
            return true;
        }
    }

    semant_error(main_class) << "No 'main' method in class Main.\n";
    return false;
}


void ClassTable::type_check(Classes classes) {
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        curr_class = classes->nth(i);
        // symbol_table = new SymbolTable<Symbol, Symbol>();
        symbol_table->enterscope();
        symbol_table->addid(self, new Symbol(curr_class->get_name()));

        add_attributes_scope(curr_class);

        for (auto const& methodsEntry : class_methods[curr_class->get_name()]) {
            check_method(curr_class, methodsEntry.second, methodsEntry.second);

            methodsEntry.second->type_check(this);
        }

        for (auto const& attrEntry : class_attrs[curr_class->get_name()]) {
            if (parentClass.find(curr_class->get_name()) != parentClass.end()) {
                Symbol parent_class_name = parentClass[curr_class->get_name()];
                Class_ parent_class = classMap[parent_class_name];

                check_attr(parent_class, attrEntry.second);

                attrEntry.second->type_check(this);
            }
        }

        symbol_table->exitscope();
    }
}



////////////////////////////////////////////////////////////////////
//
//                        Features utilities
//
////////////////////////////////////////////////////////////////////



method_class* ClassTable::get_class_method(Symbol class_name, Symbol method_name) {
    if (class_methods[class_name].find(method_name) == class_methods[class_name].end())
        return nullptr;

    return class_methods[class_name][method_name];
}


void ClassTable::add_attributes_scope(Class_ curr_class) {
    symbol_table->enterscope();
    std::unordered_map<Symbol, attr_class*> attributes = this->class_attrs[curr_class->get_name()];
    for (const auto& attr : attributes) {
        symbol_table->addid(
            attr.first,
            new Symbol(attr.second->get_type())
        );
    }

    if (curr_class->get_name() == Object) return;

    if (parentClass.find(curr_class->get_name()) != parentClass.end()) {
        Symbol parent_class_name = parentClass[curr_class->get_name()];
        add_attributes_scope(classMap[parent_class_name]);
    }
}

void ClassTable::install_features(Classes classes) {
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        curr_class = classes->nth(i);
        Features features = curr_class->get_features();
        std::unordered_map<Symbol, method_class*> methodsMap;
        std::unordered_map<Symbol, attr_class*> attrsMap;

        for (int i = features->first(); features->more(i); i = features->next(i)) {
            if (features->nth(i)->is_method()) {
                method_class* method = static_cast<method_class*>(features->nth(i));
                Symbol m_name = method->get_name();
                if (methodsMap.find(m_name) != methodsMap.end()) {
                    semant_error(curr_class) 
                    << "Method " << m_name << " has already been defined in the class" 
                    << curr_class->get_name() << ".\n";
                }
                else {
                    methodsMap[m_name] = method;
                }
            }
            //if (features->nth(i)->is_attr()) {
            else {
                attr_class* attr = static_cast<attr_class*>(features->nth(i));
                Symbol a_name = attr->get_name();

                if (attrsMap.find(a_name) != attrsMap.end()) {
                    semant_error(curr_class) 
                    << "Attribute " << a_name << " has already been defined in the class" 
                    << curr_class->get_name() << ".\n";
                }
                else 
                    attrsMap[a_name] = attr;
            }
        }

        this->class_methods[curr_class->get_name()] = methodsMap;
        this->class_attrs[curr_class->get_name()] = attrsMap;
    }
}



void ClassTable::check_method(Class_ current_class, method_class* curr_method, method_class* par_method) {
    Formals curr_method_formals = curr_method->get_formals();
    Formals par_method_formals = par_method->get_formals();

    if(curr_method->get_return_type() != par_method->get_return_type()) {
        semant_error(current_class) << "In redefined method " << curr_method->get_name()<< ", the return type " << curr_method->get_return_type() 
            << " is different from the ancestor method return type " << par_method->get_return_type()<< ".\n";
    }

    int curr_method_argnum = 0;
    int par_method_argnum = 0;
    while (curr_method_formals->more(curr_method_argnum))
        curr_method_argnum = curr_method_formals->next(curr_method_argnum);

    while (par_method_formals->more(par_method_argnum))
        par_method_argnum = par_method_formals->next(par_method_argnum);

    if (curr_method_argnum != par_method_argnum) {
        semant_error(current_class) << "In redefined method " << curr_method->get_name() << ", the number of arguments " 
            << "(" << curr_method_argnum << ")"<< " differs from the ancestor method's "<< "number of arguments "
            << "(" << par_method_argnum << ")"<< ".\n";
    }

    int idx1 = 0, idx2 = 0;

    while (curr_method_formals->more(idx1) && par_method_formals->more(idx2))
    {
        Formal curr_formal = curr_method_formals->nth(idx1);
        Formal parent_formal = par_method_formals->nth(idx2);

        if (curr_formal->get_type() != parent_formal->get_type()) {
            semant_error(current_class)<< "In redefined method "<< curr_method->get_name()<< ", the type of argument " << curr_formal->get_type() 
                << " differs from the ancestor method's corresponding argument type "<< parent_formal->get_type()<< ".\n";
        }

        idx1 = curr_method_formals->next(idx1);
        idx2 = par_method_formals->next(idx2);
    }

    if (parentClass.find(current_class->get_name()) != parentClass.end()) {
        Symbol parent_class_name = parentClass[current_class->get_name()];
        Class_ parent_class = classMap[parent_class_name];

        check_method(
            parent_class, 
            curr_method, 
            class_methods[parent_class_name][current_class->get_name()]
        );
    }

}


void ClassTable::check_attr(Class_ curr_class, attr_class* attr) {
    if (class_attrs[curr_class->get_name()].find(attr->get_name()) != class_attrs[curr_class->get_name()].end()) {
        semant_error(curr_class)<< " Attribute "<< attr->get_name()<< " is an attribute of an inherited class.\n";
        return;
    }
    Symbol parent_name = parentClass[curr_class->get_name()];
    if (classMap.find(parent_name) != classMap.end()) {
        check_attr(classMap[parent_name], attr);
    }
}



Symbol ClassTable::least_common_ancestor(Symbol left, Symbol right) { // least common ancestor's type

    Symbol l_class = left;
    Symbol r_class = left;
    std::unordered_set<Symbol> r_ancestors;
    
    while (r_class != Object) 
    {
        r_ancestors.insert(r_class);
        r_class = parentClass[r_class];
    }
    while (l_class != Object) 
    {
        if (r_ancestors.find(l_class) != r_ancestors.end())
            return l_class;
        l_class = parentClass[l_class];
    }
    return Object;
}

bool ClassTable::conform(Symbol type1, Symbol type2) {
    if (type1 == No_type) return true;

    if (type1 == SELF_TYPE) {
        if (type2 == SELF_TYPE)
            return true;
        else 
            type1 = curr_class->get_name();
    }
    Symbol current = type1;

    while (current != Object && current != type2)
        current = parentClass[current];

    return current == type2;
}



////////////////////////////////////////////////////////////////////
//
//                        Type checking
//
////////////////////////////////////////////////////////////////////



Symbol object_class::type_check(ClassTableP class_table) {
    if (name == self) {
        type = SELF_TYPE;
    } 
    else if (symbol_table->lookup(name)) {
        this->set_type(*symbol_table->lookup(name));
    } else {
        class_table->semant_error(this) << "Undeclared identifier " << name << ".\n";
        type = Object;
    }
    return get_type();
}

Symbol new__class::type_check(ClassTableP class_table) {
    if (this->type_name != SELF_TYPE && !class_table->is_type_defined(type_name)) {
        class_table->semant_error(this) << "'new' used with undefined class " << this->type_name << ".\n";
        this->type_name = Object;
    }
    type = this->type_name;
    return type;
}


Symbol no_expr_class::type_check(ClassTableP) {
    this->set_type(No_type);
    return No_type;
}

Symbol isvoid_class::type_check(ClassTableP class_table) {
    e1->type_check(class_table);
    this->set_type(Bool);
    return Bool;
}

Symbol block_class::type_check(ClassTableP class_table) {
    Symbol nth_body_expr_type = Object;
    for (int i = body->first(); body->more(i); i = body->next(i))
        nth_body_expr_type = body->nth(i)->type_check(class_table);
    this->set_type(nth_body_expr_type);
    return nth_body_expr_type;
}

Symbol branch_class::type_check(ClassTableP class_table) {
    if (name == self) {
        class_table->semant_error(this) << "'self' cannot be bound in a 'branch' expression.";
    }
    symbol_table->enterscope();
    symbol_table->addid(name, new Symbol(type_decl));
    Symbol expr_type = expr->type_check(class_table);
    this->set_type(expr_type);
    symbol_table->exitscope();
    return expr_type;
}


Symbol typcase_class::type_check(ClassTableP class_table) {
    Symbol expr_type = expr->type_check(class_table);

    std::unordered_set<Symbol> branch_type_decls;

    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        branch_class* branch = static_cast<branch_class*>(cases->nth(i));
        Symbol branch_type_decl = branch->get_type_decl();
        if (branch_type_decls.find(branch_type_decl) != branch_type_decls.end())
            class_table->semant_error(this)<< "Duplicate branch type" << branch_type_decl << " in case statement.\n";
        else
            branch_type_decls.insert(branch_type_decl);

        Symbol branch_type = branch->type_check(class_table);
        if (i == cases->first())
            type = branch_type;
        else if (type != SELF_TYPE || branch_type != SELF_TYPE)
            type = class_table->least_common_ancestor(type, branch_type);
    }

    return type;
}

Symbol loop_class::type_check(ClassTableP class_table) {
    Symbol pred_type = pred->type_check(class_table);
    Symbol body_type = body->type_check(class_table);

    if (pred_type != Bool)
    {
        class_table->semant_error(this)<< "Expected the predicate of while to be of type Bool"<< " but got the predicate of type "
        << pred_type<< " instead .\n";
    }

    this->set_type(Object);
    return Object; 
}

Symbol let_class::type_check(ClassTableP class_table) {
    symbol_table->enterscope();
    if (identifier == self)
        class_table->semant_error(this) << "'self' cannot be bound in a 'let' expression.\n";

    Symbol init_type = init->type_check(class_table);

    if (type_decl != SELF_TYPE && !class_table->is_type_defined(type_decl))
        class_table->semant_error(this) << "Type "<<type_decl<< " of let-bound identifier "<< identifier<< " is undefined.\n";

    else if (init_type != No_type && !class_table->conform(init_type, type_decl))
        class_table->semant_error(this)<< "Inferred type "<< init_type<< " in initialization of " 
        <<identifier<< " does not conform to identifier's declared type "<< type_decl << ".\n";
            
    symbol_table->addid(identifier, new Symbol(type_decl));
    this->set_type(body->type_check(class_table));
    symbol_table->exitscope();
    return type;
}


Symbol cond_class::type_check(ClassTableP class_table) {
    Symbol pred_type = pred->type_check(class_table);
    Symbol then_type = then_exp->type_check(class_table);
    Symbol else_type = else_exp->type_check(class_table);

    if (pred_type != Bool)
    {
        class_table->semant_error(this)<< "Expected the predicate of if to be of type Bool,"<< " but got the predicate of type "
            << pred_type<< " instead .\n";
    }

    Symbol cond_type = class_table->least_common_ancestor(then_type, else_type);
    this->set_type(cond_type);
    return cond_type;
}


Symbol comp_class::type_check(ClassTableP class_table) {
    Symbol expr_type = e1->type_check(class_table);
    if (expr_type == Bool) {
        this->set_type(expr_type);
        return expr_type;
    }
    this->set_type(Object);
    class_table->semant_error(this)<< "Argument of 'not' has type "<< expr_type << " instead of Bool.\n";
    return Object;
}


Symbol leq_class::type_check(ClassTableP class_table) {
    Symbol left_type = e1->type_check(class_table);
    Symbol right_type = e2->type_check(class_table);

    if(left_type == Int && right_type == Int) {
        this->set_type(Bool);
    }
    else {
        this->set_type(Object);

        class_table->semant_error(this)<< "Expected both arguments of operator <= to be of type Int"
            << " but got arguments of types "<< left_type << " and "<< right_type<< ".\n";
    }
    return this->get_type();
}

Symbol eq_class::type_check(ClassTableP class_table) {
    Symbol left_type = e1->type_check(class_table);
    Symbol right_type = e2->type_check(class_table);
    
    bool is_left_type_primitive = left_type == Int || left_type == Bool || left_type == Str;
    bool is_right_type_primitive = right_type == Int || right_type == Bool || right_type == Str;
    this->set_type(Bool);

    if ((is_left_type_primitive && is_right_type_primitive) && left_type != right_type)
    {
        class_table->semant_error(this) << "Illegal comparison with a basic type.\n";
        this->set_type(Object);
    }

    return this->get_type();
}

Symbol lt_class::type_check(ClassTableP class_table) {
    Symbol left_type = e1->type_check(class_table);
    Symbol right_type = e2->type_check(class_table);

    if(left_type == Int && right_type == Int) {
        this->set_type(Bool);
    }
    else
    {
        this->set_type(Object);
        class_table->semant_error(this)<< "Expected both arguments of operator < to be of type Int"
            << " but got arguments of types "<< left_type<< " and "<< right_type<< ".\n";
    }
    return this->get_type();
}

Symbol neg_class::type_check(ClassTableP class_table) {
    Symbol expr_type = e1->type_check(class_table);
    this->set_type(Int);

    if (expr_type != Int)
    {
        this->set_type(Object);
        class_table -> semant_error(this) << "Argument of the operator '~' has type " << expr_type << " instead of Int.\n";
    }
    return this->get_type();
}


Symbol mul_class::type_check(ClassTableP class_table) {
    Symbol left_type = e1->type_check(class_table);
    Symbol right_type = e2->type_check(class_table);
    if(left_type == Int && right_type == Int)
        this->set_type(Int);
    else
    {
        class_table->semant_error(this)<< "Expected both arguments of operator * to be of type Int"
            << " but got arguments of types "<< left_type<< " and "<< right_type<< ".\n";
        this->set_type(Object);
    }
    return this->get_type();
}

Symbol divide_class::type_check(ClassTableP class_table) {
    Symbol left_type = e1->type_check(class_table);
    Symbol right_type = e2->type_check(class_table);
    if(left_type == Int && right_type == Int)
        this->set_type(Int);
    else
    {
        class_table->semant_error(this)<< "Expected both arguments of operator * to be of type Int"
            << " but got arguments of types "<< left_type<< " and "<< right_type<< ".\n";
        this->set_type(Object);
    }
    return this->get_type();
}

Symbol sub_class::type_check(ClassTableP class_table) {
    Symbol left_type = e1->type_check(class_table);
    Symbol right_type = e2->type_check(class_table);
    if(left_type == Int && right_type == Int)
        this->set_type(Int);
    else
    {
        class_table->semant_error(this)<< "Expected both arguments of operator * to be of type Int"
            << " but got arguments of types "<< left_type<< " and "<< right_type<< ".\n";
        this->set_type(Object);
    }
    return this->get_type();
}
Symbol plus_class::type_check(ClassTableP class_table) {
    Symbol left_type = e1->type_check(class_table);
    Symbol right_type = e2->type_check(class_table);
    if(left_type == Int && right_type == Int)
        this->set_type(Int);
    else
    {
        class_table->semant_error(this)<< "Expected both arguments of operator * to be of type Int"
            << " but got arguments of types "<< left_type<< " and "<< right_type<< ".\n";
        this->set_type(Object);
    }
    return this->get_type();
}



Symbol assign_class::type_check(ClassTableP class_table) {
    Symbol expr_type = expr->type_check(class_table);
    this->set_type(expr_type);

    if (name == self) {
        class_table->semant_error(this) << "Cannot assign to 'self'" << ".\n";
        this->set_type(Object);
    }

    Symbol* identifier_type = symbol_table->lookup(name);

    if (!identifier_type) {
        class_table->semant_error(this)<< "Tried to assign undeclared identifier "<< name<< ".\n";
        this->set_type(expr_type);
    }

    if (!class_table->conform(expr_type, *identifier_type)) {
        class_table->semant_error(this)<< "The identifier " << name << " has been declared as  "<< *identifier_type
            << " but assigned with incompatible type "<< expr_type<< ".\n";
        this->set_type(Object);
        
    }
    
    return this->get_type();
}



method_class* ClassTable::find_method(Symbol class_name, Symbol method_name) { // find method that is potentially inherited
    if (class_name == No_type) 
        return nullptr;
    method_class* method = get_class_method(class_name, method_name);

    if (method) return method;

    Symbol parent_class_name = get_parent_class(class_name);
    return find_method(parent_class_name, method_name);
}



Symbol dispatch_class::type_check(ClassTableP class_table) {
    Symbol expr_type = expr->type_check(class_table);

    if (expr_type != SELF_TYPE && !class_table->is_type_defined(expr_type)) {

        class_table->semant_error(this)<< "Dispatch on undefined class "<< expr_type<< ".\n";
        this->set_type(Object);
        return this->get_type();;
    }

    Symbol expr_type_name = expr_type == SELF_TYPE ? class_table->curr_class->get_name() : expr_type;
    method_class* method = class_table->find_method(expr_type_name, name);
    if (!method) 
    {
        class_table->semant_error(this)<< "Dispatch to undefined method "<< name<< ".\n";
        this->set_type(Object);
        return this->get_type();
    }

    bool error = false;

    Formals formals = method->get_formals();
    int k1 = actual->first(), k2 = formals->first();
    while (actual->more(k1) && formals->more(k2)) {
        Symbol actual_type = actual->nth(k1)->type_check(class_table);
        Symbol formal_type = formals->nth(k2)->get_type();
        if (!class_table->conform(actual_type, formal_type)) {
            error = true;
            class_table->semant_error(this) << "In call of method " << name << ", type " << actual_type << " of parameter " << formals->nth(k2)->get_name() << " does not conform to declared type " << formal_type << ".\n";
        }
        k1 = actual->next(k1);
        k2 = formals->next(k2);
        if (actual->more(k1) xor formals->more(k2)) {
            error = true;
            class_table->semant_error(this) << "Method " << name << " called with wrong number of arguments.\n";
        }
    }

    if (error) {
        this->set_type(Object);
    }
    else {
        Symbol dispatch_type = method->get_return_type();
        if (dispatch_type == SELF_TYPE) 
            dispatch_type = expr_type_name;
        this->set_type(dispatch_type);
    }

    return this->get_type();
}



Symbol static_dispatch_class::type_check(ClassTableP class_table) {

    Symbol expr_type = expr->type_check(class_table);

    if (this->type_name != SELF_TYPE && !class_table->is_type_defined(this->type_name)) {
        class_table->semant_error(this)<< "Static dispatch on undefined class " << this->type_name << ".\n";
        this->set_type(Object);
        return this->get_type();
    }

    if (expr_type != SELF_TYPE && !class_table->is_type_defined(expr_type)) {
        this->set_type(Object);
        return this->get_type();
    }

    bool error = false;

    if (!class_table->conform(expr_type, this->type_name)) {
        error = true;
        class_table->semant_error(this) << "Expression type " << expr_type << " does not conform to declared static dispatch type " << this->type_name << ".\n";
    }

    method_class* method = class_table->find_method(type_name, name);
    if (!method) 
    {
        class_table->semant_error(this)<< "Dispatch to undefined method " << name << ".\n";
        this->set_type(Object);
        return this->get_type();
    }

    Formals formals = method->get_formals();
    int k1 = actual->first(), k2 = formals->first();
    while (actual->more(k1) && formals->more(k2)) {
        Symbol actual_type = actual->nth(k1)->type_check(class_table);
        Symbol formal_type = formals->nth(k2)->get_type();
        if (!class_table->conform(actual_type, formal_type)) {
            error = true;
            class_table->semant_error(this) << "In call of method " << name << ", type " << actual_type << " of parameter " << formals->nth(k2)->get_name() << " does not conform to declared type " << formal_type << ".\n";
        }
        k1 = actual->next(k1);
        k2 = formals->next(k2);
        if (actual->more(k1) xor formals->more(k2)) {
            error = true;
            class_table->semant_error(this) << "Method " << name << " called with wrong number of arguments.\n";
        }
    }

    if (error)
    {
        this->set_type(Object);
    }
    else {
        Symbol dispatch_type = method->get_return_type();
        if (dispatch_type == SELF_TYPE) 
            dispatch_type = expr_type;
        this->set_type(dispatch_type);
    }
    return this->get_type();
}



Symbol method_class::type_check(ClassTableP class_table) {
    symbol_table->enterscope();
    std::unordered_set<Symbol> defined_formals;

    for (int idx = formals->first(); formals->more(idx); idx = formals->next(idx))
    {
        Symbol formal_name = formals->nth(idx)->get_name();
        Symbol formal_type = formals->nth(idx)->get_type();

        if(formal_name == self)
            class_table->semant_error(formals->nth(idx)) << "'self' cannot be the name of a method argument.\n";

        else if(defined_formals.find(formal_name) != defined_formals.end())
            class_table->semant_error(formals->nth(idx))<< "The argument "<< formal_name << " in the signature of method " << get_name()<< " has already been defined.\n";

        else
        {
           defined_formals.insert(formal_name);
        }
        
        if (!class_table->is_type_defined(formal_type))
            class_table->semant_error(formals->nth(idx)) << "The argument " << formal_name << " in the signature of method "<< get_name()
                << " has undefined type "<< formal_type<< " .\n";
        else
            symbol_table->addid(formal_name, new Symbol(formal_type));
    }

    Symbol expr_type = expr->type_check(class_table);
    if (return_type != SELF_TYPE && !class_table->is_type_defined(return_type))
        class_table->semant_error(this) << "Undefined return type " << return_type << " in method " << name << ".\n";

    else if (!class_table->conform(expr_type, return_type)) {
        class_table->semant_error(this) << "Inferred return type " << expr_type << " of method " << name << " does not conform to declared return type " << return_type << ".\n";

    }
   
    symbol_table->exitscope();
    return return_type;
}



Symbol attr_class::type_check(ClassTableP class_table) {
    Symbol init_type = init->type_check(class_table);
    init_type = init_type == SELF_TYPE ? class_table->curr_class->get_name() : init_type;
    
    if (dynamic_cast<const no_expr_class*>(init) != nullptr)
        return this->get_type();

    if (this->get_name() == self) {
        class_table->semant_error(this) << "'self' cannot be the name of an attribute.\n";
    }
    
    if (!class_table->is_type_defined(this->get_type())) {
        class_table->semant_error(this)<< "The attribute "<< this->get_name()<< " is defined as " << this->get_type()
            << " but the type is undefined. \n";
    }

    if (!class_table->conform(init_type, this->get_type())) {
        class_table->semant_error(this)<< "The attribute "<< this->get_name()<< " is defined as "<< this->get_type()
            << " and does not conform with initiliazation type "<< init_type<< ".\n";
    }
    return this->get_type();
}



Symbol int_const_class::type_check(ClassTableP) {
    this->set_type(Int);
    return Int;
}

Symbol bool_const_class::type_check(ClassTableP) {
    this->set_type(Bool);
    return Bool;
}

Symbol string_const_class::type_check(ClassTableP) {
    this->set_type(Str);
    return Str;
}



////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error(tree_node *t) {
    error_stream << this->curr_class->get_filename() << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    // stage 1
    ClassTable *classtable = new ClassTable(classes);
    if (classtable->is_acyclic() || classtable->check_main()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
	    exit(1);
    }

    // stage 2
    classtable->install_features(classes);
    classtable->type_check(classes);

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


