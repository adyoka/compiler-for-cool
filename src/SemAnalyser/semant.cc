

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
        Class_ curr_class = classes->nth(i);
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

        Class_ curr_class = mapEntry.second;
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





////////////////////////////////////////////////////////////////////
//
//                        Features utilities
//
////////////////////////////////////////////////////////////////////


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
        Class_ curr_class = classes->nth(i);
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



void ClassTable::check_method(Class_ curr_class, method_class* curr_method, method_class* par_method) {
    Formals curr_method_formals = curr_method->get_formals();
    Formals par_method_formals = par_method->get_formals();

    if(curr_method->get_return_type() != par_method->get_return_type()) {
        semant_error(curr_class) << "In redefined method " << curr_method->get_name()<< ", the return type " << curr_method->get_return_type() 
            << " is different from the ancestor method return type " << par_method->get_return_type()<< ".\n";
    }

    int curr_method_argnum = 0;
    int par_method_argnum = 0;
    while (curr_method_formals->more(curr_method_argnum))
        curr_method_argnum = curr_method_formals->next(curr_method_argnum);

    while (par_method_formals->more(par_method_argnum))
        par_method_argnum = par_method_formals->next(par_method_argnum);

    if (curr_method_argnum != par_method_argnum) {
        semant_error(curr_class) << "In redefined method " << curr_method->get_name() << ", the number of arguments " 
            << "(" << curr_method_argnum << ")"<< " differs from the ancestor method's "<< "number of arguments "
            << "(" << par_method_argnum << ")"<< ".\n";
    }

    int idx1 = 0, idx2 = 0;

    while (curr_method_formals->more(idx1) && par_method_formals->more(idx2))
    {
        Formal curr_formal = curr_method_formals->nth(idx1);
        Formal parent_formal = par_method_formals->nth(idx2);

        if (curr_formal->get_type() != parent_formal->get_type()) {
            semant_error(curr_class)<< "In redefined method "<< curr_method->get_name()<< ", the type of argument " << curr_formal->get_type() 
                << " differs from the ancestor method's corresponding argument type "<< parent_formal->get_type()<< ".\n";
        }

        idx1 = curr_method_formals->next(idx1);
        idx2 = par_method_formals->next(idx2);
    }

    if (parentClass.find(curr_class->get_name()) != parentClass.end()) {
        Symbol parent_class_name = parentClass[curr_class->get_name()];
        Class_ parent_class = classMap[parent_class_name];

        check_method(
            parent_class, 
            curr_method, 
            class_methods[parent_class_name][curr_class->get_name()]
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



void ClassTable::type_check(Classes classes) {
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        Class_ curr_class = classes->nth(i);
        // symbol_table = new SymbolTable<Symbol, Symbol>();
        symbol_table->enterscope();
        symbol_table->addid(self, new Symbol(curr_class->get_name()));

        add_attributes_scope(curr_class);

        for (auto const& methodsEntry : class_methods[curr_class->get_name()]) {
            check_method(curr_class, methodsEntry.second, methodsEntry.second);
        }

        for (auto const& attrEntry : class_attrs[curr_class->get_name()]) {
            if (parentClass.find(curr_class->get_name()) != parentClass.end()) {
                Symbol parent_class_name = parentClass[curr_class->get_name()];
                Class_ parent_class = classMap[parent_class_name];

                check_attr(parent_class, attrEntry.second);
            }

        }

        symbol_table->exitscope();
    }
}


Symbol ClassTable::least_common_ancestor(Symbol left, Symbol right, ClassTableP class_table) { // least common ancestor's type

    Symbol l_class = left;
    Symbol r_class = left;
    std::unordered_set<Symbol> r_ancestors;
    
    while (r_class != Object || r_class != No_type) 
    {
        r_ancestors.insert(r_class);
        r_class = class_table->get_parent_class(r_class);
    }
    while (l_class != Object || l_class != No_type) 
    {
        if (r_ancestors.find(l_class) != r_ancestors.end())
            return l_class;
        l_class = class_table->get_parent_class(l_class);
    }
    return Object;
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
        class_table->semant_error() << "Undeclared identifier " << name << ".\n";
        type = Object;
    }
    return get_type();
}

Symbol new__class::type_check(ClassTableP class_table) {
    if (this->type_name != SELF_TYPE && !class_table->is_type_defined(type_name)) {
        class_table->semant_error() << "'new' used with undefined class " << this->type_name << ".\n";
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
        class_table->semant_error() << "'self' cannot be bound in a 'branch' expression.";
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
            class_table->semant_error()<< "Duplicate branch type" << branch_type_decl << " in case statement.\n";
        else
            branch_type_decls.insert(branch_type_decl);

        Symbol branch_type = branch->type_check(class_table);
        if (i == cases->first())
            type = branch_type;
        else if (type != SELF_TYPE || branch_type != SELF_TYPE)
            type = class_table->least_common_ancestor(type, branch_type, class_table);
    }

    return type;
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


