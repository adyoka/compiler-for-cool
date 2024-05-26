#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#include<unordered_map>
#include<vector>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  void install_classes(Classes);

  void build_inhertiance_graph();
  ostream& error_stream;

  std::unordered_map<Symbol, Class_> classMap;
  std::unordered_map<Symbol, Symbol> parentClass;
  std::unordered_map<Symbol, std::vector<Symbol>> inheritanceMap;
  std::unordered_map<Symbol, std::unordered_map<Symbol, method_class*>> class_methods;
  std::unordered_map<Symbol, std::unordered_map<Symbol, attr_class*>> class_attrs;
  
public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
    ostream& semant_error(tree_node *t);

  Class_ curr_class;
  
  Class_ get_class(Symbol s) { return classMap[s]; }
  Symbol get_parent_class(Symbol);
  method_class* get_class_method(Symbol, Symbol);
  bool is_type_defined(Symbol s) { return classMap.find(s) != classMap.end(); }
  bool is_basic_class(Symbol);
  bool traverse_graph(Symbol);
  bool is_acyclic();
  bool check_main();

  void install_features(Classes);
  void add_attributes_scope(Class_);
  void type_check(Classes);
  void check_method(Class_, method_class*, method_class*);
  void check_attr(Class_, attr_class*);
  Symbol least_common_ancestor(Symbol, Symbol);
  bool conform(Symbol, Symbol);
  method_class* find_method(Symbol, Symbol);
};


#endif

