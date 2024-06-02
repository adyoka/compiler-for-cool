#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"
#include <unordered_map>
#include <map>
#include <unordered_set>
#include <vector>
enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   int curr_classtag;
   int ioclasstag;
   int objectclasstag;
   int objectparenttag;

   std::unordered_map<Symbol, int> classtag_map;
   std::unordered_map<Symbol, Class_> class_map;
   std::unordered_map<Symbol, Symbol> parent_map;
   std::map<int, Symbol> protObjs;

   std::unordered_map<Symbol, std::vector<Symbol>>                         class_method_names;
   std::unordered_map<Symbol, std::unordered_map<Symbol, method_class*>>   class_methods;
   std::unordered_map<Symbol, std::vector<Symbol>>                         class_attr_names;
   std::unordered_map<Symbol, std::unordered_map<Symbol, attr_class*>>     class_attrs;
   std::unordered_map<Symbol, std::unordered_set<Symbol>>                  class_directly_owned_attrs;
   std::unordered_map<Symbol, std::unordered_map<Symbol, Symbol>>          class_method_defined_in;
   
   std::unordered_map<Symbol, std::unordered_map<Symbol, int>>             dispatch_method_offsets;   // class_name -> method_name -> offset
   std::unordered_map<Symbol, std::unordered_map<Symbol, int>>             class_attr_offsets;        // class_name -> attr_name -> offset
// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   void traverse_inheritance_tree();
   void inherit_features(Class_, Class_);
   void install_features(Class_);

   int get_classtag(Symbol);
   void emit_nameTab();
   void emit_objTab();
   void emit_parentTab();
   void emit_protObjs();
   void emit_dispatch_table();
   void emit_default_val(Symbol);
   void emit_initializers();
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   Class_ node_class;

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   Class_ get_class_def() { return node_class; }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

