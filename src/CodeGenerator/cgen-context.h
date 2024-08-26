#include "cool-tree.h"
#include "symtab.h"
#include <vector>
#include <unordered_map>

#ifndef cgen_context_h
#define cgen_context_h

class Class__class;
typedef Class__class *Class_;


typedef CGenContext *CGenContextP;

class CGenContext {
public:
    Class_                                                          self_class;
	Symbol                                                          self_class_name;
	Symbol                                                          method_name;
	std::vector<Symbol>                                             scope;
	std::unordered_map<Symbol, int>                                 class_attr_offset;
	std::unordered_map<Symbol, int>                                 classtag_map;
	std::unordered_map<Symbol, int>                                 method_attr_offset;
	std::unordered_map<Symbol, std::unordered_map<Symbol, int>>     dispatch_methods_offset;
	

	void push_scope_identifier(Symbol identifier) {
		scope.push_back(identifier);
	}

	void pop_scope_identifier() {
		scope.pop_back();
	}

	int get_scope_identifier_offset(Symbol identifier) {
		if (scope.size() == 0)
			return -1;
		int idx = scope.size() - 1;
		while(idx >= 0 && scope[idx] != identifier)
			idx--;
		int scope_offset = scope.size() - 1 - idx;
		return (scope[idx] == identifier ? scope_offset : -1);
	}

	int get_method_attr_offset(Symbol identifier) {
		return (method_attr_offset.find(identifier) != method_attr_offset.end() ?
            method_attr_offset[identifier] : -1);
	}

	int get_class_attr_offset(Symbol identifier) {
		return (class_attr_offset.find(identifier) != class_attr_offset.end() ?
			class_attr_offset[identifier] : -1);
	}
};



#endif