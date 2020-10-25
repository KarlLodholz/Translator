#ifndef SYMBOL_TABLE_ENTRY_H
#define SYMBOL_TABLE_ENTRY_H

#include <string>
using namespace std;

#define UNDEFINED  -1
#define FUNCTION 0

#define INT 1 
#define STR 2
#define FLOAT 3
#define BOOL 4
#define LIST 5
#define INT_OR_STR_OR_FLOAT_OR_BOOL 6 
#define NULL_TYPE 7

#define NOT_APPLICABLE -2

typedef struct { 
  int type;                // one of the above type codes 
  int numParams;           // numParams and returnType only applicable if type == FUNCTION
  int returnType;    
} TYPE_INFO;

class SYMBOL_TABLE_ENTRY 
{
private:
  // Member variables
  TYPE_INFO type_struct;
  string name;
  
public:
  // Constructors:
  // Default Constructor
  SYMBOL_TABLE_ENTRY( ) 
  { 
	  name = ""; 
    type_struct.type = NOT_APPLICABLE; 
    type_struct.numParams = NOT_APPLICABLE;
    type_struct.returnType = NOT_APPLICABLE;
  }
  
  // Table entries with all specified parameters constructor
  SYMBOL_TABLE_ENTRY(const string theName, const int theType, const int numParams, const int returnType) 
  {
    name = theName;
    type_struct.type = theType;
    type_struct.numParams = numParams;
    type_struct.returnType = returnType;
  }
  
  // Accessors
  int getTheType() const {return type_struct.type;}
  
  string getName() const { return name; }
  TYPE_INFO getTypeStruct() const { return type_struct; }

  void setTheType(int theType)
    {
        type_struct.type = theType;
        return;
    }
};

#endif  // SYMBOL_TABLE_ENTRY_H
