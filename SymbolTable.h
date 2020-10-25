#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include <map>
#include <string>
#include "SymbolTableEntry.h"
using namespace std;
/*
class SYMBOL_TABLE
{
private:
  std::map<string, SYMBOL_TABLE_ENTRY> hashTable;

public:
  //Constructor
  SYMBOL_TABLE( ) { }

  // Add SYMBOL_TABLE_ENTRY x to this symbol table.
  // If successful, return true; otherwise, return false.
  bool addEntry(SYMBOL_TABLE_ENTRY x)
  {
    // Make sure there isn't already an entry with the same name
    map<string, SYMBOL_TABLE_ENTRY>::iterator itr;
    if ((itr = hashTable.find(x.getName())) == hashTable.end())
    {
      hashTable.insert(make_pair(x.getName(), x));
      return(true);
    }
    else return(false);
  }

  // If a SYMBOL_TABLE_ENTRY with name theName is
  // found in this symbol table, then return true;
  // otherwise, return false.
  bool findEntry(string theName)
  {
    map<string, SYMBOL_TABLE_ENTRY>::iterator itr;
    if ((itr = hashTable.find(theName)) == hashTable.end())
      return(false);
    else return(true);
  }

};

#endif  // SYMBOL_TABLE_H
*/

class SYMBOL_TABLE 
{
private:
  std::map<string, SYMBOL_TABLE_ENTRY> hashTable;

public:
  //Constructor
  SYMBOL_TABLE( ) { }

  // Find symbol table entry index with theName, return type 
  int getEntryTypeST(string theName)
  {
    return hashTable[theName].getTheType();     // defn STEntry.h: int getTheType() const {return type_struct.type;}
  }

  std::map<string, SYMBOL_TABLE_ENTRY> getHashTable()
  {
	 return hashTable; 
  }
  // Add SYMBOL_TABLE_ENTRY x to this symbol table.
  // If successful, return true; otherwise, return false.
  bool addEntry(SYMBOL_TABLE_ENTRY x) 
  {
    // Make sure there isn't already an entry with the same name
    map<string, SYMBOL_TABLE_ENTRY>::iterator itr;
    if ((itr = hashTable.find(x.getName())) == hashTable.end()) 
    {
      hashTable.insert(make_pair(x.getName(), x));
      return(true);
    }
    else return(false);
  }

  // If a SYMBOL_TABLE_ENTRY with name theName is
  // found in this symbol table, then return true;
  // otherwise, return false.
  bool findEntry(string theName) 
  {
    map<string, SYMBOL_TABLE_ENTRY>::iterator itr;
    // if ((itr = hashTable.find(theName)) == hashTable.end())
    //   return(false);
    // else return(true);
    return (itr = hashTable.find(theName)) != hashTable.end();
  }

  SYMBOL_TABLE_ENTRY* getEntry(string theName)
    {
        return &hashTable[theName];
    }

};

#endif  // SYMBOL_TABLE_H
