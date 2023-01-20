'From Cuis 6.0 [latest update: #5636] on 20 January 2023 at 9:38:31 am'!

!Symbol class methodsFor: 'private' stamp: 'jmv 1/20/2023 09:38:11'!
rehash	
	"Rebuild the hash table, reclaiming unreferenced Symbols."
	"
	Symbol rehash
	"
	SymbolTable _ WeakSet withAll: Symbol allInstances.
	SymbolTable addAll: UnicodeSymbol allInstances.
	NewSymbols _ WeakSet new.! !

