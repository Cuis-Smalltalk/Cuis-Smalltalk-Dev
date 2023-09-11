'From Cuis 6.0 [latest update: #6018] on 11 September 2023 at 12:46:05 pm'!

!Symbol class methodsFor: 'private' stamp: 'jmv 9/11/2023 10:49:50'!
rehash	
	"Rebuild the hash table"
	"Symbol rehash"

	Symbols := SymbolSet new.
	Symbols rehash! !

