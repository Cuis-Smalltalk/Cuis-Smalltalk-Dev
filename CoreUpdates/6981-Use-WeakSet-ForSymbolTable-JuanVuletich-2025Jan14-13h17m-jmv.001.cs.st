'From Cuis7.3 [latest update: #6981] on 14 January 2025 at 1:17:53 pm'!

!Symbol class methodsFor: 'class initialization' stamp: 'jmv 1/14/2025 13:13:34'!
initialize
	"Rebuild the hash table"
	"
	Symbol initialize.
	"
	self accessProtect critical:
		[ 	| allSymbols |
			allSymbols := self allSymbols.
			Symbols := WeakSet new.
			Symbols addAll: allSymbols ].! !

!Symbol class methodsFor: 'private' stamp: 'jmv 1/14/2025 13:17:28'!
rehash
	"Rebuild the hash table"
	"
	Symbol rehash
	"

	
	self accessProtect critical: [
		Symbols rehash ].! !

Symbol initialize!
