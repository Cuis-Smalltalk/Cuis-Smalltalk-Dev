'From Cuis7.3 [latest update: #7150] on 28 April 2025 at 1:41:44 pm'!

!Symbol class methodsFor: 'access' stamp: 'jmv 4/28/2025 13:09:41'!
allSymbols
	"Answer all interned symbols"

	Symbols ifNil: [ ^#() ].
	^Array streamContents:
		[:s | Symbols do: [:each | s nextPut: each]]! !

!Symbol class methodsFor: 'class initialization' stamp: 'jmv 4/28/2025 13:14:17'!
initialize
	"Rebuild the hash table"
	"
	Symbol initialize.
	"
	self accessProtect critical:
		[ 	| allSymbols |
			allSymbols := Symbols ifNil: [#()].
			Symbols := WeakSet new.
			allSymbols do: [ :each |
				Symbols add: each ]].! !

Symbol initialize!
