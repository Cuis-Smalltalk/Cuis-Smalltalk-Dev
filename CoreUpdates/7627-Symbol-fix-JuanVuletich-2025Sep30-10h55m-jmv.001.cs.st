'From Cuis7.5 [latest update: #7625] on 30 September 2025 at 11:01:52 am'!

!UnicodeSymbol class methodsFor: 'testing' stamp: 'jmv 9/30/2025 10:56:01'!
representsSymbols
	^true! !


!Symbol class methodsFor: 'class initialization' library: #MinimalKernel02Symbol stamp: 'jmv 9/30/2025 10:56:57'!
initialize
	"Rebuild the hash table"
	"
	Symbol initialize.
	"
	self accessProtect critical:
		[
			Symbols := WeakSet new.
			CharacterSequence allSubclassesDo: [ :cls |
				cls representsSymbols ifTrue: [
					cls allInstancesDo: [ :sym | Symbols add: sym ]]]].! !

Symbol initialize!

!UnicodeSymbol class reorganize!
('instance creation' newFrom: readFrom:)
('private' basicFromString:)
('testing' representsSymbols)
!

