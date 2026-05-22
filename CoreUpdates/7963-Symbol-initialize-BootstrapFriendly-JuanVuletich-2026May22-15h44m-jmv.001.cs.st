'From Cuis7.7 [latest update: #7961] on 22 May 2026 at 3:45:53 pm'!

!Symbol class methodsFor: 'class initialization' library: #MinimalKernel02Symbol stamp: 'jmv 22/May/2026 14:09:23'!
initialize
	"Rebuild the hash table"
	"
	Symbol initialize.
	"
	self accessProtect critical:
		[
			Symbols := WeakSet new.
			CharacterSequence allSubclassesDo: [ :cls | | instances |
				cls representsSymbols ifTrue: [
					instances := cls allInstancesOrNil.
					instances notNil ifTrue: [
						instances do: [ :sym |
							Symbols add: sym ]]]]].! !

Symbol initialize!
