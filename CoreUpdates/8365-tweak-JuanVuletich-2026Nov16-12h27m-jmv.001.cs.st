'From Cuis7.9 [latest update: #8364] on 16 November 2026 at 12:28:13 pm'!

!CompiledMethod methodsFor: 'testing' stamp: 'jmv 16/Nov/2026, 3:27:57 pm (UTC)'!
isInstalled
	"
	Regular methods answer true
	Not installed methods include:
	- Obsolete methods (replaced by newer versions) but still referenced, for instance,
		because a contained closure is stored somewhere
	- #DoIt
		- without #methodClass (as usual Workspace doIts)
		- with a #methodClass (like a doIt evaluated in an inspector, referencing self)
	"
	self methodClass ifNotNil:
		[ :class |
		self selector ifNotNil:
			[ :selector |
			^self == (class compiledMethodAt: selector ifAbsent: nil)]].
	^false! !

