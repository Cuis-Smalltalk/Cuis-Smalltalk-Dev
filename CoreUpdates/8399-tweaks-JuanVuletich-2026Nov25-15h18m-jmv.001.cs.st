'From Cuis7.9 [latest update: #8398] on 25 November 2026 at 3:18:36 pm'!

!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 25/Nov/2026, 5:41:07 pm (UTC)'!
propertiesAsArray
	"Answer an Array with all my method properties."

	^self 
		withPropertiesDo: [ :properties | properties propertiesAsArray ] 
		ifSelector: [ :selector | #() ]! !

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'jmv 25/Nov/2026, 5:41:16 pm (UTC)'!
pragmas
	"Answer an Array with all my Pragmas."
	
	^self withPropertiesDo: [ :properties | properties pragmas ] ifSelector: [ :selector | #() ]! !


!AdditionalMethodState methodsFor: 'copying' stamp: 'jmv 25/Nov/2026, 6:01:26 pm (UTC)'!
copyWith: aPropertyOrPragma "<Association|Pragma>"
	"Answer a copy of the receiver which includes aPropertyOrPragma"
	| bs copy |
	(#(Association Pragma)
			includes: aPropertyOrPragma class name) ifFalse: [
		self error: self class name, ' instances should hold only Method Properties (Association) and Pragmas.'].
	copy := self class new: (bs := self basicSize) + 1.
	1 to: bs do: [ :i |
		copy basicAt: i put: (self basicAt: i)].
	copy basicAt: bs + 1 put: aPropertyOrPragma.
	^copy
		selector: selector;
		setMethod: method;
		yourself! !

