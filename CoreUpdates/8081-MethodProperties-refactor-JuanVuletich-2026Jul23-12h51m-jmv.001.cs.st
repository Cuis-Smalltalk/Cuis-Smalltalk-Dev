'From Cuis7.9 [latest update: #8080] on 23 July 2026 at 1:30:25 pm'!

!AdditionalMethodState methodsFor: 'properties' stamp: 'jmv 23/Jul/2026, 2:53:50 pm (UTC)'!
propertyOrPragmaAt: aKey
	"Answer the element (property or pragma) associated with aKey.
	Answer nil if not found."

	| propertyOrPragma "<Association|ReadOnlyVariableBinding|Pragma>" |
	1 to: self basicSize do: [:i |
		propertyOrPragma := self basicAt: i.
		propertyOrPragma key == aKey ifTrue:
			[^propertyOrPragma]].
	^nil! !


!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'jmv 23/Jul/2026, 2:58:37 pm (UTC)'!
removeProperty: propName
	"Remove the property propName if it exists.
	 Do _not_ raise an error if the property is missing."

	self
		withPropertiesDo: [ :properties |
			(properties propertyOrPragmaAt: propName) ifNotNil: [ :property |
				self penultimateLiteral: (properties copyWithout: property) ]]
		ifSelector: [ :selector | ].! !

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'jmv 23/Jul/2026, 4:26:49 pm (UTC)'!
removeProperty: propName ifAbsent: aBlock
	"Remove the property propName if it exists.
	 Do _not_ raise an error if the property is missing."

	self
		withPropertiesDo: [ :properties |
			(properties propertyOrPragmaAt: propName)
				ifNotNil: [ :property |
					self penultimateLiteral: (properties copyWithout: property) ]
				ifNil: [ aBlock value ]]
		ifSelector: [ :selector | aBlock value ].! !


!AdditionalMethodState methodsFor: 'copying' stamp: 'jmv 23/Jul/2026, 3:08:47 pm (UTC)'!
copyWithout: aPropertyOrPragma "<Association|Pragma>"
	"Answer a copy of the receiver which no longer includes aPropertyOrPragma
	if not found, answer self."
	| bs copy offset |
	copy := self class new: (bs := self basicSize) - 1.
	offset := 0.
	1 to: bs do:
		[ :i |
		(self basicAt: i) == aPropertyOrPragma
			ifTrue: [offset := 1]
			ifFalse: [copy basicAt: i - offset put: (self basicAt: i)]].
	offset = 1
		ifTrue: [ 
			^copy
				selector: selector;
				setMethod: method;
				yourself ]
		ifFalse: [ ^self ].! !

