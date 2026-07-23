'From Cuis7.9 [latest update: #8080] on 23 July 2026 at 1:33:05 pm'!

!CompiledMethod methodsFor: 'accessing-pragmas & properties' stamp: 'jmv 23/Jul/2026, 3:18:32 pm (UTC)'!
propertyValueAt: propName put: propValue
	"Set or add the property with key propName and value propValue.
	 If the receiver does not yet have a method properties create one and replace
	 the selector with it.  Otherwise, either relace propValue in the method properties
	 or replace method properties with one containing the new property."
	
	self 
		withPropertiesDo: [ :properties |
			(properties propertyOrPragmaAt: propName)
				ifNotNil: [ :propertyOrPragma |
					propertyOrPragma isVariableBinding
						ifTrue: [ propertyOrPragma value: propValue ]
						ifFalse: [ self error: 'I already have a Pragma with requested name' ]]
				ifNil: [
					self penultimateLiteral: (properties
						copyWith: (Association
							key: propName asSymbol
							value: propValue))].
			^propValue ] 
		ifSelector: [ :selector |
			self penultimateLiteral: ((AdditionalMethodState
									selector: selector
									with: (Association
											key: propName asSymbol
											value: propValue))
									setMethod: self;
									yourself).
			^propValue].! !

!methodRemoval: AdditionalMethodState #includesProperty: stamp: 'jmv 23/Jul/2026, 4:32:39 pm (UTC)'!
AdditionalMethodState removeSelector: #includesProperty:!
