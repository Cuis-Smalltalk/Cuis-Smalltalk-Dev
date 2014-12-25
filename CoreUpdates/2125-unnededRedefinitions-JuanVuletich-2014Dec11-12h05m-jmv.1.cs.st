'From Cuis 4.2 of 25 July 2013 [latest update: #2124] on 11 December 2014 at 12:27:02.139711 pm'!

!CompiledMethod methodsFor: 'comparing' stamp: 'jmv 12/11/2014 12:26'!
= method
	| numLits |

	"Any object is equal to itself"
	self == method ifTrue: [ ^ true ].

	"Answer whether the receiver implements the same code as the 
	argument, method."
	(method is: #CompiledMethod) ifFalse: [ ^false ].
	self size = method size ifFalse: [ ^false ].
	self header = method header ifFalse: [ ^false ].
	self initialPC to: self endPC do: [ :i |
		(self at: i) = (method at: i) ifFalse: [ ^false ]].
	(numLits _ self numLiterals) ~= method numLiterals ifTrue: [ ^false ].

	"``Dont bother checking FFI and named primitives''
	jmv: Does this make any sense?
	 (#(117 120) includes: self primitive) ifTrue: [^ true]."

	 "properties"
	(self properties analogousCodeTo: method properties) ifFalse: [
		^false ].

	"#penultimateLiteral is selector (or properties, just compared, above)
	Last literal is #methodClass.
	Don't compare them. Two methods might be equal even if they have different selector (or none at all)
	or are installed in different classes (or none at all)"
	1 to: numLits-2 do: [ :i |
		| lit1 lit2 |
		lit1 _ self literalAt: i.
		lit2 _ method literalAt: i.
		lit1 = lit2 ifFalse: [
			(i = 1 and: [ #(117 120) includes: self primitive ])
				ifTrue: [
					lit1 isArray
						ifTrue: [
							(lit2 isArray and: [ lit1 allButLast = lit2 allButLast ]) ifFalse: [
								^false ]]
						ifFalse: [ "ExternalLibraryFunction"
							(lit1 analogousCodeTo: lit2) ifFalse: [
								^false ]]]
				ifFalse: [
					 lit1 isFloat
						ifTrue: [
							"Floats match if values are close, due to roundoff error."
							(lit1 closeTo: lit2) ifFalse: [ ^false ].
							self flag: 'just checking'. self halt ]
						ifFalse: [
							"any other discrepancy is a failure"
							^ false ]]]].
	^true! !


!SystemDictionary methodsFor: 'browsing' stamp: 'jmv 12/11/2014 12:26'!
browseLikelyUnnededRedefinitions

	"
	Spot unneded method redefinitions: methods that are equal to what would be inherited if they weren't there
	Smalltalk browseLikelyUnnededRedefinitions
	Be careful with class side #initialize methods that set up instance class variables
	In general, class side #initialize methods can't be removed unless empty.
	"

	Smalltalk browseAllSelect: [ :cm |
		(cm methodClass superclass ifNotNil: [ :sup | sup lookupSelector: cm selector]) = cm ]! !

