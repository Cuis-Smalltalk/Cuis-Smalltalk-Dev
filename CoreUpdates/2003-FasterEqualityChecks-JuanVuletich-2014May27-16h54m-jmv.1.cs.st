'From Cuis 4.2 of 25 July 2013 [latest update: #2002] on 27 May 2014 at 5:14:47.18228 pm'!

!CodePackage methodsFor: 'comparing' stamp: 'jmv 5/27/2014 17:04'!
= other

	^ self species == other species and: [ packageName = other packageName ]! !


!Bag methodsFor: 'comparing' stamp: 'jmv 5/27/2014 10:47'!
= aBag
	"Two bags are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same size.
	 (c) each element occurs the same number of times in both of them".

	"Any object is equal to itself"
	self == aBag ifTrue: [ ^ true ].

	(aBag isKindOf: Bag) ifFalse: [ ^ false ].
	self size = aBag size ifFalse: [ ^ false ].
	contents associationsDo: [ :assoc |
		(aBag occurrencesOf: assoc key) = assoc value
			ifFalse: [ ^ false ]].
	^true! !


!CompiledMethod methodsFor: 'comparing' stamp: 'jmv 5/27/2014 10:49'!
= method
	| numLits |

	"Any object is equal to itself"
	self == method ifTrue: [ ^ true ].

	"Answer whether the receiver implements the same code as the 
	argument, method."
	(method is: #CompiledMethod) ifFalse: [^false].
	self size = method size ifFalse: [^false].
	self header = method header ifFalse: [^false].
	self initialPC to: self endPC do:
		[:i | (self at: i) = (method at: i) ifFalse: [^false]].
	(numLits := self numLiterals) ~= method numLiterals ifTrue: [^false].
	"``Dont bother checking FFI and named primitives''
	 (#(117 120) includes: self primitive) ifTrue: [^ true]."
	1 to: numLits do:
		[:i| | lit1 lit2 |
		lit1 := self literalAt: i.
		lit2 := method literalAt: i.
		lit1 = lit2 ifFalse:
			[(i = 1 and: [#(117 120) includes: self primitive])
				ifTrue: [lit1 isArray
							ifTrue:
								[(lit2 isArray and: [lit1 allButLast = lit2 allButLast]) ifFalse:
									[^false]]
							ifFalse: "ExternalLibraryFunction"
								[(lit1 analogousCodeTo: lit2) ifFalse:
									[^false]]] ifFalse:
			[i = (numLits - 1) ifTrue: "properties"
				[(self properties analogousCodeTo: method properties) ifFalse:
					[^false]] ifFalse:
			 [lit1 isFloat
				ifTrue:
					["Floats match if values are close, due to roundoff error."
					(lit1 closeTo: lit2) ifFalse: [^false]. self flag: 'just checking'. self halt]
				ifFalse:
					["any other discrepancy is a failure"
					^ false]]]]].
	^true! !


!String methodsFor: 'comparing' stamp: 'jmv 5/27/2014 10:46'!
= aString 
	"Answer whether the receiver sorts equally as aString.
	This means same characters in same order."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a String or Symbol, maybe it is a Text?"
	aString species == String ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	^ (self compare: self with: aString collated: CaseSensitiveOrder) = 2! !


!SortedCollection methodsFor: 'comparing' stamp: 'jmv 5/27/2014 11:25'!
= aSortedCollection
	"Answer true if my and aSortedCollection's species are the same,
	and if our blocks are the same, and if our elements are the same."

	"Any object is equal to itself"
	self == aSortedCollection ifTrue: [ ^ true ].

	self species == aSortedCollection species ifFalse: [ ^ false ].

	sortBlock = aSortedCollection sortBlock
		ifFalse: [ ^false ].

	^self hasEqualElements: aSortedCollection ! !


!ColorMap methodsFor: 'comparing' stamp: 'jmv 5/27/2014 10:56'!
= aColorMap

	"Any object is equal to itself"
	self == aColorMap ifTrue: [ ^ true ].

	"Return true if the receiver is equal to aColorMap"
	self species == aColorMap species ifFalse: [ ^false ].
	self isIndexed == aColorMap isIndexed ifFalse: [ ^false ].
	^self colors = aColorMap colors and: [
		self shifts = aColorMap shifts and: [
			self masks = aColorMap masks ]]! !


!DirectoryEntry methodsFor: 'testing' stamp: 'jmv 5/27/2014 11:22'!
= aDirectoryEntry 

	"Any object is equal to itself"
	self == aDirectoryEntry ifTrue: [ ^ true ].

	"Answer whether I am equivalent in all of my file-system attributes."
	self species == aDirectoryEntry species ifFalse: [ ^ false ].

	^ self containingDirectory = aDirectoryEntry containingDirectory
		and: [ self name = aDirectoryEntry name
				and: [ self modificationTime = aDirectoryEntry modificationTime
						and: [ self fileSize = aDirectoryEntry fileSize ]]]! !


!FileDirectory methodsFor: 'comparing' stamp: 'jmv 5/27/2014 10:58'!
= aDirectory

	"Any object is equal to itself"
	self == aDirectory ifTrue: [ ^ true ].

	"Compare two FileDirectory instances."
	^ (pathName asString 
			compare: aDirectory pathName asString 
			caseSensitive: (self isCaseSensitive | aDirectory isCaseSensitive)) = 2! !


!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 5/27/2014 10:59'!
= comparand
	"comparand conforms to protocol DateAndTime,
	or can be converted into something that conforms."

	"Any object is equal to itself"
	self == comparand ifTrue: [ ^ true ].

	self class == comparand class ifFalse: [ ^false ].
	^ self offset = comparand offset
		ifTrue: [ self hasEqualTicks: comparand ]
		ifFalse: [ self asUTC ticks = comparand asUTC ticks ]! !


!Duration methodsFor: 'ansi protocol' stamp: 'jmv 5/27/2014 11:22'!
= comparand 
	"Answer whether the argument is a <Duration> representing the same 
	period of time as the receiver."

	self == comparand
		ifTrue: [ ^ true ].

	self species == comparand species 
		ifFalse: [ ^ false ].

	^ self totalNanoSeconds = comparand totalNanoSeconds! !


!LookupKey methodsFor: 'comparing' stamp: 'jmv 5/27/2014 17:05'!
= aLookupKey

	self species == aLookupKey species
		ifFalse: [ ^false ].

	^ key = aLookupKey key! !


!WeakKeyAssociation methodsFor: 'comparing' stamp: 'jmv 5/27/2014 17:07'!
= aLookupKey

	self species == aLookupKey species
		ifFalse: [ ^ false ].

	^self key = aLookupKey key! !


!Fraction methodsFor: 'comparing' stamp: 'jmv 5/27/2014 11:03'!
= aNumber

	"Any object is equal to itself"
	self == aNumber ifTrue: [ ^ true ].

	aNumber isNumber ifFalse: [ ^ false ].
	aNumber isFraction
		ifTrue: [numerator = 0 ifTrue: [^ aNumber numerator = 0].
				^ (numerator * aNumber denominator) =
					(aNumber numerator * denominator)
				"Note: used to just compare num and denom,
					but this fails for improper fractions"].
	^ aNumber adaptToFraction: self andSend: #=! !


!Integer methodsFor: 'comparing' stamp: 'jmv 5/27/2014 11:04'!
= aNumber

	"Any object is equal to itself"
	self == aNumber ifTrue: [ ^ true ].

	aNumber isNumber ifFalse: [^ false].
	aNumber isInteger ifTrue: [
		aNumber class == self class ifFalse: [ ^false ].
		^ (self digitCompare: aNumber) = 0].
	^ aNumber adaptToInteger: self andSend: #=! !


!Time methodsFor: 'ansi protocol' stamp: 'jmv 5/27/2014 11:04'!
= aTime

	"Any object is equal to itself"
	self == aTime ifTrue: [ ^ true ].

	^ [ self ticks = aTime ticks ]
		on: MessageNotUnderstood do: [ false ]! !


!MessageSend methodsFor: 'comparing' stamp: 'jmv 5/27/2014 11:05'!
= anObject

	"Any object is equal to itself"
	self == anObject ifTrue: [ ^ true ].

	^ anObject species == self species 
		and: [receiver == anObject receiver
		and: [selector == anObject selector
		and: [arguments = anObject arguments]]]! !


!KeyboardEvent methodsFor: 'comparing' stamp: 'jmv 5/27/2014 11:36'!
= aMorphicEvent

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class = aMorphicEvent class ifFalse: [ ^false ].

	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	keyValue = aMorphicEvent keyValue ifFalse: [ ^ false ].
	^ true! !


!MouseEvent methodsFor: 'comparing' stamp: 'jmv 5/27/2014 11:35'!
= aMorphicEvent

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class = aMorphicEvent class ifFalse: [ ^ false ].

	type = aMorphicEvent eventType ifFalse: [ ^ false ].
	position = aMorphicEvent eventPosition ifFalse: [ ^ false ].
	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	^ true! !


!MouseMoveEvent methodsFor: 'comparing' stamp: 'jmv 5/27/2014 11:35'!
= aMorphicEvent

	"Any object is equal to itself"
	self == aMorphicEvent ifTrue: [ ^ true ].

	self class = aMorphicEvent class ifFalse: [ ^ false ].

	position = aMorphicEvent eventPosition ifFalse: [ ^ false ].
	buttons = aMorphicEvent buttons ifFalse: [ ^ false ].
	^ true! !


!Point methodsFor: 'comparing' stamp: 'jmv 5/27/2014 17:06'!
= aPoint

	self species == aPoint species ifFalse: [ ^ false ].

	^ x = aPoint x and: [ y = aPoint y ]! !


!Rectangle methodsFor: 'comparing' stamp: 'jmv 5/27/2014 17:06'!
= aRectangle 

	"Answer true if the receiver's species, origin and corner match aRectangle's."
	self species == aRectangle species
		ifFalse: [ ^false ].

	^ origin = aRectangle origin and: [ corner = aRectangle corner ]! !


!CharacterBlock methodsFor: 'comparing' stamp: 'jmv 5/27/2014 17:04'!
= aCharacterBlock

	self species == aCharacterBlock species
		ifFalse: [ ^false ].
		
	^ stringIndex = aCharacterBlock stringIndex! !


!ReadWriteStream methodsFor: 'testing' stamp: 'jmv 5/27/2014 11:14'!
= other

	"Any object is equal to itself"
	self == other ifTrue: [ ^ true ].

	(self class == ReadWriteStream and: [other class == ReadWriteStream]) ifFalse: [
		^ false ].

	^ self position = other position and: [ self contents = other contents ]! !


!TextLine methodsFor: 'comparing' stamp: 'jmv 5/27/2014 17:07'!
= line

	self species == line species
		ifFalse: [ ^ false ].

	^((firstIndex = line first and: [ lastIndex = line last ])
		and: [ internalSpaces = line internalSpaces ])
		and: [ paddingWidth = line paddingWidth ]! !


!WeakMessageSend methodsFor: 'comparing' stamp: 'jmv 5/27/2014 11:16'!
= anObject

	"Any object is equal to itself"
	self == anObject ifTrue: [ ^ true ].

	"Compare equal to equivalent MessageSend"
	^ (anObject is: #MessageSend)
		and: [self receiver == anObject receiver
		and: [selector == anObject selector
		and: [(Array withAll: arguments) = (Array withAll: anObject arguments)]]]
! !

!methodRemoval: MorphicEvent #=!
MorphicEvent removeSelector: #=!
