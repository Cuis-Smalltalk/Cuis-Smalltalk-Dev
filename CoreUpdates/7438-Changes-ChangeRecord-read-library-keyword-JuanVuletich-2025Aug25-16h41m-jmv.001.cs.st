'From Cuis7.5 [latest update: #7437] on 25 August 2025 at 4:46:01 pm'!
!classDefinition: #ChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ChangeRecord
	instanceVariableNames: 'file position type class category meta stamp prior libraryName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 8/21/2025 12:22:12'!
scanCategory: category class: class meta: meta stamp: stamp prior: priorVersionPosition libraryName: aStringOrNil

	| itemPosition method |

	[
		itemPosition := file position.
		method := file nextChunk.
		method notEmpty ] whileTrue: [ "done when double terminators"
			self
				addItem: (ChangeRecord new
					file: file 
					position: itemPosition 
					type: #method
					class: class 
					category: category 
					meta: meta 
					stamp: stamp
					prior: priorVersionPosition
					libraryName: aStringOrNil)
				text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
					, (((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: method) ifNil: [''])
					, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp])]! !


!CompiledMethod class methodsFor: 'source code management aux' stamp: 'jmv 8/21/2025 10:39:48'!
libraryNameFrom: tokens

	^ self field: #library: from: tokens ifAbsentOrNil: nil! !


!ChangeRecord methodsFor: 'initialization' stamp: 'jmv 8/21/2025 10:41:43'!
file: aFile position: aPosition type: aType class: aClassName category: aClassCategory meta: isMeta stamp: aStamp prior: aPrior libraryName: aString

	self file: aFile position: aPosition type: aType.
	class := aClassName.
	category := aClassCategory.
	meta := isMeta.
	stamp := aStamp.
	prior := aPrior.
	libraryName := aString.! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 8/21/2025 10:45:22'!
scanMethodDefinition: tokens

	| stamp className priorVersionPosition libraryName |
	
	className := tokens first.
	stamp := CompiledMethod stampFrom: tokens.
	priorVersionPosition := CompiledMethod priorReferenceFrom: tokens.
	libraryName := CompiledMethod libraryNameFrom: tokens.
	
	tokens second == #methodsFor: ifTrue: [
		^ self scanCategory: tokens third class: className meta: false stamp: stamp prior: priorVersionPosition libraryName: libraryName ].

	tokens third == #methodsFor: ifTrue: [
		^ self scanCategory: tokens fourth class: className meta: true stamp: stamp prior: priorVersionPosition libraryName: libraryName ].
	
	self error: 'Unsupported method definition' 
! !

!ChangeList methodsFor: 'scanning' stamp: 'jmv 8/25/2025 16:44:00'!
scanSpecificChangeRecordType
	"Scan anything that involves more than one chunk"

	| itemPosition item tokens firstToken secondToken |

	itemPosition := file position.
	item := file nextChunk.

	(self itemIsRecognized: item) ifFalse: [
		"Maybe a preamble, but not one we recognize; bail out with the preamble trick"
		^ self scanAndIgnore: item in: itemPosition ].

	tokens := Scanner new scanTokens: item.
	tokens size >= 2 ifTrue: [
		firstToken := tokens first.
		secondToken := tokens second.

		firstToken == #classDefinition:
			ifTrue: [ ^ self scanClassDefinition: tokens ].
		(firstToken == #classRemoval: or: [ firstToken == #classMoveToSomePackage: ])
			ifTrue: [ ^ self scanClassRemoval: tokens ].
		(firstToken == #methodRemoval: or: [ firstToken == #methodMoveToSomePackage: ])
			ifTrue: [ ^ self scanMethodRemoval: tokens ].
		(secondToken == #methodsFor: or: [ 
			tokens size > 2 and: [ tokens third == #methodsFor: ] ])
			ifTrue: [ ^ self scanMethodDefinition: tokens ].
		secondToken == #commentStamp:
			ifTrue: [ ^ self scanClassComment: tokens ].
		firstToken == #provides:
			ifTrue: [ ^ self scanFeatureProvision: tokens ].
		firstToken == #requires:
			ifTrue: [ ^ self scanFeatureRequirement: tokens ].
		firstToken == #classRenamed:
			ifTrue: [ ^ self scanClassRenamed: tokens ].
		]! !


!ChangeRecord methodsFor: 'initialization' stamp: 'jmv 8/21/2025 10:42:59'!
file: aFile position: aPosition type: aType class: aClassName category: aClassCategory meta: isMeta stamp: aStamp

	self 
		file: aFile 
		position: aPosition 
		type: aType 
		class: aClassName 
		category: aClassCategory 
		meta: isMeta 
		stamp: aStamp 
		prior: nil
		libraryName: nil.
! !

!methodRemoval: ChangeRecord #file:position:type:class:category:meta:stamp:prior: stamp: 'jmv 8/25/2025 16:43:21'!
ChangeRecord removeSelector: #file:position:type:class:category:meta:stamp:prior:!
!methodRemoval: ChangeList #scanCategory:class:meta:stamp:prior: stamp: 'jmv 8/25/2025 16:43:00'!
ChangeList removeSelector: #scanCategory:class:meta:stamp:prior:!
!classDefinition: #ChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ChangeRecord
	instanceVariableNames: 'file position type class category meta stamp prior libraryName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
