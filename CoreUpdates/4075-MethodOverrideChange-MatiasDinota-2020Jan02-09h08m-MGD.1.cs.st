'From Cuis 5.0 [latest update: #3964] on 21 March 2020 at 3:51:01 pm'!
!classDefinition: #ChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ChangeRecord
	instanceVariableNames: 'file position type class category meta stamp prior isTest overrides '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeList methodsFor: 'scanning' stamp: 'MGD 3/17/2020 19:35:38'!
overridenMethodReferenceFrom: tokens

	| overridenMethodReference tagIndex |
	
	tagIndex _ tokens indexOf: #overrides: ifAbsent: [ ^ nil ].
	overridenMethodReference _ tokens at: tagIndex + 1.
	
	^ overridenMethodReference
! !

!ChangeList methodsFor: 'scanning' stamp: 'MGD 3/17/2020 19:37:43'!
scanCategory: category class: class meta: meta stamp: stamp prior: aPriorMethod overrides: anOverridenMethod

	| itemPosition method |

	[
		itemPosition _ file position.
		method _ file nextChunk.
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
					prior: aPriorMethod
					overrides: anOverridenMethod)
				text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
					, (((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: method) ifNil: [''])
					, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp])]! !


!ClassDescription methodsFor: 'fileIn/Out'!
printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod overridesMethod: overridenMethod
	"Print a method category preamble.  This must have a category name.
	It may have an author/date stamp, and it may have a prior source link.
	If it has a prior source link, it MUST have a stamp, even if it is empty."

"The current design is that changeStamps and prior source links are preserved in the changes file.  All fileOuts include changeStamps.  Condensing sources, however, eliminates all stamps (and links, natch)."

	aFileStream newLine; nextPut: $!!.
	aFileStream nextChunkPut: (String streamContents: [ :strm |
		strm nextPutAll: self name; nextPutAll: ' methodsFor: '; print: category asString.
		(changeStamp notNil and: [
			changeStamp size > 0 or: [priorMethod notNil]]) ifTrue: [
			strm nextPutAll: ' stamp: '; print: changeStamp].
		priorMethod notNil ifTrue: [
			strm nextPutAll: ' prior: '; print: priorMethod sourcePointer].
		overridenMethod notNil ifTrue: [
			strm nextPutAll: ' overrides: '; print: overridenMethod sourcePointer]
		]).
! !


!CompiledMethod methodsFor: 'source code management' stamp: 'MGD 3/17/2020 18:12:26'!
putSource: sourceStr fromParseNode: methodNode class: class category: catName
	withStamp: changeStamp inFile: fileIndex priorMethod: priorMethod overridesMethod: overridenMethod

	^ self putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: [ :file |
			class
				printCategoryChunk: catName
				on: file
				withStamp: changeStamp
				priorMethod: priorMethod
				overridesMethod: overridenMethod.
			file newLine ]! !


!ChangeRecord methodsFor: 'access' stamp: 'MGD 3/17/2020 19:40:04'!
overridesASuperclassMethod
	^ overrides notNil ! !

!ChangeRecord methodsFor: 'initialization' stamp: 'MGD 3/17/2020 19:39:04'!
file: aFile position: aPosition type: aType class: aClassName category: aClassCategory meta: isMeta stamp: aStamp prior: aPrior overrides: anOverridenMethod

	self file: aFile position: aPosition type: aType.
	class _ aClassName.
	category _ aClassCategory.
	meta _ isMeta.
	stamp _ aStamp.
	prior _ aPrior.
	overrides _ anOverridenMethod.! !


!ChangeList methodsFor: 'scanning' stamp: 'MGD 3/17/2020 19:35:05'!
scanMethodDefinition: tokens

	| stamp className priorMethod overridenMethod |
	
	className _ tokens first.
	stamp _ self stampFrom: tokens.
	priorMethod _ self priorMethodReferenceFrom: tokens.
	overridenMethod _ self overridenMethodReferenceFrom: tokens.
	
	tokens second == #methodsFor: ifTrue: [
		^ self scanCategory: tokens third class: className meta: false stamp: stamp prior: priorMethod overrides: overridenMethod ].

	tokens third == #methodsFor: ifTrue: [
		^ self scanCategory: tokens fourth class: className meta: true stamp: stamp prior: priorMethod overrides: overridenMethod ].
	
	self error: 'Unsupported method definition' 
! !


!ClassDescription methodsFor: 'private' stamp: 'MGD 3/17/2020 19:08:00'!
logMethodSource: aText forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp notifying: requestor
	| priorMethodOrNil overridenMethodOrNil |
	
	priorMethodOrNil := self compiledMethodAt: aCompiledMethodWithNode selector ifAbsent: nil.
	overridenMethodOrNil := self superclass ifNotNil: [ self superclass lookupSelector: aCompiledMethodWithNode selector ].
	
	aCompiledMethodWithNode method putSource: aText asString
		fromParseNode: aCompiledMethodWithNode node
		class: self category: category withStamp: changeStamp 
		inFile: 2 priorMethod: priorMethodOrNil overridesMethod: overridenMethodOrNil.! !

!methodRemoval: ChangeList #scanCategory:class:meta:stamp:prior: stamp: 'MGD 3/17/2020 19:37:30'!
ChangeList removeSelector: #scanCategory:class:meta:stamp:prior:!
!classDefinition: #ChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ChangeRecord
	instanceVariableNames: 'file position type class category meta stamp prior isTest overrides'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeRecord reorganize!
('access' category changeClass changeClassName changeType compilerClass dateAndTime fileName fileOutOn: isMetaClassChange methodSelector overridesASuperclassMethod prior stamp stamp: string)
('initialization' file:position:type: file:position:type:class:category:meta:stamp: file:position:type:class:category:meta:stamp:prior: file:position:type:class:category:meta:stamp:prior:overrides: fileIn markAsTest:)
('testing' isDoIt isTestClassChange)
('printing' printOn:)
!

