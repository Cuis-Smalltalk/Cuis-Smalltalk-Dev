'From Cuis7.5 [latest update: #7431] on 20 August 2025 at 4:50:26 pm'!
!classDefinition: #ChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ChangeRecord
	instanceVariableNames: 'file position type class category meta stamp prior overrides '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeList methodsFor: 'scanning' stamp: 'jmv 8/20/2025 16:47:35'!
scanCategory: category class: class meta: meta stamp: stamp prior: aPriorMethod

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
					prior: aPriorMethod)
				text: 'method: ' , class , (meta ifTrue: [' class '] ifFalse: [' '])
					, (((Smalltalk at: class ifAbsent: [Object class]) parserClass selectorFrom: method) ifNil: [''])
					, (stamp isEmpty ifTrue: [''] ifFalse: ['; ' , stamp])]! !


!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 8/20/2025 16:49:06'!
putSource: sourceStr fromParseNode: methodNode class: class category: catName
	withStamp: changeStamp inFile: fileIndex priorMethod: priorMethod

	^ self putSource: sourceStr fromParseNode: methodNode inFile: fileIndex withPreamble: [ :file |
			class
				printCategoryChunk: catName
				on: file
				withStamp: changeStamp
				priorMethod: priorMethod.
			file newLine ]! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 8/20/2025 16:48:04'!
scanMethodDefinition: tokens

	| stamp className priorMethod |
	
	className := tokens first.
	stamp := CompiledMethod stampFrom: tokens.
	priorMethod := CompiledMethod priorReferenceFrom: tokens.
	
	tokens second == #methodsFor: ifTrue: [
		^ self scanCategory: tokens third class: className meta: false stamp: stamp prior: priorMethod ].

	tokens third == #methodsFor: ifTrue: [
		^ self scanCategory: tokens fourth class: className meta: true stamp: stamp prior: priorMethod ].
	
	self error: 'Unsupported method definition' 
! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 8/20/2025 16:48:47'!
printCategoryChunk: category on: aFileStream withStamp: changeStamp priorMethod: priorMethod
	"Print a method category preamble.  This must have a category name.
	It may have an author/date stamp, and it may have a prior source link.
	If it has a prior source link, it MUST have a stamp, even if it is empty."

"The current design is that changeStamps and prior source links are preserved in the changes file.  All fileOuts include changeStamps.  Condensing sources, however, eliminates all stamps (and links, natch)."

	aFileStream newLine; nextPut: $!!.
	aFileStream nextChunkPut: (String streamContents: [ :strm |
		strm nextPutAll: self name; nextPutAll: ' methodsFor: '; print: category asString.
		(changeStamp notNil and: [
			changeStamp notEmpty or: [priorMethod notNil]]) ifTrue: [
			strm nextPutAll: ' stamp: '; print: changeStamp].
		priorMethod notNil ifTrue: [
			strm nextPutAll: ' prior: '; print: priorMethod sourcePointer].
		]).
! !

!ClassDescription methodsFor: 'private' stamp: 'jmv 8/20/2025 16:49:19'!
logMethodSource: code forMethodWithNode: aCompiledMethodWithNode inCategory: category withStamp: changeStamp notifying: requestor
	| priorMethodOrNil |

	priorMethodOrNil := self compiledMethodAt: aCompiledMethodWithNode selector ifAbsent: nil.

	aCompiledMethodWithNode method
		putSource: code
		fromParseNode: aCompiledMethodWithNode node
		class: self 
		category: category 
		withStamp: changeStamp 
		inFile: 2 
		priorMethod: priorMethodOrNil.! !

!methodRemoval: ChangeRecord #file:position:type:class:category:meta:stamp:prior:overrides: stamp: 'jmv 8/20/2025 16:47:17'!
ChangeRecord removeSelector: #file:position:type:class:category:meta:stamp:prior:overrides:!
!methodRemoval: ChangeRecord #overridesASuperclassMethod stamp: 'jmv 8/20/2025 16:46:46'!
ChangeRecord removeSelector: #overridesASuperclassMethod!
!methodRemoval: CompiledMethod class #overridenMethodReferenceFrom: stamp: 'jmv 8/20/2025 16:48:26'!
CompiledMethod class removeSelector: #overridenMethodReferenceFrom:!
!methodRemoval: CompiledMethod #putSource:fromParseNode:class:category:withStamp:inFile:priorMethod:overridesMethod: stamp: 'jmv 8/20/2025 16:49:24'!
CompiledMethod removeSelector: #putSource:fromParseNode:class:category:withStamp:inFile:priorMethod:overridesMethod:!
!methodRemoval: ClassDescription #printCategoryChunk:on:withStamp:priorMethod:overridesMethod: stamp: 'jmv 8/20/2025 16:48:57'!
ClassDescription removeSelector: #printCategoryChunk:on:withStamp:priorMethod:overridesMethod:!
!methodRemoval: ChangeList #scanCategory:class:meta:stamp:prior:overrides: stamp: 'jmv 8/20/2025 16:47:55'!
ChangeList removeSelector: #scanCategory:class:meta:stamp:prior:overrides:!
!classDefinition: #ChangeRecord category: #'Tools-Changes'!
ChangeListElement subclass: #ChangeRecord
	instanceVariableNames: 'file position type class category meta stamp prior'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Changes'!
