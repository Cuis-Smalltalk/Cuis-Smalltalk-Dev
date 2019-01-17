'From Cuis 5.0 of 7 November 2016 [latest update: #3564] on 17 January 2019 at 7:00:11 pm'!

!CodePackage methodsFor: 'enumerating' stamp: 'KLG 1/17/2019 18:40:59'!
coreMethodsForFileinOf: aClass do: aBlock
	"Evaluate aBlock with the actual method objects in this package.
	Only enumerate those methods that might be needed for proper filein.
	such as #compilerClass."

	aClass organization categories do: [ :cat |
		(self isForeignClassExtension: cat) ifFalse: [
			(aClass organization listAtCategoryNamed: cat) do: [ :s |
				(self selectorNeededForFilein: s) ifTrue: [
					aBlock value: (self referenceForMethod: s ofClass: aClass) ]]]]! !

!CodePackage methodsFor: 'testing' stamp: 'KLG 1/17/2019 18:43:05'!
selectorNeededForFilein: aSelector
	"Answer true if aSelector might be needed for proper filein of the remaing methods."

	^ Metaclass isScarySelector: aSelector! !

!CodePackage methodsFor: 'saving' stamp: 'KLG 1/17/2019 18:54:50'!
write: classes methodsForFileinOn: aStream

	classes
		do: [ :class |
			self 
				writeCoreMethodsForFileinOf: class class on: aStream;
				writeCoreMethodsForFileinOf: class on: aStream ]
		displayingProgress: 'Saving methods...'! !

!CodePackage methodsFor: 'saving' stamp: 'KLG 1/17/2019 18:56:42'!
writeCoreMethodsForFileinOf: aClass on: aStream

	self coreMethodsForFileinOf: aClass do: [ :methodReference |
		methodReference isValid
			ifTrue: [
				self writeMethod: methodReference on: aStream ]]! !


!CodePackage methodsFor: 'enumerating' stamp: 'KLG 1/17/2019 18:44:19'!
coreMethodsOf: aClass do: aBlock
	"Evaluate aBlock with the actual method objects in this package.
	
	Leave out all the methods needed for filein."

	aClass organization categories do: [ :cat |
		(self isForeignClassExtension: cat) ifFalse: [
			(aClass organization listAtCategoryNamed: cat) do: [ :s |
				(self selectorNeededForFilein: s) ifFalse: [
					aBlock value: (self referenceForMethod: s ofClass: aClass) ]]]]! !

!CodePackage methodsFor: 'saving' stamp: 'KLG 1/17/2019 18:55:00'!
write: classes methodsOn: aStream

	classes
		do: [ :class |
			self 
				writeCoreMethodsOf: class on: aStream;
			 	writeCoreMethodsOf: class class on: aStream ]
		displayingProgress: 'Saving methods...'! !

!CodePackage methodsFor: 'saving' stamp: 'KLG 1/17/2019 18:56:15'!
writeOnStream: aStream

	| sortedClasses |
	aStream
		nextChunkPut: ('Description ', description) printString;
		newLine.
	self writeFeatureSpecOn: aStream.
	self writeSystemCategoriesOn: aStream.

	self codePackageClass ifNotNil: [ :codePackageClass |
		self
			write: {codePackageClass} classDefinitionsOn: aStream;
			write: {codePackageClass} classCommentsOn: aStream;
			write: {codePackageClass} methodsOn: aStream.
		aStream nextChunkPut: codePackageClass name, ' prePackageInstall'; newLine ].

	sortedClasses _ Array streamContents: [ :strm |
		self classesInSafeOrderDo: [ :cls |
			cls == self class ifFalse: [
				strm nextPut: cls ]]].
	self
		write: sortedClasses classDefinitionsOn: aStream;
		write: sortedClasses classCommentsOn: aStream;
		write: sortedClasses methodsForFileinOn: aStream;
		write: sortedClasses methodsOn: aStream.

	self
		sortedExtensionMethodsDo:  [ :methodReference |
			methodReference isValid ifTrue: [
				self writeMethod: methodReference on: aStream ]]
		displayingProgress: 'Saving extension methods...'.
	self write: sortedClasses initializersOn: aStream.

	self codePackageClass ifNotNil: [ :codePackageClass |
		self write: { codePackageClass } initializersOn: aStream.
		aStream nextChunkPut: codePackageClass name, ' postPackageInstall'; newLine ]! !

