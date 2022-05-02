'From Cuis 6.0 [latest update: #5129] on 2 May 2022 at 1:16:16 pm'!

!CodePackage methodsFor: 'enumerating' stamp: 'jmv 5/2/2022 13:12:41'!
coreMethodsOf: aClass do: aBlock
	"Evaluate aBlock with the actual method objects in this package."

	aClass organization categories do: [ :cat |
		(self isForeignClassExtension: cat) ifFalse: [
			(aClass organization listAtCategoryNamed: cat) do: [ :s |
				aBlock value: (self referenceForMethod: s ofClass: aClass) ]]].! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 5/2/2022 13:14:07'!
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

!methodRemoval: CodePackage #selectorNeededForFilein: stamp: 'jmv 5/2/2022 13:14:41'!
CodePackage removeSelector: #selectorNeededForFilein:!
!methodRemoval: CodePackage #coreMethodsForFileinOf:do: stamp: 'jmv 5/2/2022 13:14:35'!
CodePackage removeSelector: #coreMethodsForFileinOf:do:!
!methodRemoval: CodePackage #write:methodsForFileinOn: stamp: 'jmv 5/2/2022 13:14:25'!
CodePackage removeSelector: #write:methodsForFileinOn:!
!methodRemoval: CodePackage #writeCoreMethodsForFileinOf:on: stamp: 'jmv 5/2/2022 13:14:30'!
CodePackage removeSelector: #writeCoreMethodsForFileinOf:on:!
