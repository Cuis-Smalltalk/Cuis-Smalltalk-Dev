'From Cuis7.9 [latest update: #8122] on 31 July 2026 at 12:46:20 pm'!
!classDefinition: #ClassCategoryReader category: #'Kernel-Classes'!
Object subclass: #ClassCategoryReader
	instanceVariableNames: 'class category changeStamp libraryName '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Classes'!

!ClassCategoryReader methodsFor: 'fileIn/Out' stamp: 'jmv 31/Jul/2026, 3:45:07 pm (UTC)'!
scanFrom: aStream
	"File in methods from the stream, aStream.
	Convert line endings to Cuis convention."

	| methodSource methodProperties |
	[
		methodProperties := nil.
		aStream skipSeparators.
		(aStream peekFor: $!!) ifTrue: [ | keyword additionalChunk |
			keyword := aStream nextKeyword.
			keyword = '' ifTrue: [ ^self ]. "done when double terminators"
			additionalChunk := aStream nextChunk.
			keyword = 'methodProperties:' ifTrue: [
				methodProperties := Compiler evaluate: additionalChunk ]].
		methodSource := aStream nextChunk withCuisLineEndings.
		methodSource = '' ifTrue: [ ^self ]. "done when double terminators"
		class
			compile: methodSource
			classified: category
			withStamp: changeStamp
			methodProperties: methodProperties
			notifying: nil
	] repeat.! !

!methodRemoval: ClassCategoryReader #setClass:category:changeStamp:libraryName: stamp: 'jmv 31/Jul/2026, 3:45:44 pm (UTC)'!
ClassCategoryReader removeSelector: #setClass:category:changeStamp:libraryName:!
!methodRemoval: ClassDescription #methodsFor:library:stamp: stamp: 'jmv 31/Jul/2026, 3:45:34 pm (UTC)'!
ClassDescription removeSelector: #methodsFor:library:stamp:!
!methodRemoval: ClassDescription #methodsFor:library:stamp:prior: stamp: 'jmv 31/Jul/2026, 3:45:39 pm (UTC)'!
ClassDescription removeSelector: #methodsFor:library:stamp:prior:!
!classDefinition: #ClassCategoryReader category: #'Kernel-Classes'!
Object subclass: #ClassCategoryReader
	instanceVariableNames: 'class category changeStamp'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Classes'!
