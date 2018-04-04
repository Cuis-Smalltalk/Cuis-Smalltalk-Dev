'From Cuis 5.0 of 7 November 2016 [latest update: #3277] on 27 March 2018 at 10:42:25 am'!

!CodePackage methodsFor: 'saving' stamp: 'jmv 3/27/2018 10:31:21'!
writeInitializerExtensionMethods: extensionInitializers on: aStream
	"Write the call to package initialization methods in clases not defined in the
	package (usually, classes in base system that requires specific init 
	of stuff that is package extensions)"

	extensionInitializers do: [ :methodReference |
		aStream nextChunkPut: methodReference classSymbol asString, ' ' , methodReference selector asString; newLine ]! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 3/27/2018 10:20:51'!
write: classes initializersOn: aStream
	"Write the call to #initialize method of classes defined in us."

	Smalltalk hierarchySorted: classes do: [ :class |
		(class class includesSelector: #initialize) ifTrue: [
			aStream nextChunkPut: class name, ' initialize'; newLine ]]! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 3/27/2018 10:40:34'!
writeOnStream: aStream

	| sortedClasses initExtensions |
	aStream
		nextChunkPut: ('Description ', description) printString;
		newLine.
	self writeFeatureSpecOn: aStream.
	self writeSystemCategoriesOn: aStream.
	sortedClasses _ Array streamContents: [ :strm |
		self classesInSafeOrderDo: [ :cls |
			strm nextPut: cls ]].
	self
		write: sortedClasses classDefinitionsOn: aStream;
		write: sortedClasses classCommentsOn: aStream;
		write: sortedClasses methodsOn: aStream.
	initExtensions _ OrderedCollection new.
	self
		sortedExtensionMethodsDo:  [ :methodReference |
			methodReference isValid ifTrue: [
				self writeMethod: methodReference on: aStream.
				(methodReference selector beginsWith: 'initialize') 
					ifTrue: [ initExtensions add: methodReference ]]]
		displayingProgress: 'Saving extension methods...'.
	self write: sortedClasses initializersOn: aStream.
	self writeInitializerExtensionMethods: initExtensions on: aStream! !

