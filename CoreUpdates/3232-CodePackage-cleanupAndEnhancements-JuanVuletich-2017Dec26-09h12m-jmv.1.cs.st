'From Cuis 5.0 of 7 November 2016 [latest update: #3231] on 26 December 2017 at 9:21:34 am'!

!CodePackage methodsFor: 'enumerating' stamp: 'jmv 12/26/2017 08:36:49'!
coreMethodsOf: aClass do: aBlock
	"Evaluate aBlock with the actual method objects in this package."

	aClass organization categories do: [ :cat |
		(self isForeignClassExtension: cat) ifFalse: [
			(aClass organization listAtCategoryNamed: cat) do: [ :s |
				aBlock value: (self referenceForMethod: s ofClass: aClass) ]]]! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 12/26/2017 08:39:58'!
writeCoreMethodsOf: aClass on: aStream

	self coreMethodsOf: aClass do: [ :methodReference |
		methodReference isValid
			ifTrue: [
				self writeMethod: methodReference on: aStream ]]! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 12/26/2017 08:52:22'!
write: classes methodsOn: aStream

	classes
		do: [ :class |
			self writeCoreMethodsOf: class on: aStream.
			self writeCoreMethodsOf: class class on: aStream ]
		displayingProgress: 'Saving methods...'! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 12/26/2017 09:07:44'!
writeOnStream: aStream

	| sortedClasses |
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
	self
		sortedExtensionMethodsDo:  [ :methodReference |
			methodReference isValid ifTrue: [
				self writeMethod: methodReference on: aStream ]]
		displayingProgress: 'Saving extension methods...'.
	self
		write: sortedClasses initializersOn: aStream! !

!methodRemoval: CodePackage #actualMethodsDo:!
CodePackage removeSelector: #actualMethodsDo:!
!methodRemoval: CodePackage #addCoreMethod:!
CodePackage removeSelector: #addCoreMethod:!
!methodRemoval: CodePackage #addExtensionMethod:!
CodePackage removeSelector: #addExtensionMethod:!
!methodRemoval: CodePackage #addMethod:!
CodePackage removeSelector: #addMethod:!
!methodRemoval: CodePackage #allOverriddenMethods!
CodePackage removeSelector: #allOverriddenMethods!
!methodRemoval: CodePackage #allOverriddenMethodsDo:!
CodePackage removeSelector: #allOverriddenMethodsDo:!
!methodRemoval: CodePackage #baseCategoryOfMethod:!
CodePackage removeSelector: #baseCategoryOfMethod:!
!methodRemoval: CodePackage #changeRecordForOverriddenMethod:!
CodePackage removeSelector: #changeRecordForOverriddenMethod:!
!methodRemoval: CodePackage #coreCategoriesForClass:!
CodePackage removeSelector: #coreCategoriesForClass:!
!methodRemoval: CodePackage #extensionClasses!
CodePackage removeSelector: #extensionClasses!
!methodRemoval: CodePackage #externalCallers!
CodePackage removeSelector: #externalCallers!
!methodRemoval: CodePackage #externalRefsSelect:thenCollect:!
CodePackage removeSelector: #externalRefsSelect:thenCollect:!
!methodRemoval: CodePackage #externalSubclasses!
CodePackage removeSelector: #externalSubclasses!
!methodRemoval: CodePackage #externalUsers!
CodePackage removeSelector: #externalUsers!
!methodRemoval: CodePackage #foreignClasses!
CodePackage removeSelector: #foreignClasses!
!methodRemoval: CodePackage #foreignSystemCategories!
CodePackage removeSelector: #foreignSystemCategories!
!methodRemoval: CodePackage #includesChangeRecord:!
CodePackage removeSelector: #includesChangeRecord:!
!methodRemoval: CodePackage #includesClassNamed:!
CodePackage removeSelector: #includesClassNamed:!
!methodRemoval: CodePackage #includesMethodCategory:ofClassNamed:!
CodePackage removeSelector: #includesMethodCategory:ofClassNamed:!
!methodRemoval: CodePackage #isOverrideCategory:!
CodePackage removeSelector: #isOverrideCategory:!
!methodRemoval: CodePackage #isOverrideMethod:!
CodePackage removeSelector: #isOverrideMethod:!
!methodRemoval: CodePackage #isOverrideOfYourMethod:!
CodePackage removeSelector: #isOverrideOfYourMethod:!
!methodRemoval: CodePackage #overriddenMethods!
CodePackage removeSelector: #overriddenMethods!
!methodRemoval: CodePackage #overriddenMethodsDo:!
CodePackage removeSelector: #overriddenMethodsDo:!
!methodRemoval: CodePackage #overriddenMethodsInClass:!
CodePackage removeSelector: #overriddenMethodsInClass:!
!methodRemoval: CodePackage #overriddenMethodsInClass:do:!
CodePackage removeSelector: #overriddenMethodsInClass:do:!
!methodRemoval: CodePackage #overrideCategoriesForClass:!
CodePackage removeSelector: #overrideCategoriesForClass:!
!methodRemoval: CodePackage #overrideCategoriesForClass:do:!
CodePackage removeSelector: #overrideCategoriesForClass:do:!
!methodRemoval: CodePackage #overrideMethods!
CodePackage removeSelector: #overrideMethods!
!methodRemoval: CodePackage #writeMethodsOf:on:!
CodePackage removeSelector: #writeMethodsOf:on:!
