'From Cuis7.3 [latest update: #6957] on 30 December 2024 at 1:06:16 pm'!

!SystemDictionary methodsFor: 'browsing' stamp: 'hlsf 12/30/2024 12:55:18'!
browseMethodsWithSourceString: aString in: aPackage
	" Browse all methods in aPackage whose source code contains aString as a substring."

	| matchingMethods |
	matchingMethods := Set new.
	aPackage classesDo: [ :aPackageClass |
		(aPackageClass organization classComment
			findString: aString
			startingAt: 1
			caseSensitive: false) > 0 ifTrue: [
				matchingMethods add: (MethodReference class: aPackageClass selector: #Comment) ]].
	aPackage methods do: [ :methodReference |
		(methodReference sourceCode 
			findString: aString
			startingAt: 1
			caseSensitive: false) > 0 ifTrue: [
				matchingMethods add: methodReference ]].
		
	^ self 
		browseMessageList: matchingMethods asArray sort
		name: 'Methods containing ' , aString printString, ' in package: ', aPackage packageName
		autoHighlight: aString
		allOccurrences: true.! !


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'hlsf 12/30/2024 12:57:55'!
findMethodSourceContainingIt
	"Open a browser on methods which contain the current selection in their source (case-sensitive full-text search of source).  Slow!!"
	| searchString selection scopeClass scopeClassName includeSubclasses includeSuperclasses labelList package |
	
	searchString := StringRequestMorph
		request: 'Enter search text:'
		initialAnswer: self selectedString
		orCancel: [^self].
	
	scopeClass := self codeProvider selectedClass.
	scopeClassName := scopeClass name asString.
	includeSubclasses := false.
	includeSuperclasses := false.

	labelList := {
		'This method'. 
		'All methods in image'. } asOrderedCollection.
	(scopeClass notNil)
		ifTrue: [
			labelList  addAll: {
				'Class ', scopeClassName printString.
				'Class ', scopeClassName printString, ' and subclasses'.
				'Class ', scopeClassName printString, ', subclasses, and superclasses'.}.
			package := self codeProvider isEditingMethod
				ifTrue: [CodePackage packageOfMethod: self codeProvider currentCompiledMethod asMethodReference ifNone: []]
				ifFalse: [CodePackage packageOfClass: scopeClass ifNone: []].
			package ifNotNil: [labelList add: 'In my package: ', package packageName]].
				
	selection := (SelectionMenu labelList: labelList selections: nil) startUpWithCaption: 'Search Scope'.

	(selection = 0) ifTrue:[^self].
	(selection = 1) "This method"
		ifTrue:[^self find: searchString].
	(selection = 2) "All methods in image"
		ifTrue:[scopeClass := nil].
	"(selection = 3) ifTrue:[]. ""Class only - nothing to do"
	(selection = 4) "Class and subclasses"
		ifTrue:[includeSubclasses := true].
	(selection = 5) "Class, subclasses, and superclasses"
		ifTrue:[
			includeSubclasses := true.
			includeSuperclasses := true].
	(selection = 6) "In my package if any "
		ifTrue: [^ Smalltalk browseMethodsWithSourceString: searchString in: package].
	Smalltalk browseMethodsWithSourceString: searchString scopeClass: scopeClass includeSubclasses: includeSubclasses includeSuperclasses: includeSuperclasses! !

