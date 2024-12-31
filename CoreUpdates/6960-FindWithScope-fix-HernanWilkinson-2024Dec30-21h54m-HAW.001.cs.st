'From Cuis7.3 [latest update: #6959] on 30 December 2024 at 9:56:55 pm'!

!CodeProvider methodsFor: 'testing' stamp: 'jmv 12/30/2024 21:54:06'!
isEditingExistingMethod

	^self isEditingMethod! !


!Browser methodsFor: 'testing' stamp: 'jmv 12/30/2024 21:54:13'!
isEditingExistingMethod

	^editSelection = #editMessage! !


!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'HAW 12/30/2024 21:56:51'!
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
			package := self codeProvider isEditingExistingMethod
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

