'From Cuis7.9 [latest update: #8324] on 5 November 2026 at 2:53:55 pm'!

!Browser methodsFor: 'class functions' stamp: 'jmv 5/Nov/2026, 5:43:10 pm (UTC)'!
copyClass
	| originalClass originalName copysName newDefinition newMetaDefinition newClass |
	selectedClassName ifNil: [^ self].
	originalClass := self selectedClass.
	originalName := originalClass name.
	self request: 'New class name?'
		initialAnswer: originalName
		verifying: [:aString| aString notEmpty and: [aString ~= originalName]]
		do: [:aString|
			copysName := aString asSymbol.
			(Smalltalk includesKey: copysName)
				ifTrue: [self error: copysName , ' already exists'].
			newDefinition := originalClass definition
				copyReplaceAll: originalName printString
				with: copysName printString.
			newClass := Compiler evaluate: newDefinition logged: true.
			newMetaDefinition := originalClass class definition
				copyReplaceAll: originalClass class name
				with: newClass class name.
			Compiler evaluate: newMetaDefinition logged: true.
			newClass copyAllCategoriesFrom: originalClass.
			newClass class copyAllCategoriesFrom: originalClass class.
			originalClass hasComment ifTrue: [newClass classComment: originalClass smalltalkComment].
			self classListIndex: 0.
			self changed: #classList]! !


!CodeFileBrowser methodsFor: 'accessing' stamp: 'jmv 5/Nov/2026, 5:41:53 pm (UTC)'!
contents: input notifying: aRequestor 
	"The retrieved information has changed and its source must now be 
	updated. The information can be a variety of things, depending on the 
	list selections (such as templates for class or message definition, methods) 
	or the user menu commands (such as definition, comment, hierarchy). 
	Answer the result of updating the source."

	| codeString theClass |
	codeString := input string.

	editSelection == #editComment 
		ifTrue: [
			theClass := self selectedClass.
			theClass ifNil: [
				self inform: 'You must select a class
before giving it a comment.'.
				^ false ].
			theClass classComment: codeString. 
			^ true ].

	self inform:'You cannot change the current selection'.
	^false! !

!methodRemoval: PseudoClass #comment: stamp: 'jmv 5/Nov/2026, 5:53:30 pm (UTC)'!
PseudoClass removeSelector: #comment:!
!methodRemoval: ClassDescription #comment: stamp: 'jmv 5/Nov/2026, 5:53:30 pm (UTC)'!
ClassDescription removeSelector: #comment:!
