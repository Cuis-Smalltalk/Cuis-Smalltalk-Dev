'From Cuis7.1 [latest update: #6824] on 19 November 2024 at 5:28:34 pm'!

!Editor methodsFor: 'private' stamp: 'jmv 11/19/2024 17:20:19'!
wordSelectAndEmptyCheck: returnBlock
	"If the current selection is empty, expand it to be the entire current word; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this."

	"if current selection is empty, then first select the entire word in which occurs before proceeding"
	self hasSelection ifFalse: [
		self selectWord ].
	self selectionIsEmptyOrSeparators ifTrue: [
		^ returnBlock value ].! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 11/19/2024 17:27:43'!
browseClassFromIt
	"Launch a hierarchy browser for the class indicated by the current selection.  If multiple classes matching the selection exist, let the user choose among them."

	| aClass |
	self wordSelectAndEmptyCheck: [
		^ Smalltalk browseHierarchy: Object selector: nil ].

	aClass := Utilities
		classFromPattern: self selectedString withBlanksCondensed
		withCaption: 'choose a class to browse...'.
	aClass ifNil: [^ morph flash ].

	Smalltalk
		browseHierarchy: aClass
		selector: nil.! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 11/19/2024 17:27:48'!
browseIt
	"Launch a browser for the current selection, if appropriate"

	| aSymbol anEntry |
	(Preferences at: #alternativeBrowseIt) ifTrue: [^ self browseClassFromIt].

	self wordSelectAndEmptyCheck: [
		^ Smalltalk browseFull: Object selector: nil ].

	aSymbol := self selectedSymbol ifNil: [
		self
			evaluate: self selection string
			andDo: [ :result | result class name ]
			ifFail: [ ^morph flash ]
			profiled: false ].

	aSymbol first isUppercase
		ifTrue: [
			anEntry := (Smalltalk
				at: aSymbol
				ifAbsent: [
					Smalltalk browseAllImplementorsOf: aSymbol.
					^ nil]).
			anEntry ifNil: [^ morph flash].
			(anEntry isKindOf: Class)
				ifFalse: [anEntry := anEntry class].
			Smalltalk browseFull: anEntry selector: nil ]
		ifFalse: [
			Smalltalk browseAllImplementorsOf: aSymbol ].! !

!SmalltalkEditor methodsFor: 'private' stamp: 'jmv 11/19/2024 17:20:48'!
lineOrNodeSelectAndEmptyCheck: returnBlock
	"If the current selection is empty, either
		- expand it to be the entire current line
		- or expand it to a meaningful chunk of code.
	If after that's done the selection is still empty, then evaluate the returnBlock."

	self hasSelection ifFalse: [
		(model actualContents isLineEndAt: self pointIndex)
			ifTrue: [ self selectLine ]
			ifFalse: [
				self
					withMethodNodeAndClassDo: [ :methodNode :selectedClass |
						self selectNodeUnderCursorForInspectionIn: methodNode ]
					ifErrorsParsing: [ :anError | ]]].
	self selectionIsEmptyOrSeparators ifTrue: [
		self selectLine ].! !

