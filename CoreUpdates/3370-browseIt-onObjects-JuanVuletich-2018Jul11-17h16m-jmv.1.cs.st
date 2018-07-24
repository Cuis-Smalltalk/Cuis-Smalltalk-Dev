'From Cuis 5.0 of 7 November 2016 [latest update: #3369] on 11 July 2018 at 5:18:12 pm'!

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 7/11/2018 17:16:50'!
browseIt
	"Launch a browser for the current selection, if appropriate"

	| aSymbol anEntry browser |
	Preferences alternativeBrowseIt ifTrue: [^ self browseClassFromIt].

	self wordSelectAndEmptyCheck: [^ self].
	aSymbol _ self selectedSymbol ifNil: [
		self
			evaluateSelectionAndDo: [ :result | result class name ]
			ifFail: [ morph flash ]
			profiled: false].

	aSymbol first isUppercase
		ifTrue: [
			anEntry _ (Smalltalk
				at: aSymbol
				ifAbsent: [
					Smalltalk browseAllImplementorsOf: aSymbol.
					^ nil]).
			anEntry ifNil: [^ morph flash].
			(anEntry isKindOf: Class)
				ifFalse: [anEntry _ anEntry class].
			browser _ Browser new.
			browser setClass: anEntry selector: nil.
			BrowserWindow open: browser label:'System Browser: ',  aSymbol]
		ifFalse:
			[Smalltalk browseAllImplementorsOf: aSymbol]! !

