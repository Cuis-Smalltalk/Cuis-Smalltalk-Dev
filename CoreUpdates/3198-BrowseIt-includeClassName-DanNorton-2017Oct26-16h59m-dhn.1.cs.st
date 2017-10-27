'From Cuis 5.0 of 7 November 2016 [latest update: #3197] on 26 October 2017 at 4:59:57 pm'!

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'dhn 10/25/2017 16:48:11'!
browseIt
	"Launch a browser for the current selection, if appropriate"

	| aSymbol anEntry browser |
	Preferences alternativeBrowseIt ifTrue: [^ self browseClassFromIt].

	self wordSelectAndEmptyCheck: [^ self].
	(aSymbol _ self selectedSymbol) ifNil: [^ morph flash].

	aSymbol first isUppercase
		ifTrue: [
			anEntry _ (Smalltalk
				at: aSymbol
				ifAbsent: [
					Smalltalk browseAllImplementorsOf: aSymbol.
					^ nil]).
			anEntry ifNil: [^ morph flash].
			(anEntry isKindOf: Class)
				ifFalse:	[anEntry _ anEntry class].
			browser _ Browser new.
			browser setClass: anEntry selector: nil.
			BrowserWindow open: browser label:'System Browser: ',  aSymbol]
		ifFalse:
			[Smalltalk browseAllImplementorsOf: aSymbol]! !

