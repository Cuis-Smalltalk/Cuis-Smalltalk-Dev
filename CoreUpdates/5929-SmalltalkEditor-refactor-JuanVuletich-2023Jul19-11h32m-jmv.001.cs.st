'From Cuis 6.0 [latest update: #5924] on 19 July 2023 at 11:56:24 am'!

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 7/19/2023 11:53:01'!
browseIt
	"Launch a browser for the current selection, if appropriate"

	| aSymbol anEntry |
	(Preferences at: #alternativeBrowseIt) ifTrue: [^ self browseClassFromIt].

	self wordSelectAndEmptyCheck: [^ self].
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
			BrowserWindow fullOnClass: anEntry selector: nil]
		ifFalse:
			[Smalltalk browseAllImplementorsOf: aSymbol]! !

