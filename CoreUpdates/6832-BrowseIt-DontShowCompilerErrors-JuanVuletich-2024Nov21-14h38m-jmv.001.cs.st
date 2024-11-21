'From Cuis7.1 [latest update: #6831] on 21 November 2024 at 2:44:50 pm'!

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 11/21/2024 14:39:25'!
browseIt
	"Launch a browser for the current selection, if appropriate"

	| aSymbol anEntry |
	(Preferences at: #alternativeBrowseIt) ifTrue: [^ self browseClassFromIt].

	self wordSelectAndEmptyCheck: [
		^ Smalltalk browseFull: Object selector: nil ].

	aSymbol := self selectedSymbol ifNil: [
		[
			self
				evaluate: self selection string
				andDo: [ :result | result class name ]
				ifFail: [ ^morph flash ]
				profiled: false
		] on: ParserNotification do: [ ^morph flash ]].

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

