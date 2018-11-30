'From Cuis 5.0 of 7 November 2016 [latest update: #3510] on 30 November 2018 at 9:29:13 am'!

!Editor methodsFor: 'new selection' stamp: 'jmv 11/30/2018 09:26:44'!
selectWordLeftDelimiters: leftDelimiters rightDelimiters: rightDelimiters
	"Select delimited text or word--the result of double-clicking."

	| initialDelimiter finalDelimiter direction match level string here hereChar start stop |
	string _ self privateCurrentString.
	string size < 2 ifTrue: [^self selectFrom: 1 to: 1].
	here _ self pointIndex min: string size max: 2.
	initialDelimiter _ string at: here - 1.
	match _ leftDelimiters indexOf: initialDelimiter.
	match > 0
		ifTrue: [
			"delimiter is on left -- match to the right"
			start _ here.
			direction _ 1.
			here _ here - 1.
			finalDelimiter _ rightDelimiters at: match]
		ifFalse: [
			initialDelimiter _ string at: here.
			match _ rightDelimiters indexOf: initialDelimiter.
			match > 0
				ifTrue: [
					"delimiter is on right -- match to the left"
					stop _ here - 1.
					direction _ -1.
					finalDelimiter _ leftDelimiters at: match]
				ifFalse: [
					"no delimiters -- select a token"
					direction _ -1]].
	level _ 1.
	[level > 0 and: [direction > 0
			ifTrue: [here < string size]
			ifFalse: [here > 1]]]
		whileTrue: [
			hereChar _ string at: (here _ here + direction).
			match = 0
				ifTrue: [
					"token scan goes left, then right"
					hereChar isValidInIdentifiers 	"Consider $: as a word separator"
						ifTrue: [
							here = 1
								ifTrue: [
									start _ 1.
									"go right if hit string start"
									direction _ 1]]
						ifFalse: [
							direction < 0
								ifTrue: [
									start _ here + 1.
									"go right if hit non-token"
									direction _ 1]
								ifFalse: [level _ 0]]]
				ifFalse: [
					"delimiter match just counts nesting level"
					hereChar = finalDelimiter
						ifTrue: [level _ level - 1"leaving nest"]
						ifFalse: [
							hereChar = initialDelimiter 
								ifTrue: [level _ level + 1"entering deeper nest"]]]].
	level > 0 ifTrue: [
		"If we failed to find final delimiter, then just select word."
		^self selectWordLeftDelimiters: '' rightDelimiters: '' ].
	direction > 0
		ifTrue: [
			"If a word ends with $: (a keyword), consider it part of the word"
			hereChar = $: ifTrue: [here _ here + 1].
			self selectFrom: start to: here - 1]
		ifFalse: [
			"But if a word starts with $: (this is the argument to a keyword), then it is not part of the word."
			self selectFrom: here + 1 to: stop]! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'HAW 11/29/2018 19:11:47'!
browseIt
	"Launch a browser for the current selection, if appropriate"

	| aSymbol anEntry browser |
	Preferences alternativeBrowseIt ifTrue: [^ self browseClassFromIt].

	self wordSelectAndEmptyCheck: [^ self].
	aSymbol _ self selectedSymbol ifNil: [
		self
			evaluateSelectionAndDo: [ :result | result class name ]
			ifFail: [ ^morph flash ]
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

