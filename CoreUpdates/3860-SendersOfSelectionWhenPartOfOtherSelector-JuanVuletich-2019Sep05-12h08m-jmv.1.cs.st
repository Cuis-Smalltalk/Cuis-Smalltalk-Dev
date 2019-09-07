'From Cuis 5.0 [latest update: #3859] on 5 September 2019 at 12:09:26 pm'!

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 9/5/2019 11:57:33'!
implementorsOfIt
	"Open an implementors browser.
	If text selection defines a selector, take it. Otherwise, try finding selector under cursor. If this fails, consider the whole line."

	self selectedSelector ifNotNil: [ :selector |
		^ Smalltalk browseAllImplementorsOf: selector ].
	self
		withSelectorUnderCursorDo: [ :selector | Smalltalk browseAllImplementorsOf: selector ]
		otherwise: [ self implementorsOfItWhenErrorsParsing ]! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 9/5/2019 11:58:03'!
sendersOfIt
	"Open a senders browser.
	If text selection defines a selector, take it. Otherwise, try finding selector under cursor. If this fails, consider the whole line."

	self selectedSelector ifNotNil: [ :selector |
		^ Smalltalk browseAllCallsOn: selector ].
	self
		withSelectorUnderCursorDo: [ :selector | Smalltalk browseAllCallsOn: selector ]
		otherwise: [ self sendersOfItWhenErrorsParsing ]! !

