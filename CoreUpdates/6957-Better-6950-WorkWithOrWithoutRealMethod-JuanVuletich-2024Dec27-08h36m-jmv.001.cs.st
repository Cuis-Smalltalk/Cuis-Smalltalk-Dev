'From Cuis7.3 [latest update: #6950] on 27 December 2024 at 8:51:10 am'!

!CodeWindow methodsFor: 'misc' stamp: 'len 5/20/2020 08:09:08'!
getSelectorAndSendQuery: querySelector to: queryPerformer with: queryArgs
	"Obtain a selector relevant to the current context, and then send the querySelector to the queryPerformer with the selector obtained and queryArgs as its arguments.  If no message is currently selected, then obtain a method name from a user type-in"

	| strm array |
	strm := WriteStream on: (array := Array new: queryArgs size + 1).
	strm nextPut: nil.
	strm nextPutAll: queryArgs.

	model selectedMessageName ifNil: [ | selector |
		selector := self request: 'Type selector:' initialAnswer: 'flag:'.
		^ selector isEmpty ifFalse: [
			(Symbol hasInterned: selector
				ifTrue: [ :aSymbol |
					array at: 1 put: aSymbol.
					queryPerformer perform: querySelector withArguments: array])
				ifFalse: [ self inform: 'no such selector']
		]
	].

	self selectMessageAndEvaluate: [:selector |
		array at: 1 put: selector.
		queryPerformer perform: querySelector withArguments: array
	]! !

!CodeWindow methodsFor: 'misc' stamp: 'jmv 12/27/2024 08:50:01'!
selectMessageAndEvaluate: aBlock
	"Allow the user to choose one selector, chosen from the currently selected message's selector, as well as those of all messages sent by it, and evaluate aBlock on behalf of chosen selector.  If there is only one possible choice, simply make it; if there are multiple choices, put up a menu, and evaluate aBlock on behalf of the the chosen selector, doing nothing if the user declines to choose any"

	| selector method messages |
	(selector := model selectedMessageName) ifNil: [^ self].
	method := (model selectedClassOrMetaClass ifNil: [^ aBlock value: selector])
		compiledMethodAt: selector
		ifAbsent: nil.
	(method isNil or: [(messages := method messages) isEmpty])
		 ifTrue: [^ aBlock value: selector].
	(messages size = 1 and: [messages includes: selector])
		ifTrue:
			[^ aBlock value: selector].  "If only one item, there is no choice"

	Smalltalk
		showMenuOf: messages
		withFirstItem: selector
		ifChosenDo: aBlock! !

