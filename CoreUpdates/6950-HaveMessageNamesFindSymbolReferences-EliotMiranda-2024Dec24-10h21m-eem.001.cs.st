'From Cuis7.3 [latest update: #6949] on 24 December 2024 at 10:34:31 am'!

!CodeWindow methodsFor: 'misc' stamp: 'eem 12/24/2024 10:33:48'!
getSelectorAndSendQuery: querySelector to: queryPerformer with: queryArgs
	"Obtain a selector relevant to the current context, and then send the querySelector to the queryPerformer with the selector obtained and queryArgs as its arguments.  If no message is currently selected, then obtain a method name from a user type-in"

	| strm array |
	strm := WriteStream on: (array := Array new: queryArgs size + 1).
	strm nextPut: nil.
	strm nextPutAll: queryArgs.

	model selectedMessageName
		ifNotNil:
			[:messageName|
			querySelector numArgs = 1 ifTrue:
				[Symbol
					hasInterned: messageName
					ifTrue:
						[:messageSymbol|
						^queryPerformer perform: querySelector with: messageSymbol]]]
		ifNil:
			[ | selector |
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
