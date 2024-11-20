'From Cuis7.1 [latest update: #6820] on 19 November 2024 at 5:04:47 pm'!

!CodeWindow methodsFor: 'misc' stamp: 'jfr 8/18/2024 12:17:46'!
sendQuery: querySelector to: queryPerformer with: queryArgs
	"Apply a query to the primary selector associated with the current context.  If no such selection exists, obtain one by user type-in. Then send querySelector to queryPerformer with the selector as its argument."
	|argsBlock|
	argsBlock := [:aSymbol | |strm array|
		strm := WriteStream on: (array := Array new: queryArgs size + 1).
		strm nextPut: aSymbol.
		strm nextPutAll: queryArgs.
		array].

	model selectedMessageName ifNotNil: [:aSymbol| ^ queryPerformer perform: querySelector withArguments: (argsBlock value: aSymbol)].
	self request: 'Type selector:' initialAnswer: 'flag:' verifying: [:aString| aString notEmpty] do: [:aString|
		(Symbol hasInterned: aString ifTrue: [ :aSymbol | queryPerformer perform: querySelector withArguments: (argsBlock value: aSymbol)])
			ifFalse: [self inform: 'no such selector']]! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jfr 8/18/2024 12:20:14'!
browseLocalSenders
	"Present a menu of the currently selected message, as well as all
	messages sent by it.  Open a message set browser of all implementors
	of the message chosen in or below the selected class"

	self sendQuery: #browseAllCallsOn:localTo:
		to: Smalltalk
		with: { model selectedClass } ! !

