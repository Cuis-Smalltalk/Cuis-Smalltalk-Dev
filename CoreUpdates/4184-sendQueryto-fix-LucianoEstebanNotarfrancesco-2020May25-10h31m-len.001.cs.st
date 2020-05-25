'From Cuis 5.0 [latest update: #4183] on 25 May 2020 at 10:31:26 am'!

!CodeWindow methodsFor: 'misc' stamp: 'len 5/25/2020 07:08:44'!
sendQuery: querySelector to: queryPerformer
	"Apply a query to the primary selector associated with the current context.  If no such selection exists, obtain one by user type-in. Then send querySelector to queryPerformer with the selector as its argument."

	model selectedMessageName ifNotNil: [:aSymbol| ^ queryPerformer perform: querySelector with: aSymbol].
	self request: 'Type selector:' initialAnswer: 'flag:' verifying: [:aString| aString notEmpty] do: [:aString|
		(Symbol hasInterned: aString ifTrue: [ :aSymbol | queryPerformer perform: querySelector with: aSymbol])
			ifFalse: [self inform: 'no such selector']]! !

