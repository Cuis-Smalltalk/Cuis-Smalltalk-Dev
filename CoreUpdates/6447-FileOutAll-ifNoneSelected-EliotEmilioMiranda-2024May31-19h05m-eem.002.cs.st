'From Cuis7.1 [latest update: #6426] on 4 June 2024 at 4:26:27 pm'!

!CodeProvider methodsFor: 'message list' stamp: 'eem 5/31/2024 17:20:01'!
messageList
	"Answer the list of messages being displayed by the receiver, if any.
	 By default answer nil. Subclasses that present a list of messages override."

	^nil! !


!CodeProvider methodsFor: 'message list menu' stamp: 'eem 5/31/2024 17:23:31'!
fileOutMessage
	"Put a description of the selected message on a file"

	self selectedMessageName ifNotNil:
		[^self selectedClassOrMetaClass fileOutMethod: self selectedMessageName].
	self messageList ifNotNil:
		[:theMethods|
		self request: 'Enter file name'
			initialAnswer: 'methods.st'
			do: [ :filename |
				DirectoryEntry fileOutsDirectory // filename writeStreamDo:
					[ :stream |
					stream timeStamp.
					theMethods do:
						[ :methodRef |
						methodRef actualClass
							printMethodChunk: methodRef methodSymbol
							withPreamble: true
							on: stream
							moveSource: false
							toFile: 0 ]]]]! !

