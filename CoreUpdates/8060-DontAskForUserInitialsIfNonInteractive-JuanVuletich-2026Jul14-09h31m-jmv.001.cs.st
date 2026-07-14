'From Cuis7.9 [latest update: #8059] on 14 July 2026 at 9:49:14 am'!

!Utilities class methodsFor: 'identification' stamp: 'jmv 14/Jul/2026, 12:41:37 pm (UTC)'!
changeStampOrUnknown
	"Answer a string to be pasted into source code to mark who changed it and when.
	Don't ask for user initials. If unknown, just say that.
	
	Utilities changeStampOrUnknown
	'unknown 14/Jul/2026, 12:40:54 pm (UTC)' .
	'jmv 16/Jun/2026, 9:01:18 pm (UTC)' .
	"
	"^ self authorInitials , ' ' , (Date today printFormat: #(1 2 3 $/ 2 1)), ' ',
		(String streamContents: [:s | Time now print24: true showSeconds: true on: s]).
		"
	^String streamContents: [ :strm |
		strm nextPutAll: (self authorInitialsPerSe ifNil: ['unknown']).
		strm space.
		DateAndTime nowUTC printNiceOn: strm ]! !


!ClassDescription methodsFor: 'compiling' stamp: 'jmv 14/Jul/2026, 12:48:35 pm (UTC)'!
compile: text classified: category notifying: requestor
	| stamp |
	stamp := self acceptsLoggingOfCompilation
		ifTrue: [
			requestor
				ifNil: [ Utilities changeStampOrUnknown ]
				ifNotNil:[ Utilities changeStamp ]].
	^ self compile: text classified: category
		withStamp: stamp notifying: requestor

 ! !

!ClassDescription methodsFor: 'fileIn/Out' stamp: 'jmv 14/Jul/2026, 12:46:29 pm (UTC)'!
reformatMethodAt: selector
	| newCodeString method | 
	newCodeString := self compilerClass new
		format: (self sourceCodeAt: selector)
		in: self
		notifying: nil.
	method := self compiledMethodAt: selector.
	method
		putSource: newCodeString
		fromParseNode: nil
		class: self
		category: (self organization categoryOfElement: selector)
		withStamp: Utilities changeStampOrUnknown
		libraryName: (method propertyValueAt: #libraryName)
		inFile: 2
		priorMethod: method.! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 14/Jul/2026, 12:45:49 pm (UTC)'!
changeStampField
	
	^' stamp: ', self changeStampOrUnknown printString.! !


!ClassChangeRecord methodsFor: 'all changes' stamp: 'jmv 14/Jul/2026, 12:45:26 pm (UTC)'!
noteChangeType: changeSymbol fromClass: class

	stamp := Utilities changeStampOrUnknown.
	
	changeSymbol == #movedToOtherPackage ifTrue: [
		changeTypes removeAll.
		^ changeTypes add: changeSymbol].
	"Any other change type meanse we're still here!!"
	changeTypes remove: #movedToOtherPackage ifAbsent: nil.

	(changeSymbol == #new or: [changeSymbol == #add]) ifTrue: [
		changeTypes add: #add.
		changeTypes remove: #change ifAbsent: nil.
		^ self].
	changeSymbol == #change ifTrue: [
		(changeTypes includes: #add) ifTrue: [^ self].
		^ changeTypes add: changeSymbol].
	changeSymbol == #addedThenRemoved ifTrue: [
		^ self].  "An entire class was added but then removed"
	changeSymbol == #comment ifTrue: [
		^ changeTypes add: changeSymbol].
	changeSymbol == #reorganize ifTrue: [
		^ changeTypes add: changeSymbol].
	changeSymbol == #rename ifTrue: [
		^ changeTypes add: changeSymbol].
	(changeSymbol beginsWith: 'oldName: ') ifTrue: [
		"Must only be used when assimilating other changeSets"
		(changeTypes includes: #add) ifTrue: [^ self].
		priorName := changeSymbol copyFrom: 'oldName: ' size + 1 to: changeSymbol size.
		^ changeTypes add: #rename].
	changeSymbol == #remove ifTrue: [
		(changeTypes includes: #add)
			ifTrue: [changeTypes add: #addedThenRemoved]
			ifFalse: [changeTypes add: #remove].
		^ changeTypes removeAllFoundIn: #(add change comment reorganize)].

	self error: 'Unrecognized changeType'! !


!MethodChangeRecord methodsFor: 'change type' stamp: 'jmv 14/Jul/2026, 12:45:33 pm (UTC)'!
noteChangeType: newChangeType

	stamp := Utilities changeStampOrUnknown.
	
	"Change of an added method, is still an add"
	(changeType == #add and: [ newChangeType == #change ])
		ifTrue: [ ^self ].

	"Change of an added method, is still an add"
	(changeType == #addedThenRemoved and: [ newChangeType == #change ])
		ifTrue: [ 
			changeType := #add.
			^self ].

	changeType := newChangeType.! !

