'From Cuis7.9 [latest update: #8210] on 22 September 2026 at 6:28:49 pm'!

!MethodSet methodsFor: 'private' stamp: 'FJG 8/Apr/2026, 3:30:37 am (UTC)'!
labelForSelector: aMethodReference

	"We receive a MethodReference here (instead of a selector) because MethodSet>>#messageList returns an array of method references"

	^ aMethodReference displayStringOrText! !


!MessageNames methodsFor: 'selector list' stamp: 'FJG 6/Apr/2026, 4:43:07 pm (UTC)'!
messageList
	"Answer the receiver's message list, computing it if necessary. The way 
	to force a recomputation is to set the messageList to nil"
	messageList
		ifNil: [
			self initializeMessageList: (
				selectedSelector
					ifNil: [#()]
					ifNotNil: [ Smalltalk allImplementorsOf: selectedSelector]).
			self messageListIndex: (messageList notEmpty
				ifTrue: [1]
				ifFalse: [0])
			].
	^ messageList! !


!MethodSetWindow methodsFor: 'GUI building' stamp: 'FJG 6/Apr/2026, 5:03:19 pm (UTC)'!
buildMorphicMessageList

	^(PluggableListMorph
		withModel: model
		listGetter: #messageItems
		indexGetter: #messageListIndex
		indexSetter: #messageListIndex:)
			menuNshortcutsKey: #messageListMenuSpec;
			doubleClickSelector: #browseMethodFull.! !


'Migrate existing instances of MethodSetWindow'!
MethodSetWindow allSubInstances do: [:methodSetWindow |
	methodSetWindow allSubmorphsDo: [:aMorph |
		aMorph isMemberOf: PluggableListMorph
			:: and: [ (aMorph instVarNamed: 'getListSelector') = #messageList ]
			:: ifTrue: [
				aMorph instVarNamed: 'getListSelector' put: #messageItems
			]
	]
]!

