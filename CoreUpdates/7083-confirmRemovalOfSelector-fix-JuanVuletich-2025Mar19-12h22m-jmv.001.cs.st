'From Cuis7.3 [latest update: #7081] on 19 March 2025 at 12:22:50 pm'!

!SystemDictionary methodsFor: 'ui' stamp: 'jmv 3/19/2025 11:53:47'!
confirmRemovalOf: aSelector on: aClass andSubclasses: aBoolean
	"Determine if it is okay to remove the given selector. Answer 1 if it  
	should be removed, 2 if it should be removed followed by a senders  
	browse, and 3 if it should not be removed."
	| count aMenu labels answer caption allCalls |
	allCalls := self allCallsOn: aSelector.
	(count := allCalls size) = 0
		ifTrue: [^ 1].
	"no senders -- let the removal happen without warning"
	count = 1
		ifTrue: [(allCalls first actualClass == aClass
					and: [allCalls first methodSymbol == aSelector])
				ifTrue: [^ 1]].
	"only sender is itself"
	labels := aBoolean
		ifTrue: [
'Remove from ', aClass name, ' and subclasses
Remove them, then browse senders
Don''t remove, but show me those senders
Forget it -- do nothing -- sorry I asked']
		ifFalse: [
'Remove it
Remove, then browse senders
Don''t remove, but show me those senders
Forget it -- do nothing -- sorry I asked'].
	aMenu := PopUpMenu labels: labels
		icons: #(acceptIcon acceptIcon cancelIcon cancelIcon).

	caption := 'This message has ' , count printString , ' sender'.
	count > 1
		ifTrue: [caption := caption copyWith: $s].
	answer := aMenu startUpWithCaption: caption.
	answer = 3
		ifTrue: [
			self
				browseMessageList: allCalls
				name: 'Senders of ' , aSelector
				autoHighlight: aSelector asString
				allOccurrences: false ].
	answer = 0
		ifTrue: [answer := 3].
	"If user didn't answer, treat it as cancel"
	^ answer min: 3! !

