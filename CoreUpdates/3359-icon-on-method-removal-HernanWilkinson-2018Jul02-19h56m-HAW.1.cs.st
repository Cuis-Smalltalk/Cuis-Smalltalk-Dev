'From Cuis 5.0 of 7 November 2016 [latest update: #3348] on 2 July 2018 at 8:04:58 pm'!

!SystemDictionary methodsFor: 'ui' stamp: 'HAW 7/2/2018 20:04:12'!
confirmRemovalOf: aSelector on: aClass 
	"Determine if it is okay to remove the given selector. Answer 1 if it  
	should be removed, 2 if it should be removed followed by a senders  
	browse, and 3 if it should not be removed."
	| count aMenu answer caption allCalls |
	allCalls _ self allCallsOn: aSelector.
	(count _ allCalls size) = 0
		ifTrue: [^ 1].
	"no senders -- let the removal happen without warning"
	count = 1
		ifTrue: [(allCalls first actualClass == aClass
					and: [allCalls first methodSymbol == aSelector])
				ifTrue: [^ 1]].
	"only sender is itself"
	aMenu _ PopUpMenu labels: 'Remove it
Remove, then browse senders
Don''t remove, but show me those senders
Forget it -- do nothing -- sorry I asked'
		icons: #(acceptIcon acceptIcon cancelIcon cancelIcon).

	caption _ 'This message has ' , count printString , ' sender'.
	count > 1
		ifTrue: [caption _ caption copyWith: $s].
	answer _ aMenu startUpWithCaption: caption.
	answer = 3
		ifTrue: [self
				browseMessageList: allCalls
				name: 'Senders of ' , aSelector
				autoSelect: aSelector keywords first].
	answer = 0
		ifTrue: [answer _ 3].
	"If user didn't answer, treat it as cancel"
	^ answer min: 3! !

