Class {
	#name : 'TrieTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-System-Text',
	#package : 'BaseImageTests',
	#tag : 'System-Text'
}

{ #category : 'aux' }
TrieTest >> assert: subject isEquivalentToDictionary: controlGroup [
	"Test #size, #do:, "
	| count subjectAsDictionary someRandomString |
	self assert: subject isEmpty = controlGroup isEmpty.
	self assert: subject size = controlGroup size.
	controlGroup keysDo: [ :each |
		self assert: (subject includesKey: each) ].
	subject keysDo: [ :each |
		self assert: (controlGroup includesKey: each) ].
	controlGroup keysAndValuesDo: [ :k :v |
		self assert: (subject at: k) = v ].
	subject keysAndValuesDo: [ :k :v |
		self assert: (controlGroup at: k) = v ].
	someRandomString _ 'someRandomStringZZZ'.
	self assert: (subject includesKey: someRandomString)
		= (controlGroup includesKey: someRandomString).
	subjectAsDictionary _ Dictionary new.
	count _ 0.
	subject keysAndValuesDo: [ :k :v |
		subjectAsDictionary at: k put: v.
		count _ count + 1 ].
	self assert: subjectAsDictionary size = controlGroup size.
	self assert: count = controlGroup size.
	self assert: subjectAsDictionary = controlGroup.
]

{ #category : 'aux' }
TrieTest >> assert: subject isEquivalentToSet: controlGroup [
	"Test #size, #do:, "
	| count subjectAsSet someRandomString |
	self assert: subject isEmpty = controlGroup isEmpty.
	self assert: subject size = controlGroup size.
	controlGroup do: [ :each |
		self assert: (subject includes: each) ].
	someRandomString _ 'someRandomStringZZZ'.
	self assert: (subject includes: someRandomString)
		= (controlGroup includes: someRandomString).
	subjectAsSet _ Set new.
	count _ 0.
	subject do: [ :element |
		subjectAsSet add: element.
		count _ count + 1 ].
	self assert: subjectAsSet size = controlGroup size.
	self assert: count = controlGroup size.
	self assert: subjectAsSet = controlGroup.
]

{ #category : 'aux' }
TrieTest >> assert: subject prefixSelection: aString isEquivalentToSet: controlGroup [
	| selection1 selection2 |
	selection1 _ OrderedCollection new.
	subject forPrefix: aString keysAndValuesDo: [ :k :v |
		selection1 add: k ].
	selection2 _ (controlGroup select: [ :each |
		aString isEmpty or: [
			each asLowercase asUnaccented beginsWith: aString asLowercase asUnaccented]]) asOrderedCollection 
				sort: [ :a :b | a asLowercase asUnaccented < b asLowercase asUnaccented ].
	self assert: selection1 = selection2.
]

{ #category : 'testing' }
TrieTest >> testAllMesssagesLikeDicionary [
	"
	TrieTest new testAllMesssagesLikeDicionary
	"
	| subject  controlGroup allMessages copy c set1 set2 |
	subject _ Trie new.
	controlGroup _ Dictionary new.
	allMessages _ Smalltalk allImplementedMessages.
	c _ 0.
	3 timesRepeat: [
		allMessages do: [ :symbol |
			subject at: symbol put: c.
			controlGroup at: symbol put: c.
			c _ c + 1 ]].
	self assert: subject isEquivalentToDictionary: controlGroup.

	copy _ Dictionary new.
	set1 _ Set new.
	subject keysAndValuesDo: [ :symbol :number |
		copy at: symbol put: number.
		set1 add: number ].

	set2 _ Set new.
	subject do: [ :number |
		set2 add: number ].

	self assert: subject isEquivalentToDictionary: copy.
	self assert: copy isEquivalentToDictionary: controlGroup.
	self assert: set1 = set2
]

{ #category : 'testing' }
TrieTest >> testAllMesssagesLikeSet [
	"
	TrieTest new testAllMesssagesLikeSet
	"
	| subject  controlGroup allMessages copy|
	subject _ Trie new.
	controlGroup _ Set new.
	allMessages _ Smalltalk allImplementedMessages.
	3 timesRepeat: [
		allMessages do: [ :symbol |
			subject add: symbol.
			controlGroup add: symbol ]].
	self assert: subject isEquivalentToSet: controlGroup.

	copy _ Set new.
	subject do: [ :symbol |
		copy add: symbol ].

	self assert: subject isEquivalentToSet: copy.
	self assert: copy isEquivalentToSet: controlGroup.
]

{ #category : 'testing' }
TrieTest >> testIncludesReturnsFalseForObjectsThatAreNotString [
	
	self deny: ((Trie with: 'hello') includes: 1)
	
]

{ #category : 'testing' }
TrieTest >> testIncludesReturnsFalseForStringsNotIncluded [
	
	self deny: ((Trie with: 'hello') includes: 'bye')
	
]

{ #category : 'testing' }
TrieTest >> testIncludesReturnsTrueForIncludedStrings [
	
	self assert: ((Trie with: 'hello') includes: 'hello')
	
]

{ #category : 'testing' }
TrieTest >> testKeyNotFoundSignalTheRightMessage [

	self 
		should: [ Trie new errorKeyNotFound: 'theKey' ]
		raise: Error
		withMessageText: 'key: ''theKey'' not found'
]

{ #category : 'testing' }
TrieTest >> testPrefixIteration [
	"
	TrieTest new testPrefixIteration
	"
	| subject  controlGroup |
	subject _ Trie new.
	controlGroup _ Set new.

	self assert: subject prefixSelection: '' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'c' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ca' isEquivalentToSet: controlGroup.

	subject add: 'car'.
	controlGroup add: 'car'.
	subject add: 'car'.
	controlGroup add: 'car'.
	subject add: 'cat'.
	controlGroup add: 'cat'.
	subject add: 'cart'.
	controlGroup add: 'cart'.

	self assert: subject prefixSelection: '' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'c' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ca' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'car' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'cat' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'cart' isEquivalentToSet: controlGroup.

	subject add: 'ñandú'.
	controlGroup add: 'ñandú'.
	subject add: 'ñandues'.
	controlGroup add: 'ñandues'.
	subject add: 'ÑANDÚSES'.
	controlGroup add: 'ÑANDÚSES'.
	
	self assert: subject prefixSelection: '' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'c' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ca' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'car' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'cat' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'cart' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ñ' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ñan' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ñandu' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ñandú' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'Ñ' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'ÑANDÚ' isEquivalentToSet: controlGroup.
]

{ #category : 'testing' }
TrieTest >> testPrefixIteration2 [
	"
	TrieTest new testPrefixIteration2
	"
	| subject  controlGroup allMsg toAvoid |
	subject _ Trie new.
	controlGroup _ Set new.

	allMsg _ Smalltalk allImplementedMessages.
	toAvoid _ (allMsg asArray collect: [ :each | each asLowercase asUnaccented ]) asBag.
	allMsg do: [ :symbol |
		(toAvoid occurrencesOf: symbol) = 1 ifTrue: [
			subject add: symbol.
			controlGroup add: symbol ]].
	
	self assert: subject prefixSelection: '' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'a' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:p' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:pu' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:put' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:put:' isEquivalentToSet: controlGroup.
	self assert: subject prefixSelection: 'at:put:nonExistant:' isEquivalentToSet: controlGroup.
]

{ #category : 'testing' }
TrieTest >> testRemoveLikeDicionary [
	"
	TrieTest new testRemoveLikeDicionary
	"
	| subject  controlGroup allMessages copy c set1 set2 |
	subject _ Trie new.
	controlGroup _ Dictionary new.
	allMessages _ Smalltalk allImplementedMessages.

	subject at: 'doNotRemove' put: 'this'.
	controlGroup at: 'doNotRemove' put: 'this'.
	
	c _ 0.
	3 timesRepeat: [
		allMessages do: [ :symbol |
			subject at: symbol put: c.
			controlGroup at: symbol put: c.
			c _ c + 1 ]].

	subject at: 'doNotRemove2' put: 'this2'.
	controlGroup at: 'doNotRemove2' put: 'this2'.

	self assert: subject isEquivalentToDictionary: controlGroup.
	
	allMessages do: [ :symbol |
		subject removeKey: symbol.
		controlGroup removeKey: symbol ].

	copy _ Dictionary new.
	set1 _ Set new.
	subject keysAndValuesDo: [ :symbol :number |
		copy at: symbol put: number.
		set1 add: number ].

	set2 _ Set new.
	subject do: [ :number |
		set2 add: number ].

	self assert: subject isEquivalentToDictionary: copy.
	self assert: copy isEquivalentToDictionary: controlGroup.
	self assert: set1 = set2.
	self assert: subject isEmpty = controlGroup isEmpty.

	subject removeKey: 'doNotRemove'.
	controlGroup removeKey: 'doNotRemove'.

	self assert: subject isEquivalentToDictionary: controlGroup.
	self deny: subject isEmpty.
	self deny: controlGroup isEmpty.

	subject removeKey: 'doNotRemove2'.
	controlGroup removeKey: 'doNotRemove2'.

	self assert: subject isEquivalentToDictionary: controlGroup.
	self assert: subject isEmpty.
	self assert: controlGroup isEmpty.
]

{ #category : 'testing' }
TrieTest >> testRemoveLikeSet [
	"
	TrieTest new testRemoveLikeSet
	"
	| subject  controlGroup allMessages copy |
	subject _ Trie new.
	controlGroup _ Set new.
	allMessages _ Smalltalk allImplementedMessages.

	subject add: 'doNotRemove'.
	controlGroup add: 'doNotRemove'.
	
	3 timesRepeat: [
		allMessages do: [ :symbol |
			subject add: symbol.
			controlGroup add: symbol ]].

	subject add: 'doNotRemove2'.
	controlGroup add: 'doNotRemove2'.

	self assert: subject isEquivalentToSet: controlGroup.
	
	allMessages do: [ :symbol |
		subject remove: symbol.
		controlGroup remove: symbol ].

	copy _ Set new.
	subject do: [ :symbol |
		copy add: symbol ].

	self assert: subject isEquivalentToSet: copy.
	self assert: copy isEquivalentToSet: controlGroup.

	subject remove: 'doNotRemove'.
	controlGroup remove: 'doNotRemove'.

	self assert: subject isEquivalentToSet: controlGroup.
	self deny: subject isEmpty.
	self deny: controlGroup isEmpty.

	subject remove: 'doNotRemove2'.
	controlGroup remove: 'doNotRemove2'.

	self assert: subject isEquivalentToSet: controlGroup.
	self assert: subject isEmpty.
	self assert: controlGroup isEmpty.
]

{ #category : 'testing' }
TrieTest >> testSetProtocol [
	"
	TrieTest new testSetProtocol
	"
	| subject  controlGroup |
	subject _ Trie new.
	controlGroup _ Set new.
	self assert: subject isEquivalentToSet: controlGroup.
	
	subject add: 'car'.
	controlGroup add: 'car'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'car'.
	controlGroup add: 'car'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'cat'.
	controlGroup add: 'cat'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'cart'.
	controlGroup add: 'cart'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'ñandú'.
	controlGroup add: 'ñandú'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'ñandu'.
	controlGroup add: 'ñandu'.
	self assert: subject isEquivalentToSet: controlGroup.

	subject add: 'ÑANDÚ'.
	controlGroup add: 'ÑANDÚ'.
	self assert: subject isEquivalentToSet: controlGroup.
]
