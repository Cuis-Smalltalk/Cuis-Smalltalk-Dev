'From Cuis 6.0 [latest update: #5220] on 6 June 2022 at 10:03:28 am'!

!Utf8String methodsFor: 'comparing' stamp: 'jmv 6/6/2022 09:50:43'!
commonPartWith: aString startAt: startIndex stopAt: stopIndexRequested applying: aBlock
	"Return the size of the longest common subsequence with aString, only between startIndex and stopIndex.
	Apply aBlock to each character before comparing.
	Do a character-by-character comparison between the receiver and aString.  Return the index of the final character that matched exactly."
	| stopIndex |
	stopIndex _ stopIndexRequested min: aString size.
	startIndex
		to: stopIndex
		do: [ :i |
			(aBlock value: (self at: i)) = (aBlock value: (aString at: i)) ifFalse: [ ^ i - 1 ]].
	^ stopIndex! !


!Trie methodsFor: 'enumerating' stamp: 'jmv 6/6/2022 08:59:31'!
keysDo: aBlock

	self keysAndValuesDo: [ :k :v | aBlock value: k ]! !

!Trie methodsFor: 'enumerating' stamp: 'jmv 6/6/2022 08:59:37'!
valuesDo: aBlock

	self keysAndValuesDo: [ :k :v | aBlock value: v ]! !


!TrieNode methodsFor: 'private' stamp: 'jmv 6/6/2022 09:33:06'!
childFor1: aString characterIndex: i orAdd: doAdd
	"Answer child at childCode.
	If it is not there,  and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it.
		otherwise, just answer nil.
	This method for the case where we currently have exactly one child."
	| nextSegmentStart nextSegmentFirst answer |

	nextSegmentStart _ i + segmentSize.
	nextSegmentFirst _ nextSegmentStart > aString size
		ifTrue: [TrieNode characterForLeaf]
		ifFalse: [(aString at: nextSegmentStart) asLowercase asUnaccented].

	childrenFirstChars = nextSegmentFirst ifTrue: [ ^ children].

	doAdd ifFalse: [ ^ nil ].
	
	answer _ nextSegmentStart > aString size
		ifTrue: [ TrieLeaf new ]
		ifFalse: [ TrieNode someKey: aString segmentStart: nextSegmentStart ].

	"Just one child, if adding, convert references to a collection"
	nextSegmentFirst < childrenFirstChars
		ifTrue: [
			children _ {answer . children }.
			childrenFirstChars _ Array with: nextSegmentFirst with: childrenFirstChars ]
		ifFalse: [
			children _ {children . answer}.
			childrenFirstChars _ Array with: childrenFirstChars with: nextSegmentFirst ].
	^answer! !

!TrieNode methodsFor: 'private' stamp: 'jmv 6/6/2022 09:32:06'!
childFor: aString characterIndex: i orAdd: doAdd
	"Answer child at childCode.
	If it is not there,  and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it.
		otherwise, just answer nil."

	"No children yet"
	childrenFirstChars ifNil: [
		doAdd ifTrue: [
			children _ TrieLeaf new.
			childrenFirstChars _ TrieNode characterForLeaf ].
		^children ].

	"Just one child, if adding, convert references to a collection"
	childrenFirstChars isCollection ifFalse: [
		^ self childFor1: aString characterIndex: i orAdd: doAdd ].

	"Already more than one child. If adding, convert to a bigger collection."
	^self childForN: aString characterIndex: i orAdd: doAdd! !

!TrieNode methodsFor: 'private' stamp: 'jmv 6/6/2022 09:30:11'!
childForN: aString characterIndex: i orAdd: doAdd
	"Answer child at childCode.
	If it is not there,  and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it.
		otherwise, just answer nil.
	This method for the case where we currently have more than one child."
	| answer newChildren newFirstChars nextSegmentFirst nextSegmentStart s |

	nextSegmentStart _ i + segmentSize.
	nextSegmentFirst _ nextSegmentStart > aString size
		ifTrue: [TrieNode characterForLeaf]
		ifFalse: [(aString at: nextSegmentStart) asLowercase asUnaccented].

	childrenFirstChars
		findBinaryIndex: [ :each |
			each = nextSegmentFirst ifTrue: [ 0 ]
				ifFalse: [each > nextSegmentFirst ifTrue: [-1] ifFalse: [1]]]
		do: [ :found | ^ children at: found ]
		ifNone: [ :a :b |
			"Already more than one child. If adding, convert to a bigger collection."
			doAdd
				ifFalse: [ ^nil]
				ifTrue: [
					 answer _ nextSegmentStart > aString size
						ifTrue: [ TrieLeaf new ]
						ifFalse: [  TrieNode someKey: aString segmentStart: nextSegmentStart ].
					s _ children size + 1.
					newChildren _ Array new: s.
					newChildren
						replaceFrom: 1 to: a with: children startingAt: 1;
						at: a+1 put: answer;
						replaceFrom: a+2 to: s with: children startingAt: a+1.
					newFirstChars _ Array new: s.
					newFirstChars
						replaceFrom: 1 to: a with: childrenFirstChars startingAt: 1;
						at: a+1 put: nextSegmentFirst;
						replaceFrom: a+2 to: s with: childrenFirstChars startingAt: a+1.
					children _ newChildren.
					childrenFirstChars _ newFirstChars.
					^answer ]]! !

!TrieNode methodsFor: 'private' stamp: 'jmv 6/6/2022 09:32:10'!
removeChildFor: aString characterIndex: i
	"Answer child at childCode.
	If it is not there,  and aBlockOrNil notNil, evaluate it to create it anew, add it, and answer it.
		otherwise, just answer nil."
	| nextSegmentStart nextSegmentFirst |

	nextSegmentStart _ i + segmentSize.
	nextSegmentFirst _ nextSegmentStart > aString size
		ifTrue: [TrieNode characterForLeaf]
		ifFalse: [(aString at: nextSegmentStart) asLowercase asUnaccented].

	"Just one child, if adding, convert references to a collection"
	childrenFirstChars isCollection ifFalse: [
		childrenFirstChars = nextSegmentFirst
			ifTrue: [ childrenFirstChars _ nil. children _ nil ].
		^self ].
	
	"Already more than one child."
	childrenFirstChars
		findBinaryIndex: [ :each |
			each = nextSegmentFirst ifTrue: [ 0 ]
				ifFalse: [each > nextSegmentFirst ifTrue: [-1] ifFalse: [1]]]
		do: [ :found |
			childrenFirstChars size = 2
				ifTrue: [		"Two children: remove the arrays"
					childrenFirstChars _ childrenFirstChars at: 3-found.
					children _ children at: 3-found ]
				ifFalse: [	"More than two children. condense the arrays"
					childrenFirstChars _ (childrenFirstChars copyFrom: 1 to: found-1), 
						(childrenFirstChars copyFrom: found+1 to: childrenFirstChars size).
					children _ (children copyFrom: 1 to: found-1), (children copyFrom: found+1 to: children size) ]]
		ifNone: [].! !

!methodRemoval: TrieNode #usedMemory stamp: 'jmv 6/6/2022 10:00:42'!
TrieNode removeSelector: #usedMemory!
!methodRemoval: TrieLeaf #usedMemory stamp: 'jmv 6/6/2022 10:01:00'!
TrieLeaf removeSelector: #usedMemory!
!methodRemoval: Trie #usedMemory stamp: 'jmv 6/6/2022 10:00:38'!
Trie removeSelector: #usedMemory!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
SmalltalkCompleter initialize!

