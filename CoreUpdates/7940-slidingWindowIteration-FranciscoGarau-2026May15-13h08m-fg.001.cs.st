'From Cuis7.7 [latest update: #7938] on 15 May 2026 at 1:09:56 pm'!

!SequenceableCollection methodsFor: 'enum. subcollections' stamp: 'fg 15/May/2026 12:59:58'!
collectSlidingWindow: aBlock
	"
	(#( 3 5 8 13 ) collectSlidingWindow: [:a | a + 1]) = #(4 6 9 14).
	(#( 3 5 8 13 ) collectSlidingWindow: [:a :b | a@b]) = {3@5 . 5@8 . 8@13}.
	(#( 3 5 8 13 ) collectSlidingWindow: [:a :b :c | {a. b. c}]) = {{3. 5. 8}. {5. 8. 13.}}.
	('hello_word'  collectSlidingWindow: [:a | Character codePoint: a codePoint - 32]) = 'HELLO?WORD'.
	((1 to: 7) collectSlidingWindow: [:a :b :c :d | a + b + c + d ]) = #(10 14 18 22).
	"
	|  windowSize newSize result  slidingWindow |
	windowSize := aBlock numArgs.
	newSize := self size - windowSize + 1.
	slidingWindow := Array new: windowSize.
	result := self species new: newSize.
	1 to: newSize do: [ :index |
		slidingWindow replaceFrom: 1 to: windowSize with: self startingAt: index.
		result at: index put: (aBlock valueWithArguments: slidingWindow) ].
	^result! !

!SequenceableCollection methodsFor: 'enum. subcollections' stamp: 'fg 15/May/2026 12:49:19'!
slidingWindowDo: aBlock
	"Evaluate a block with my elements taken as a 'sliding window',
	i.e. do an evaluation for each of my elements and the following ones,
	but only up to where enough elements are left.
	(1 to: 11) asArray slidingWindowDo: [ :a :b :c | { a. b. c} print ].
	(1 to: 11) asArray slidingWindowDo: [ :a :b | { a. b} print ].
	(1 to: 11) asArray slidingWindowDo: [ :a | { a} print ].
	"

	|  windowSize slidingWindow |
	windowSize := aBlock numArgs. 
	slidingWindow := Array new: windowSize.
	1 to: self size - windowSize + 1 do: [ :index |
		slidingWindow replaceFrom: 1 to: windowSize with: self startingAt: index.
		aBlock valueWithArguments: slidingWindow ].! !

