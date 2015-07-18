'From Cuis 4.2 of 25 July 2013 [latest update: #2415] on 11 July 2015 at 4:28:01.134618 pm'!

!TimeProfileBrowser methodsFor: 'private' stamp: 'jmv 7/11/2015 16:25'!
initializeMessageList: anArray talliesList: anotherArray

	messageList _ anArray.
	talliesList _ anotherArray.
	selectedMessage _ talliesList isEmpty ifFalse: [ talliesList first ]! !


!TimeProfileBrowser methodsFor: 'private' stamp: 'jmv 7/11/2015 16:26'!
runBlock: aBlock
"
	TimeProfileBrowser spyOn:  [20 timesRepeat: 
			[Transcript show: 100 factorial printString]]
"
	| result linesStream talliesStream textStream |

self assert: AndreasSystemProfiler canWork.

	tally := AndreasSystemProfiler new.
	tally observedProcess: Processor activeProcess.
	result := tally spyOn: aBlock.

	textStream _ DummyStream on: nil.
	linesStream _ WriteStream on: Array new.
	talliesStream _ WriteStream on: Array new.
	
	tally reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream.
	self initializeMessageList: linesStream contents talliesList: talliesStream contents.

	self changed: #messageList.
	self changed: #messageListIndex.
	self changed: #decorateButtons.
	^result! !


!LargePositiveInteger methodsFor: 'printing' stamp: 'nice 6/17/2011 04:12'!
printOn: aStream base: b
	"Append a representation of this number in base b on aStream.
	In order to reduce cost of LargePositiveInteger ops, split the number in approximately two equal parts in number of digits."
	
	| halfDigits halfPower head tail nDigitsUnderestimate |
	"Don't engage any arithmetic if not normalized"
	(self digitLength = 0 or: [(self digitAt: self digitLength) = 0]) ifTrue: [^self normalize printOn: aStream base: b].
	
	nDigitsUnderestimate := b = 10
		ifTrue: [((self highBit - 1) * 1233 >> 12) + 1. "This is because (2 log)/(10 log)*4096 is slightly greater than 1233"]
		ifFalse: [self highBit quo: b highBit].
		
	"splitting digits with a whole power of two is more efficient"
	halfDigits := 1 bitShift: nDigitsUnderestimate highBit - 2.
	
	halfDigits <= 1
		ifTrue: ["Hmmm, this could happen only in case of a huge base b... Let lower level fail"
			^self printOn: aStream base: b nDigits: (self numberOfDigitsInBase: b)].
	
	"Separate in two halves, head and tail"
	halfPower := b raisedToInteger: halfDigits.
	head := self quo: halfPower.
	tail := self - (head * halfPower).
	
	"print head"
	head printOn: aStream base: b.
	
	"print tail without the overhead to count the digits"
	tail printOn: aStream base: b nDigits: halfDigits! !

