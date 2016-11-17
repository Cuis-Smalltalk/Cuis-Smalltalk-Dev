'From Cuis 5.0 of 7 November 2016 [latest update: #2979] on 17 November 2016 at 10:51:20 am'!

!StdIOWriteStream methodsFor: 'character writing' stamp: 'jmv 11/17/2016 10:48:18'!
newLine
	"Append a newLine character to the receiver.
	The Cuis convention is to use lf on output."

	self nextPut: Character newLineCharacter! !


!StdIOWriteStream reorganize!
('open / close' openOnHandle:name:)
('streaming' flush nextPut:)
('printing' printOn:)
('primitives' primFlush: primWrite:from:startingAt:count:)
('character writing' newLine)
!

