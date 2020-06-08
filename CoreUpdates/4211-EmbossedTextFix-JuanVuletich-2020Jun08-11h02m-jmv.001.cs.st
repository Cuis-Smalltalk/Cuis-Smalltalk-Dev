'From Cuis 5.0 [latest update: #4208] on 8 June 2020 at 11:06:28 am'!

!BitBltCanvas methodsFor: 'drawing-text' stamp: 'jmv 6/8/2020 11:05:56'!
drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint font: fontOrNil color: aColor

	^super drawStringEmbossed: aString from: firstIndex to: lastIndex at: aPoint x @ (aPoint y // 2 * 2) font: fontOrNil color: aColor! !

