'From Cuis 4.2 of 25 July 2013 [latest update: #2934] on 9 September 2016 at 10:45:45 pm'!

!String methodsFor: 'copying' stamp: 'KenD 9/4/2016 15:49:17'!
append: aStringOrText

	^ aStringOrText appendToString: self! !

!String methodsFor: 'copying' stamp: 'KenD 9/4/2016 16:21:53'!
appendToString: aString

	| stringSize |
	stringSize := aString size.
	^ aString copyReplaceFrom: stringSize + 1
		 	    to: stringSize
		   	    with: self! !

!String methodsFor: 'copying' stamp: 'KenD 9/4/2016 18:22:51'!
appendToText: aText

	| textStringSize |
	textStringSize := aText string size.
	^ aText replaceFrom: textStringSize + 1
			 to: textStringSize 
			 with: self asText! !


!Text methodsFor: 'copying' stamp: 'KenD 9/4/2016 16:51:44'!
, aStringOrText

	^ aStringOrText appendToText: self! !

!Text methodsFor: 'copying' stamp: 'KenD 9/4/2016 16:41:08'!
appendToString: aString

	| stringSize |
	stringSize := aString size.
	^ (aString asText) 
			replaceFrom: stringSize + 1
			to: stringSize 
			with: self! !

!Text methodsFor: 'copying' stamp: 'KenD 9/4/2016 19:08:07'!
appendToText: aText

	| textSize start stop textResult |
	textSize := aText size.
	start := textSize + 1.
	stop := textSize.
	textResult := Text fromString: aText string , self string.
	textResult privateSetRuns: (aText runs copyReplaceFrom: start to: stop with: self runs).
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	textResult fixParagraphAttributesFrom: start to: start + self size - 1.
	^ textResult
	! !


!String methodsFor: 'copying' stamp: 'KenD 9/4/2016 16:50:54'!
, aStringOrText
	^ aStringOrText appendToString: self! !


!Text methodsFor: 'accessing' stamp: 'KenD 9/5/2016 15:59:40'!
append: stringOrText

	^ stringOrText appendToText: self! !

!Text methodsFor: 'converting' stamp: 'KenD 4/5/2013 14:16'!
replaceFrom: start to: stop withString: replacementString attributes: attributesArray startingAt: repStart 
	"This destructively replaces elements from start to stop in the receiver starting at index, repStart, in replacementCollection. 
	Do it to both the string and the runs.
	The size does not change"

	| newRepRuns |
	string := string replaceFrom: start to: stop with: replacementString startingAt: repStart.
	newRepRuns _ RunArray new: stop-start+1 withAll: attributesArray.
	self privateSetRuns: (runs copyReplaceFrom: start to: stop with: newRepRuns).
	"Ensure the ParagraphAttributes invariant for the interval that could have been affected"
	self fixParagraphAttributesFrom: start to: start + replacementString size - 1! !

