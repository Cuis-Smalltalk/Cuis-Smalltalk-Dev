'From Cuis7.5 [latest update: #7719] on 26 November 2025 at 5:00:06 pm'!

!Utilities class methodsFor: 'identification' stamp: 'jmv 26/Nov/2025 16:59:31'!
changeStamp
	"Answer a string to be pasted into source code to mark who changed it and when."
	^ self authorInitials , ' ' , (Date today printFormat: #(1 2 3 $/ 2 1)), ' ',
		(String streamContents: [:s | Time now print24: true showSeconds: true on: s]).! !

