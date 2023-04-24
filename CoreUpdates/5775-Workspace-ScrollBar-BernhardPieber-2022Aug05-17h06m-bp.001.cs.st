'From Cuis 6.0 [latest update: #5299] on 16 December 2022 at 7:00:06 pm'!

!Workspace methodsFor: 'gui' stamp: 'bp 12/16/2022 18:59:11'!
openLabel: aString 
	"Create a standard system view of the model, me, and open it."
	| window |
	window _ WorkspaceWindow editText: self label: aString wrap: true.
	self changed: #actualContents.
	^window! !

!methodRemoval: SystemWindow class #editWordWrapText:label: stamp: 'bp 12/16/2022 18:58:48'!
SystemWindow class removeSelector: #editWordWrapText:label:!
