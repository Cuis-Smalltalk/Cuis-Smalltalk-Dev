'From Cuis7.1 [latest update: #6380] on 20 May 2024 at 11:07:10 am'!

!ProtocolBrowser methodsFor: 'private' stamp: 'bp 5/20/2024 11:05:55'!
hierarchyForClass: aClass
	"Set the class hierarchy for the list pane"
	| tab |

	selectiveClassList := OrderedCollection new.
	tab := ''.
	aClass withAllSuperclasses reverse do: [:ea | 
		selectiveClassList add: tab , ea name.
		tab := tab , '  '].
! !

