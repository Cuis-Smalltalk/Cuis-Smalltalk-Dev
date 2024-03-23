'From Cuis6.3 [latest update: #6292] on 23 March 2024 at 4:49:37 pm'!

!ProtocolBrowser methodsFor: 'private' stamp: 'jmv 3/23/2024 16:46:19'!
hierarchyForClass: aClass
	"Set the class hierarchy for the list pane"
	| tab |

	selectiveClassList := OrderedCollection new.
	tab := ''.
	aClass withAllSuperclasses reverse do: [:ea | 
		selectiveClassList add: tab , ea name.
		tab := tab , '  '].
	selectedMessage := nil.! !

!ProtocolBrowser methodsFor: 'private' stamp: 'jmv 3/23/2024 16:42:17'!
protocolFor: anIndex
	"Change the listed protocol"

	exclude := OrderedCollection new.
	anIndex > 0
		ifTrue: [
			selectedName := (selectiveClassList at: anIndex) withBlanksTrimmed.
			(1 to: anIndex - 1) do: [:ix |
				exclude addLast: (selectiveClassList at: ix) withBlanksTrimmed]]
		ifFalse: [
			selectedName := nil.
			].
	self on: baseClass.
	self changed: #messageList.
	self changed: #relabel.
	self acceptedContentsChanged! !

