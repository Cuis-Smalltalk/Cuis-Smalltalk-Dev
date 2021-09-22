'From Cuis 5.0 [latest update: #4877] on 22 September 2021 at 4:22:12 pm'!

!MessageSetWindow class methodsFor: 'opening' stamp: 'jmv 9/22/2021 16:20:42'!
openMessageListUnsorted: methodReferences label: labelString
	"Open a system view for a MessageSet on messageList.
	Don't sort entries by default."

	| messageSet |

	messageSet _ MessageSet messageList: methodReferences asArray.

	^self open: messageSet label: labelString ! !


!ChangeListWindow methodsFor: 'menu commands' stamp: 'jmv 9/22/2021 16:20:50'!
browseCurrentVersionsOfSelections
	"Opens a message-list browser on the current in-memory versions of all methods that are currently seleted"
	| aList |

	aList _ model currentVersionsOfSelections.

	aList size = 0 ifTrue: [^ self inform: 'no selected methods have in-memory counterparts'].
	MessageSetWindow
		openMessageListUnsorted: aList 
		label: 'Current versions of selected methods in ', model file localName! !

